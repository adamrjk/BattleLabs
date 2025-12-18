package apps

import cs214.webapp.server.ApplicationJVM
import io.undertow.Undertow
import io.undertow.server.{HttpHandler, HttpServerExchange}
import io.undertow.util.{Headers, HttpString, Cookies}
import io.undertow.server.handlers.CookieImpl
import io.undertow.server.handlers.proxy.{LoadBalancingProxyClient, ProxyHandler}
import io.undertow.server.handlers.ResponseCodeHandler
import java.net.{URI, ServerSocket}
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*
import java.util.UUID
import scala.util.Using

object GameServer extends ApplicationJVM

object MainJVM:
  // Active games storage
  // Map GameId -> (Port, Process)
  case class GameInstance(id: String, name: String, port: Int, process: Process, createdAt: Long)
  val activeGames = new ConcurrentHashMap[String, GameInstance]()
  
  // Cache of proxy clients per port to avoid recreating them
  val proxyClients = new ConcurrentHashMap[Int, LoadBalancingProxyClient]()

  def main(args: Array[String]): Unit =
    if (args.contains("--internal")) {
      startInternalServer()
    } else {
      startLobbyServer()
    }

  def startInternalServer(): Unit =
    // The library expects WEBAPPLIB_PORT env var or property, we set it in the process builder env
    // But just in case, we can verify or print it
    val port = System.getenv("WEBAPPLIB_PORT")
    println(s"[Internal] Starting Game Server on port $port...")
    GameServer.start()

  def startLobbyServer(): Unit =
    val proxyPort = Option(System.getenv("PORT")).flatMap(_.toIntOption).getOrElse(8080)
    println(s"[Lobby] Starting Lobby Proxy on port $proxyPort...")

    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      println("[Lobby] Shutting down all game processes...")
      activeGames.values().forEach(_.process.destroy())
    }))

    val server = Undertow.builder()
      .addHttpListener(proxyPort, "0.0.0.0")
      .setHandler(new HttpHandler {
        override def handleRequest(exchange: HttpServerExchange): Unit = {
          val path = exchange.getRequestPath
          val queryParams = exchange.getQueryParameters

          // 1. Check for Cookie
          val gameCookie = Option(exchange.getRequestCookie("gameId")).map(_.getValue)
          
          // 2. Check for Query Param (Overrides Cookie for joining/creating)
          val gameIdParam = if (queryParams.containsKey("gameId")) Option(queryParams.get("gameId").peekFirst()) else None

          val targetGameId = gameIdParam.orElse(gameCookie)

          if (path == "/api/create") {
            handleCreateGame(exchange)
          } else if (path == "/api/games") {
            handleListGames(exchange)
          } else if (targetGameId.isDefined && activeGames.containsKey(targetGameId.get)) {
             // We have a target game. Proxy to it.
             val gameId = targetGameId.get
             val gameInstance = activeGames.get(gameId)
             
             // If this request established the gameId via Param, ensure we set the cookie for future requests
             if (gameIdParam.isDefined) {
               exchange.setResponseCookie(new CookieImpl("gameId", gameId).setPath("/"))
             }
             
             proxyToGame(exchange, gameInstance.port)
          } else {
             // No game associated, and not an API call.
             // If path is "/" serve Lobby.
             if (path == "/") {
               serveLobby(exchange)
             } else {
                // If the user requests /static/something but has no cookie and no param,
                // it implies they might be lost or the cookie expired.
                // However, the lobby itself might need static assets?
                // The current lobby is self-contained HTML.
                // So we can safely 404 or redirect to /
                exchange.setStatusCode(404)
                exchange.getResponseSender.send("Not Found / No Game Session")
             }
          }
        }
      })
      .build()

    server.start()
    println(s"[Lobby] Proxy Server started on port $proxyPort")

  def proxyToGame(exchange: HttpServerExchange, port: Int): Unit =
    // Get or Create ProxyClient for this port
    val client = proxyClients.computeIfAbsent(port, p => {
      new LoadBalancingProxyClient()
        .addHost(new URI(s"http://localhost:$p"))
        .setConnectionsPerThread(20)
    })

    // Create a handler for this request
    val handler = ProxyHandler.builder()
      .setProxyClient(client)
      .setNext(ResponseCodeHandler.HANDLE_404)
      .build()

    handler.handleRequest(exchange)

  def handleCreateGame(exchange: HttpServerExchange): Unit =
    if (activeGames.size() >= 5) {
      exchange.setStatusCode(503)
      exchange.getResponseSender.send("""{"error": "Too many active games"}""")
      return
    }

    val id = UUID.randomUUID().toString.substring(0, 8)
    val port = findFreePort()
    
    // Spawn Process
    try {
      val classpath = System.getProperty("java.class.path")
      println(s"[Lobby] Spawning game on port $port with classpath length ${classpath.length}")
      
      val pb = new ProcessBuilder("java", "-Xmx100m", s"-DWEBAPPLIB_PORT=$port", "-cp", classpath, "apps.MainJVM", "--internal")
      val env = pb.environment()
      env.put("WEBAPPLIB_PORT", port.toString)
      env.put("PORT", port.toString) // Some frameworks look for PORT
      pb.inheritIO()
      
      val process = pb.start()
      activeGames.put(id, GameInstance(id, s"Game $id", port, process, System.currentTimeMillis()))
      
      // Wait for server to come online
      var attempts = 0
      var connected = false
      while (attempts < 50 && !connected) { 
        if (!process.isAlive) {
            println(s"[Lobby] Game process died prematurely with exit code ${process.exitValue()}")
            attempts = 100 // Break loop
        } else {
            try {
              Thread.sleep(100)
              new java.net.Socket("localhost", port).close()
              connected = true
            } catch {
              case _: Exception => attempts += 1
            }
        }
      }

      if (!connected) {
         if (process.isAlive) process.destroy()
         activeGames.remove(id)
         exchange.setStatusCode(504)
         exchange.getResponseSender.send("""{"error": "Game server timed out or crashed"}""")
         return
      }
      
      println(s"[Lobby] Game $id started on port $port")
      val json = s"""{"id": "$id"}"""
      exchange.getResponseHeaders.put(Headers.CONTENT_TYPE, "application/json")
      exchange.getResponseSender.send(json)
    } catch {
      case e: Exception =>
        e.printStackTrace()
        exchange.setStatusCode(500)
        exchange.getResponseSender.send("Failed to start game process: " + e.getMessage)
    }

  def handleListGames(exchange: HttpServerExchange): Unit =
    // Filter out dead processes?
    val games = activeGames.values().asScala.toList
      .filter(_.process.isAlive)
      .sortBy(-_.createdAt)
    
    val json = games.map(g => s"""{"id": "${g.id}", "name": "${g.name}"}""").mkString("[", ",", "]")
    exchange.getResponseHeaders.put(Headers.CONTENT_TYPE, "application/json")
    exchange.getResponseSender.send(json)

  def findFreePort(): Int =
    Using(new ServerSocket(0)) { socket =>
      socket.getLocalPort
    }.get

  def serveLobby(exchange: HttpServerExchange): Unit =
    exchange.getResponseHeaders.put(Headers.CONTENT_TYPE, "text/html")
    exchange.getResponseSender.send(lobbyHtml)

  val lobbyHtml = """
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Game Lobby</title>
    <style>
        body { font-family: 'Inter', sans-serif; background-color: #1a1a1a; color: white; display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100vh; margin: 0; }
        h1 { font-size: 3rem; margin-bottom: 2rem; background: linear-gradient(45deg, #ff00cc, #3333ff); -webkit-background-clip: text; -webkit-text-fill-color: transparent; }
        .container { background: #2a2a2a; padding: 2rem; border-radius: 15px; box-shadow: 0 10px 30px rgba(0,0,0,0.5); text-align: center; width: 400px; }
        button { background: #3333ff; color: white; border: none; padding: 15px 30px; font-size: 1.2rem; border-radius: 8px; cursor: pointer; margin: 10px; width: 100%; transition: transform 0.2s, background 0.2s; }
        button:hover { background: #4444ff; transform: scale(1.05); }
        .secondary { background: #444; }
        .secondary:hover { background: #555; }
        #games-list { margin-top: 20px; text-align: left; max-height: 200px; overflow-y: auto; }
        .game-item { background: #333; padding: 10px; margin: 5px 0; border-radius: 5px; cursor: pointer; display: flex; justify-content: space-between; }
        .game-item:hover { background: #444; }
        .hidden { display: none; }
    </style>
</head>
<body>
    <h1>Battle Labs Lobby</h1>
    <div class="container" id="main-menu">
        <button onclick="createGame()">Create Game</button>
        <button class="secondary" onclick="showJoin()">Join Game</button>
    </div>

    <div class="container hidden" id="join-menu">
        <h2>Active Games</h2>
        <div id="games-list">Loading...</div>
        <button class="secondary" onclick="hideJoin()">Back</button>
    </div>

    <script>
        function createGame() {
            fetch('/api/create', { method: 'POST' })
                .then(r => r.json())
                .then(data => {
                    if (data.error) { alert(data.error); return; }
                    window.location.href = '/?gameId=' + data.id;
                })
                .catch(e => alert('Failed to create game'));
        }

        function showJoin() {
            document.getElementById('main-menu').classList.add('hidden');
            document.getElementById('join-menu').classList.remove('hidden');
            loadGames();
        }

        function hideJoin() {
            document.getElementById('join-menu').classList.add('hidden');
            document.getElementById('main-menu').classList.remove('hidden');
        }

        function loadGames() {
            fetch('/api/games')
                .then(r => r.json())
                .then(games => {
                    const list = document.getElementById('games-list');
                    list.innerHTML = '';
                    if (games.length === 0) list.innerHTML = '<p>No active games.</p>';
                    games.forEach(g => {
                        const div = document.createElement('div');
                        div.className = 'game-item';
                        div.innerHTML = `<span>${g.name}</span> <span>Join &rarr;</span>`;
                        div.onclick = () => window.location.href = '/?gameId=' + g.id;
                        list.appendChild(div);
                    });
                });
        }
    </script>
</body>
</html>
"""
