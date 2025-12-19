package apps

import cs214.webapp.server.ApplicationJVM

object MainJVM extends ApplicationJVM:
  this.start()


// import io.undertow.Undertow
// import io.undertow.server.{HttpHandler, HttpServerExchange}
// import io.undertow.util.{Headers, HttpString, Cookies}
// import io.undertow.server.handlers.CookieImpl
// import java.net.{URI, ServerSocket}
// import scala.util.Using
// import sttp.client4._
// import upickle.default._

// // Data models for Fly API
// case class FlyMachineConfig(image: String, guest: Map[String, String] = Map("cpu_kind" -> "shared", "cpus" -> "1", "memory_mb" -> "512"), auto_destroy: Boolean = true)
// case class FlyMachineRequest(name: String, config: FlyMachineConfig)
// implicit val machineConfigRw: ReadWriter[FlyMachineConfig] = macroRW
// implicit val machineRequestRw: ReadWriter[FlyMachineRequest] = macroRW

// object GameServer extends ApplicationJVM

// object MainJVM:
//   def main(args: Array[String]): Unit =
//     val role = sys.env.getOrElse("ROLE", "game")
//     println(s"[Boot] Starting with ROLE=$role")
    
//     if (role == "lobby") {
//       FlyLobby.start()
//     } else {
//       // Game Mode (Default)
//       println("[Game] Starting Game Server...")
//       GameServer.start()
//     }

// object FlyLobby:
//   def start(): Unit =
//     val port = Option(System.getenv("PORT")).flatMap(_.toIntOption).getOrElse(8080)
//     println(s"[Lobby] Starting Fly Lobby on port $port...")

//     val server = Undertow.builder()
//       .addHttpListener(port, "0.0.0.0")
//       .setHandler(new HttpHandler {
//         override def handleRequest(exchange: HttpServerExchange): Unit = {
//           val path = exchange.getRequestPath
//           val queryParams = exchange.getQueryParameters

//           // 1. Check for Cookie or Param
//           val gameCookie = Option(exchange.getRequestCookie("gameId")).map(_.getValue)
//           val gameIdParam = if (queryParams.containsKey("gameId")) Option(queryParams.get("gameId").peekFirst()) else None
//           val targetGameId = gameIdParam.orElse(gameCookie)

//           if (path == "/api/create") {
//             handleCreateGame(exchange)
//           } else if (path == "/api/games") {
//             handleListGames(exchange)
//           } else if (targetGameId.isDefined && targetGameId.get.nonEmpty) {
//              val gameId = targetGameId.get
//              // Replay to the specific machine instance
//              println(s"[Lobby] Replaying to instance: $gameId")
             
//              if (gameIdParam.isDefined) {
//                exchange.setResponseCookie(new CookieImpl("gameId", gameId).setPath("/"))
//              }
             
//              exchange.getResponseHeaders.put(new HttpString("Fly-Replay"), s"instance=$gameId")
//              exchange.setStatusCode(200)
//              exchange.getResponseSender.send("")
//           } else {
//              if (path == "/") {
//                serveLobby(exchange)
//              } else {
//                 exchange.setStatusCode(404)
//                 exchange.getResponseSender.send("Not Found / No Game Session")
//              }
//           }
//         }
//       })
//       .build()

//     server.start()

//   def handleCreateGame(exchange: HttpServerExchange): Unit =
//     // Call Fly API to spawn a machine
//     val appName = sys.env.getOrElse("FLY_APP_NAME", "battlelabs")
//     val apiToken = sys.env.getOrElse("FLY_API_TOKEN", "")
//     val image = sys.env.getOrElse("FLY_IMAGE_REF", "registry.fly.io/battlelabs:latest") // Must be set in deployment
    
//     if (apiToken.isEmpty) {
//         exchange.setStatusCode(500)
//         exchange.getResponseSender.send("""{"error": "Missing FLY_API_TOKEN"}""")
//         return
//     }

//     try {
//         val machineName = "game-" + java.util.UUID.randomUUID().toString.substring(0, 8)
//         val body = FlyMachineRequest(
//             name = machineName, 
//             config = FlyMachineConfig(image = image)
//         )
        
//         val backend = DefaultSyncBackend()
//         val response = basicRequest
//           .post(uri"https://api.machines.dev/v1/apps/$appName/machines")
//           .header("Authorization", s"Bearer $apiToken")
//           .header("Content-Type", "application/json")
//           .body(write(body))
//           .send(backend)

//         if (response.code.isSuccess) {
//             val body = response.body match {
//               case Right(b) => b
//               case Left(b) => b
//             }
//             val json = ujson.read(body)
//             val machineId = json("id").str
            
//             // Wait for it to start? For now, just return the ID.
//             // In a better impl, we'd poll for 'started' state.
            
//             println(s"[Lobby] Created machine $machineId")
            
//             // Redirect to it
//             exchange.setStatusCode(302)
//             exchange.getResponseHeaders.put(Headers.LOCATION, s"/?gameId=$machineId")
//             exchange.endExchange()
//         } else {
//             println(s"[Error] Fly API: ${response.body}")
//             exchange.setStatusCode(500)
//             exchange.getResponseSender.send(s"""{"error": "Failed to create machine: ${response.code}"}""")
//         }
//     } catch {
//         case e: Exception => 
//             e.printStackTrace()
//             exchange.setStatusCode(500)
//             exchange.getResponseSender.send(s"""{"error": "${e.getMessage}"}""")
//     }

//   def handleListGames(exchange: HttpServerExchange): Unit =
//     val appName = sys.env.getOrElse("FLY_APP_NAME", "battlelabs")
//     val apiToken = sys.env.getOrElse("FLY_API_TOKEN", "")
    
//     if (apiToken.isEmpty) {
//         exchange.getResponseSender.send("[]")
//         return
//     }

//     try {
//         val backend = DefaultSyncBackend()
//         val response = basicRequest
//           .get(uri"https://api.machines.dev/v1/apps/$appName/machines")
//           .header("Authorization", s"Bearer $apiToken")
//           .send(backend)
          
//         if (response.code.isSuccess) {
//              val body = response.body match {
//                case Right(b) => b
//                case Left(b) => b
//              }
//              // Filter for only machines that look like games (optional, based on metadata)
//              // For now, return all
//              exchange.getResponseHeaders.put(Headers.CONTENT_TYPE, "application/json")
//              exchange.getResponseSender.send(body)
//         } else {
//              exchange.getResponseSender.send("[]")
//         }
//     } catch {
//         case e: Exception => exchange.getResponseSender.send("[]")
//     }

//   def serveLobby(exchange: HttpServerExchange): Unit =
//     exchange.getResponseHeaders.put(Headers.CONTENT_TYPE, "text/html")
//     exchange.getResponseSender.send(lobbyHtml)

//   val lobbyHtml = """
// <!DOCTYPE html>
// <html lang="en">
// <head>
//     <meta charset="UTF-8">
//     <meta name="viewport" content="width=device-width, initial-scale=1.0">
//     <title>Game Lobby</title>
//     <style>
//         body { font-family: 'Inter', sans-serif; background-color: #1a1a1a; color: white; display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100vh; margin: 0; }
//         h1 { font-size: 3rem; margin-bottom: 2rem; background: linear-gradient(45deg, #ff00cc, #3333ff); -webkit-background-clip: text; -webkit-text-fill-color: transparent; }
//         .container { background: #2a2a2a; padding: 2rem; border-radius: 15px; box-shadow: 0 10px 30px rgba(0,0,0,0.5); text-align: center; width: 400px; }
//         button { background: #3333ff; color: white; border: none; padding: 15px 30px; font-size: 1.2rem; border-radius: 8px; cursor: pointer; margin: 10px; width: 100%; transition: transform 0.2s, background 0.2s; }
//         button:hover { background: #4444ff; transform: scale(1.05); }
//         .secondary { background: #444; }
//         .secondary:hover { background: #555; }
//         #games-list { margin-top: 20px; text-align: left; max-height: 200px; overflow-y: auto; }
//         .game-item { background: #333; padding: 10px; margin: 5px 0; border-radius: 5px; cursor: pointer; display: flex; justify-content: space-between; }
//         .game-item:hover { background: #444; }
//         .hidden { display: none; }
//     </style>
// </head>
// <body>
//     <h1>Battle Labs Lobby</h1>
//     <div class="container" id="main-menu">
//         <form action="/api/create" method="post" onsubmit="document.getElementById('create-btn').innerText = 'Creating...'; document.getElementById('create-btn').disabled = true;">
//             <button id="create-btn" type="submit">Create Game</button>
//         </form>
//         <button class="secondary" onclick="showJoin()">Join Game</button>
//     </div>

//     <div class="container hidden" id="join-menu">
//         <h2>Active Games</h2>
//         <div id="games-list">Loading...</div>
//         <div style="margin-top: 20px; border-top: 1px solid #444; padding-top: 20px; display: flex; justify-content: space-between;">
//             <input type="text" id="game-id-input" placeholder="Enter Game Code" style="padding: 10px; width: 60%; border-radius: 5px; border: none;">
//             <button style="width: 30%; padding: 10px; margin: 0;" onclick="joinByCode()">Go</button>
//         </div>
//         <button class="secondary" onclick="hideJoin()">Back</button>
//     </div>

//     <script>
//         function showJoin() {
//             document.getElementById('main-menu').classList.add('hidden');
//             document.getElementById('join-menu').classList.remove('hidden');
//             loadGames();
//         }

//         function hideJoin() {
//             document.getElementById('join-menu').classList.add('hidden');
//             document.getElementById('main-menu').classList.remove('hidden');
//         }

//         function loadGames() {
//             fetch('/api/games')
//                 .then(r => r.json())
//                 .then(games => {
//                     const list = document.getElementById('games-list');
//                     list.innerHTML = '';
//                     if (games.length === 0) list.innerHTML = '<p>No active games.</p>';
//                     games.forEach(g => {
//                         if(g.state === 'started') {
//                             const div = document.createElement('div');
//                             div.className = 'game-item';
//                             div.innerHTML = `<span>${g.name || g.id}</span> <span>Join &rarr;</span>`;
//                             div.onclick = () => window.location.href = '/?gameId=' + g.id;
//                             list.appendChild(div);
//                         }
//                     });
//                 });
//         }

//         function joinByCode() {
//             const id = document.getElementById('game-id-input').value.trim();
//             if(id) window.location.href = '/?gameId=' + id;
//         }
//     </script>
// </body>
// </html>
// """