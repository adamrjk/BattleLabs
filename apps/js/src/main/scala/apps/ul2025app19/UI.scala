package apps
package ul2025app19

import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.WebClientAppInstance
import org.scalajs.dom
import org.scalajs.dom.{KeyCode, html}
import scalatags.JsDom.all.*
import scalatags.JsDom.svgAttrs.{x, y, cx, cy, r, fill, stroke, strokeWidth, viewBox, preserveAspectRatio, transform}
import scalatags.JsDom.svgTags.{circle, rect, svg, image}
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}
import scala.collection.mutable
import ul2025app19.Event.*
import ul2025app19.Area.*
import ul2025app19.EntityView.*
import ul2025app19.Bonus.*
import ul2025app19.Penalty.*



/**
 * The main entry point for the client-side application.
 */
@JSExportTopLevel("ul2025app19")
object UI extends WSClientApp:
  def appId: String = "ul2025app19"
  def uiId: String = "html"

  /**
   * Initializes the client application instance.
   *
   * @param userId The ID of the user.
   * @param sendMessage Function to send messages to the server.
   * @param target The DOM element where the UI will be mounted.
   * @return A new instance of ClientAppInstance
   */
  def init(
      userId: UserId,
      sendMessage: ujson.Value => Unit,
      target: dom.Element
  ): ClientAppInstance =
    require(userId.nonEmpty, "User ID must not be empty")
    require(target != null, "Target element must not be null")
    UIInstance(userId, sendMessage, target)

  val linkTag = dom.document.createElement("link").asInstanceOf[dom.html.Link]
  linkTag.rel = "stylesheet"
  linkTag.href = "/static/ul2025app19/ul2025app19.css"
  dom.document.head.appendChild(linkTag)

/**
 * Represents the current input mode of the client.
 * Used to determine how keyboard events are handled.
 */
enum InputMode:
    case None, Waiting, Playing

  

/**
 * The main UI logic class.
 * Handles keyboard events, state management, and rendering of the game view.
 *
 * @param userId The ID of the current user.
 * @param sendMessage Function to send messages to the server.
 * @param target The DOM element where the UI is rendered.
 */
class UIInstance(
    userId: UserId,
    sendMessage: ujson.Value => Unit,
    target: dom.Element
) extends WebClientAppInstance[Event, View](userId, sendMessage, target):

  require(userId.nonEmpty, "User ID must not be empty")
  override val wire = ul2025app19.Wire


  val bodyElement = dom.document.body.asInstanceOf[dom.html.Body] 
  bodyElement.classList.add("full-screen-app")


  val pressedKeys = mutable.Set[String]()
  var inputMode = InputMode.None

  dom.window.addEventListener(
    "keydown",
    (e: dom.KeyboardEvent) => inputMode match
      case InputMode.Waiting => 
        pressedKeys.clear()
        if e.code == "Space" then sendEvent(Ready)
      case InputMode.Playing =>
        pressedKeys += e.code
        sendEvent(Pressing(pressedKeys.toSet))
      case InputMode.None => 
        pressedKeys.clear()
  )
  dom.window.addEventListener(
    "keyup",
    (e: dom.KeyboardEvent) => inputMode match
      case InputMode.Playing =>
        pressedKeys -= e.code
        sendEvent(Pressing(pressedKeys.toSet))
      case _ => 
        pressedKeys.clear()
  )

  /**
   * Renders the current state of the application based on the provided view.
   * Updates the input mode and body classes accordingly.
   *
   * @param userId The ID of the current user.
   * @param view The current view state from the server.
   * @return The HTML fragment representing the UI.
   */
  override def render(userId: UserId, view: View): Frag =
    require(userId.nonEmpty, "User ID must not be empty")
    inputMode = view match
    case _: View.Waiting => InputMode.Waiting
    case _: View.Playing => InputMode.Playing
    case _               => InputMode.None

    view match
    case _: View.Waiting => bodyElement.classList.remove("hide-banner")
    case _               => bodyElement.classList.add("hide-banner")

    val viewCode = view match
      case _: View.Waiting       => "waiting"
      case _: View.ChoosingPower => "power"
      case _: View.Playing       => "playing"
      case _: View.Results       => "results"

    List("view-waiting", "view-power", "view-playing", "view-results").foreach(cls => bodyElement.classList.remove(cls))
    bodyElement.classList.add(s"view-$viewCode")

    div(
      cls := "game",
      renderArena(userId, view),
      renderGameOverlay(userId, view)
    )
  /**
   * Renders the game arena, including walls and entities.
   *
   * @param userId The ID of the current user.
   * @param view The current view state.
   * @return An SVG fragment representing the arena.
   */
  private def renderArena(userId: UserId, view: View): Frag =
    require(userId.nonEmpty, "User ID must not be empty")
    (view match
      case View.Playing(colors, scores, kills, entities, walls) =>
        svg(height := "100%", width := "100%", preserveAspectRatio := "xMidYMid meet", viewBox := s"0 0 $WIDTH $HEIGHT",
          walls.toSeq.map(wall =>
            wall match
              case r @ Rectangle(h, w, _, behavior) =>
                rect(x := r.leftX, y := r.topY, width := w, height := h, fill := wall.color, strokeWidth := 0)
              case Circle(radius, center, behavior) =>
                circle(cx := center.x, cy := center.y, r := radius, fill := wall.color, strokeWidth := 0)
          ),
          for entity <- entities
          yield entity match
            case MovingEntity(leftX, topY, diameter, sprite) =>
              image(x := leftX, y := topY, width := diameter, height := diameter, href := f"/static/ul2025app19/${sprite}.png")
            case RotatingEntity(leftX, topY, diameter, sprite, angle) =>
              image(x := leftX, y := topY, width := diameter, height := diameter, href := f"/static/ul2025app19/${sprite}.png",
                cls := "rotating", transform := s"rotate(${angle})")
        )
      case _ => frag()
    ) ensuring (_ != null, "Rendered arena fragment cannot be null")


  /**
   * Renders the game overlays such as the lobby, power picking screen, and results.
   *
   * @param userId The ID of the current user.
   * @param view The current view state.
   * @return An HTML fragment representing the overlay.
   */
  private def renderGameOverlay(userId: UserId, view: View): Frag =
    require(userId.nonEmpty, "User ID must not be empty")
    (view match
      case View.Waiting(colors, ready) =>
        //To make a good looking lobby, we split the players into left, right and at the center the main player
        val otherPlayers = colors.keySet.filter(_ != userId).toSeq
        val (leftPlayers, rightPlayers) = otherPlayers.splitAt(otherPlayers.length / 2)
        div(cls := "waiting-view",
          h1(cls := "header", "Waiting for Players..."),
          div(cls := "players",
            div(cls := "side-group left-group",
              for user <- leftPlayers yield renderPlayer(colors, ready, user, false)
            ),
            renderPlayer(colors, ready, userId, true),
            div(cls := "side-group right-group",
              for user <- rightPlayers yield renderPlayer(colors, ready, user, false)
            )
          )
        )
      case View.ChoosingPower(colors, scores, kills, choices, chosen) =>
        div(
          cls := "ability-pick-view",
          h1(cls := "header", "Time to Upgrade. Choose a Power"),
          div(cls := s"power-selection${ if chosen.isDefined then " selection-made" else ""}",
            div(  cls := "power-container",
              for (power, index) <- choices.zipWithIndex
              goodOrBad = power match
                case b: Bonus => "bonus"
                case p: Penalty => "malus"
              yield a( cls := s"power-button ${goodOrBad}${if chosen.contains(index) then " selected" else ""}",
                      onclick := { () => sendEvent(ChosePower(index)) },
                      img(src := f"/static/ul2025app19/power/${power.toString}.png"),
                      p(power.toString()),
                      h2( cls := "power-description", power.description))
            )
          ),
          renderLeaderboard(colors, scores, kills)
        )
      case View.Playing(colors, scores, kills, entities, walls) => renderLeaderboard(colors, scores, kills)
      case View.Results(colors, scores, kills) =>
        val ranking = scores.keys.toSeq
            .map(uid => uid -> (scores(uid), kills.getOrElse(uid, 0)))
            .sortBy { case (_, (rounds, k)) => (-rounds, -k) }
        val userRank = ranking.indexWhere(_._1 == userId) + 1
        div( cls := "results-overlay",
          div( cls := "rank",
            if userRank == 1 then
              img( src := f"/static/ul2025app19/top1.png")
            else
              h1( s"Top $userRank/${scores.values.size}")
          ),
          div(cls := "final-scores",
            for ((user, (score, kills)), index) <- ranking.zipWithIndex
            yield div(cls := "playerFinalStats",
              h1(s"${index + 1}"),
              img(
                cls := "player-sprite-icon",
                src := f"/static/ul2025app19/${colors(user).name}/8.png",
                alt := user
              ),
              h2(f"${user} | $kills Kills | $score pts")
            )
          ),
          div(cls := "button-container",
            a(
              href := "/",
              cls := "home-button",
              "Back to home page"
            ),
            a(
              cls := "new-game-button",
              onclick := { () => sendEvent(Restart) },
              "Play a New Game"
            )
          )
        )
    ) ensuring (_ != null, "Game overlay must not be null")
  /**
   * Renders a single player's card in the waiting/lobby view.
   *
   * @param colors Map of user IDs to their assigned colors.
   * @param ready Map of user IDs to their ready status.
   * @param user The user ID to render.
   * @param isMain Boolean indicating if this is the current user.
   * @return An HTML fragment representing the player card.
   */
  private def renderPlayer(colors: Map[UserId, Color], ready: Map[UserId, Boolean], user: UserId, isMain: Boolean) =
    require(colors.contains(user), s"Color for user $user missing")
    require(ready.contains(user), s"Ready status for user $user missing")
    div(cls := (if isMain then "main-player" else "other-player") + (if ready(user) then " is-ready" else ""),
      img(src := f"/static/ul2025app19/${colors(user).name}/8.png"),
      h1(cls := "player-name", user),
      h2(cls := s"ready-status ${if ready(user) then "status-ready" else "status-not-ready"}",
        if ready(user) then "Ready" else "Not Ready"
      ),
      if isMain then p(cls := "sub-headline", "Press Space to get ready")
      else frag()
    ) ensuring (_ != null, "Player card must not be null")

  /**
   * Renders the leaderboard showing player stats (kills and scores).
   *
   * @param colors Map of user IDs to their assigned colors.
   * @param scores Map of user IDs to their scores.
   * @param kills Map of user IDs to their kill counts.
   * @return An HTML fragment representing the leaderboard.
   */
  private def renderLeaderboard(colors: Map[UserId, Color], scores: Map[UserId, Int], kills: Map[UserId, Int]): Frag =
    require(colors.keys.forall(scores.contains), "All players must have a score")
    require(colors.keys.forall(kills.contains), "All players must have a kill count")
    div(cls := "leaderboard",
      for (userId, color) <- colors.toSeq
      yield div(cls := "playerStats",
        img(
          cls := "player-sprite-icon",
          src := f"/static/ul2025app19/${color.name}/8.png",
          alt := color.name
        ),
        div(
          cls := "player-info",
          div(cls := "player-name", userId),
          div(
            cls := "stats-row",
            span(cls := "stat-item", f"${kills(userId)} Kills"),
            span(cls := "stat-divider", "|"),
            span(cls := "stat-item", f"${scores(userId)} pts")
          )
        )
      )
    ) ensuring (_ != null, "Leaderboard must not be null")

  

