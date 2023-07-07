import scalafx.animation.{AnimationTimer, KeyFrame, Timeline}
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.{ObjectProperty}
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Rectangle2D
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Circle
import scalafx.stage.Screen
import scalafx.util.Duration

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Main extends JFXApp3 {

  // Paramètres du jeu
  val cellSize = 25
  val gridSize = 86
  val gridSizeCellSize = gridSize * cellSize
  val numParticles = 1500
  val particleRadius = 5
  val particleSpeed = 200
  // Générateur de nombres aléatoires
  val random = new Random()

  // Classe représentant une particule
  case class Particle(var position: (Double, Double), var direction: (Double, Double))

  // Création des particules avec des coordonnées et des directions aléatoires
  val particles: List[Particle] = List.fill(numParticles)(Particle(randomCoordinate, randomDirection))

  // Création des cercles représentant les particules
  val particleCircles: List[Circle] = particles.map(createParticleCircle)

  // Crée un cercle pour représenter une particule avec des coordonnées et une couleur aléatoires
  def createParticleCircle(particle: Particle): Circle = {
    new Circle {
      centerX = particle.position._1
      centerY = particle.position._2
      radius = particleRadius
      fill = Color.rgb(random.nextInt(256), random.nextInt(256), random.nextInt(256))
    }
  }

  // Génération aléatoire de coordonnées
  def randomCoordinate: (Double, Double) =
    (random.nextDouble() * gridSizeCellSize, random.nextDouble() * gridSizeCellSize)

  // Génération aléatoire de directions
  def randomDirection: (Double, Double) =
    (random.nextDouble() - 0.5, random.nextDouble() - 0.5)

  // Met à jour la position d'une particule en fonction du temps écoulé et de sa direction
  def updateParticlePosition(particle: Particle, elapsedSeconds: Double): Unit = {
    val (x, y) = particle.position
    val (dx, dy) = particle.direction
    val newX = (x + dx * particleSpeed * elapsedSeconds + gridSizeCellSize) % gridSizeCellSize
    val newY = (y + dy * particleSpeed * elapsedSeconds + gridSizeCellSize) % gridSizeCellSize
    particle.position = (newX, newY)
  }

  // Vérifie si une particule entre en collision avec les limites de la fenêtre et inverse sa direction si nécessaire
  def checkCollisionWithWindowBounds(particle: Particle): Unit = {
    val (x, y) = particle.position
    val outOfBoundsX = x < particleRadius || x > gridSizeCellSize - particleRadius
    val outOfBoundsY = y < particleRadius || y > gridSizeCellSize - particleRadius
    if (outOfBoundsX || outOfBoundsY) {
      particle.direction = (-particle.direction._1, -particle.direction._2)
    }
  }

  // Vérifie si deux particules entrent en collision
  def checkCollision(p1: Particle, p2: Particle): Boolean = {
    val dx = p1.position._1 - p2.position._1
    val dy = p1.position._2 - p2.position._2
    val distanceSquared = dx * dx + dy * dy
    distanceSquared <= (2 * particleRadius) * (2 * particleRadius)
  }

  // Point d'entrée de l'application
  override def start(): Unit = {
    val screenBounds: Rectangle2D = Screen.primary.visualBounds
    val (boardWidth, boardHeight): (Int, Int) = (screenBounds.width.toInt, screenBounds.height.toInt)
    stage = new PrimaryStage {
      title = "Particles"
      width = boardWidth
      height = boardHeight
      scene = new Scene {
        fill = Black
        content = particleCircles

        var lastFrameTime: Long = 0

        val particlesProperty: ObjectProperty[List[Particle]] = ObjectProperty(particles)

        val timeline: Timeline = infiniteTimeline(particlesProperty, boardWidth, boardHeight)

        AnimationTimer { currentTime =>
          val elapsedSeconds = (currentTime - lastFrameTime) / 1e9

          particlesProperty.get().foreach { particle =>
            updateParticlePosition(particle, elapsedSeconds)
            checkCollisionWithWindowBounds(particle)
          }

          particlesProperty.get().zipWithIndex.foreach { case (p1, i) =>
            particlesProperty.get().drop(i + 1).foreach { p2 =>
              if (checkCollision(p1, p2)) {
                p1.direction = (-p1.direction._1, -p1.direction._2)
                p2.direction = (-p2.direction._1, -p2.direction._2)

                // Ajustement des positions pour éviter les chevauchements
                val dx = p1.position._1 - p2.position._1
                val dy = p1.position._2 - p2.position._2
                val distance = math.sqrt(dx * dx + dy * dy)
                val overlap = (2 * particleRadius) - distance
                val adjustX = dx / distance * overlap * 0.5
                val adjustY = dy / distance * overlap * 0.5
                p1.position = (p1.position._1 + adjustX, p1.position._2 + adjustY)
                p2.position = (p2.position._1 - adjustX, p2.position._2 - adjustY)
              }
            }
          }

          for {
            (circle, particle) <- particleCircles zip particlesProperty.get()
          } {
            circle.centerX = particle.position._1
            circle.centerY = particle.position._2
          }

          lastFrameTime = currentTime
        }.start()

        timeline.play()
      }
    }
  }


  def infiniteTimeline(particles: ObjectProperty[List[Particle]], boardWidth: Int, boardHeight: Int): Timeline =
    new Timeline {
      keyFrames = List(KeyFrame(time = Duration(25), onFinished = _ => updateState(particles, boardWidth, boardHeight)))
      cycleCount = Timeline.Indefinite
    }

  def updateState(particles: ObjectProperty[List[Particle]], boardWidth: Int, boardHeight: Int): Unit = {
    val elapsedSeconds = 0.025 // Correspond à une durée de 25 millisecondes (0.025 seconde)

    particles.get().foreach { particle =>
      updateParticlePosition(particle, elapsedSeconds)
      checkCollisionWithWindowBounds(particle)
    }

    particles.get().zipWithIndex.map { case (p1, i) =>
      particles.get().drop(i + 1).foreach { p2 =>
        if (checkCollision(p1, p2)) {
          p1.direction = (-p1.direction._1, -p1.direction._2)
          p2.direction = (-p2.direction._1, -p2.direction._2)

          // Ajustement des positions pour éviter les chevauchements
          val dx = p1.position._1 - p2.position._1
          val dy = p1.position._2 - p2.position._2
          val distance = math.sqrt(dx * dx + dy * dy)
          val overlap = (2 * particleRadius) - distance
          val adjustX = dx / distance * overlap * 0.5
          val adjustY = dy / distance * overlap * 0.5
          p1.position = (p1.position._1 + adjustX, p1.position._2 + adjustY)
          p2.position = (p2.position._1 - adjustX, p2.position._2 - adjustY)
        }
      }
    }

    particles.get().zip(particleCircles).foreach { case (particle, circle) =>
      circle.centerX = particle.position._1
      circle.centerY = particle.position._2
    }
  }
}
