import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.layout.Pane
import scalafx.scene.text.Text
import scalafx.scene.shape.Line
import scalafx.animation.AnimationTimer
import scala.math.{cos, sin}

object WindowConfig {
  private val _screenTitle = "Cube3D"
  private val _screenWidth = 1000
  private val _screenHeight = 1000
  private val _screenColor: Color = Color.Black

  //val getScreenTitle: () => String = () => _screenTitle
  def getScreenTitle: String = _screenTitle
  def getScreenWidth: Int = _screenWidth
  def getScreenHeight: Int = _screenHeight
  def getScreenColor: Color = _screenColor
}

object GridConfig {
  private val _cellSize = 10
  private val _gridSize = 100

  def getCellSize: Int = _cellSize
  def getGridSize: Int = _gridSize
  def getGridMiddle: Int = _gridSize / 2
}

def CreateText(cellSize: Int, fontSize: Int, character: Char, row: Int, col: Int): Text = new Text {
  x = row * cellSize + cellSize / 4
  y = col * cellSize + cellSize / 1.5
  this.text = character.toString
  fill = Color.White
  style = s"-fx-font-size: $fontSize px;"
}

def DrawCharacter(cellSize: Int, pane: Pane, character: Char, row: Int, col: Int): Unit = {
  val fontSize = cellSize
  val text = CreateText(cellSize, fontSize, character, row, col)
  pane.children.add(text)
}

class Point2D(val x: Int, val y: Int)

object DrawLine {
  def draw(cellSize: Int, pane: Pane, character: Char, pointA: Point2D, pointB: Point2D): Unit = {
    val deltaX = pointB.x - pointA.x
    val deltaY = pointB.y - pointA.y
    var stepX = if deltaX < 0 then -1 else 1
    var stepY = if deltaY < 0 then -1 else 1

    val absDeltaX = math.abs(deltaX)
    val absDeltaY = math.abs(deltaY)

    var error = absDeltaX - absDeltaY

    var currentX = pointA.x
    var currentY = pointA.y

    while currentX != pointB.x || currentY != pointB.y do
      DrawCharacter(cellSize, pane, character, currentX, currentY)
      val errorMultiplyTwo = error * 2
      if errorMultiplyTwo > -absDeltaY then
        error -= absDeltaY
        currentX += stepX
      end if
      if errorMultiplyTwo < absDeltaX then
        error += absDeltaX
        currentY += stepY
      end if

    DrawCharacter(cellSize, pane, character, pointB.x, pointB.y)
  }
}

object CubeDrawer {
  private val cubeSize = GridConfig.getGridMiddle / 2

  private val vertices: Array[(Double, Double, Double)] = Array(
    (-1, -1, -1), (1, -1, -1), (-1, 1, -1), (1, 1, -1), //Front
    (-1, -1, 1), (1, -1, 1), (-1, 1, 1), (1, 1, 1) // Back
  ).map { case (x, y, z) =>
    (
      x * cubeSize,
      y * cubeSize,
      z * cubeSize
    )
  }

  private val edges = Array(
    (0, 1), (0, 2), (0, 4),
    (1, 3), (1, 5),
    (2, 3), (2, 6),
    (3, 7),
    (4, 5), (4, 6),
    (5, 7),
    (6, 7)
  )

  def draw(cellSize: Int, pane: Pane, rotationAngles: (Double, Double, Double)): Unit = {
    pane.children.clear()
    val rotatedVertices = vertices.map { vertex =>
      val rotated = RotationMatrices.rotateX(rotationAngles._1, vertex)
      val rotated2 = RotationMatrices.rotateY(rotationAngles._2, rotated)
      RotationMatrices.rotateZ(rotationAngles._3, rotated2)
    }

    val projectedVertices = rotatedVertices.map(vertex => ProjectionMatrix.project(vertex, GridConfig.getGridSize, GridConfig.getGridSize))

    edges.foreach { case (start, end) =>
      val (x1, y1) = projectedVertices(start)
      val (x2, y2) = projectedVertices(end)
      DrawLine.draw(cellSize, pane, '@', new Point2D(x1.toInt, y1.toInt), new Point2D(x2.toInt, y2.toInt))
      val line = new Line {
        startX = x1
        startY = y1
        endX = x2
        endY = y2
        stroke = Color.White
      }
      pane.children.add(line)
    }
  }
}

object RotationMatrices {
  def rotateX(angle: Double, point: (Double, Double, Double)): (Double, Double, Double) = {
    val (x, y, z) = point
    val cosA = cos(angle)
    val sinA = sin(angle)
    (
      x,
      y * cosA - z * sinA,
      y * sinA + z * cosA
    )
  }

  def rotateY(angle: Double, point: (Double, Double, Double)): (Double, Double, Double) = {
    val (x, y, z) = point
    val cosB = cos(angle)
    val sinB = sin(angle)
    (
      x * cosB + z * sinB,
      y,
      -x * sinB + z * cosB
    )
  }

  def rotateZ(angle: Double, point: (Double, Double, Double)): (Double, Double, Double) = {
    val (x, y, z) = point
    val cosC = cos(angle)
    val sinC = sin(angle)
    (
      x * cosC - y * sinC,
      x * sinC + y * cosC,
      z
    )
  }
}

object ProjectionMatrix {
  def project(point: (Double, Double, Double), width: Int, height: Int): (Double, Double) = {
    val (x, y, z) = point
    val scale = 100 / (z + 100)
    (
      x * scale + width / 2,
      y * scale + height / 2
    )
  }
}

object Window extends JFXApp3 {
  private var rotationAngles = (0.0, 0.0, 0.0)
  private val rotationSpeed = 0.005

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = WindowConfig.getScreenTitle

      val gridPane: Pane = new Pane {
        prefWidth = WindowConfig.getScreenWidth
        prefHeight = WindowConfig.getScreenHeight
      }

      val timer: AnimationTimer = AnimationTimer { _ =>
        gridPane.children.clear()

        rotationAngles = (
          rotationAngles._1 + rotationSpeed,
          rotationAngles._2 + rotationSpeed,
          rotationAngles._3 + rotationSpeed
        )

        CubeDrawer.draw(GridConfig.getCellSize, gridPane, rotationAngles)

        //DrawCharacter(GridConfig.getCellSize, gridPane, '*', GridConfig.getGridMiddle, GridConfig.getGridMiddle)
      }

      timer.start()

      scene = new Scene(WindowConfig.getScreenWidth, WindowConfig.getScreenHeight) {
        fill = WindowConfig.getScreenColor
        content = gridPane
      }
    }
  }
}
