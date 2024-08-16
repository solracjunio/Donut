import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.layout.Pane
import scalafx.scene.text.Text

object WindowConfig {
  private val _screenTitle = "Demo"
  private val _screenWidth = 800
  private val _screenHeight = 600
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

object Grid {
  def createGrid(cellSize: Int, rows: Int, cols: Int): Pane = new Pane {
      prefWidth = cellSize * cols
      prefHeight = cellSize * rows
    }
}

object CreateText {
  def create(cellSize: Int, fontSize: Int, character: Char, row: Int, col: Int): Text = new Text {
      x = row * cellSize + cellSize / 4
      y = col * cellSize + cellSize / 1.5
      this.text = character.toString
      fill = Color.White
      style = s"-fx-font-size: $fontSize px;"
    }
}

object CharacterDraw {
  def draw(cellSize: Int, pane: Pane, character: Char, row: Int, col: Int): Unit = {
    val fontSize = cellSize
    val text = CreateText.create(cellSize, fontSize, character, row, col)
    pane.children.add(text)
  }
}

class Point(val x: Int, val y: Int)

object DrawLine {
  def draw(cellSize: Int, pane: Pane, character: Char, pointA: Point, pointB: Point): Unit = {
    var deltaX = pointB.x - pointA.x
    var deltaY = pointB.y - pointA.y
    var stepX = if deltaX < 0 then -1 else 1
    var stepY = if deltaY < 0 then -1 else 1

    val absDeltaX = math.abs(deltaX)
    val absDeltaY = math.abs(deltaY)

    var error = absDeltaX - absDeltaY

    var currentX = pointA.x
    var currentY = pointA.y

    while currentX != pointB.x || currentY != pointB.y do
      CharacterDraw.draw(cellSize, pane, character, currentX, currentY)
      val errorMultiplyTwo = error * 2
      if errorMultiplyTwo > -absDeltaY then
        error -= absDeltaY
        currentX += stepX
      end if
      if errorMultiplyTwo < absDeltaX then
        error += absDeltaX
        currentY += stepY
      end if

    CharacterDraw.draw(cellSize, pane, character, pointB.x, pointB.y)
  }
}

object CubeDrawer {
  val cubeSize = GridConfig.getGridMiddle / 2

  val vertices: Array[(Double, Double, Double)] = Array(
    (-1, -1, -1), (1, -1, -1), (-1, 1, -1), (1, 1, -1), //Front
    (-1, -1, 1), (1, -1, 1), (-1, 1, 1), (1, 1, 1) // Back
  ).map { case (x, y, z) =>
    (
      (x * cubeSize) + GridConfig.getGridMiddle,
      (y * cubeSize) + GridConfig.getGridMiddle,
      (z * cubeSize) + GridConfig.getGridMiddle)
  }

  val edges = Array(
    (0, 1), (0, 2), (0, 4),
    (1, 3), (1, 5),
    (2, 3), (2, 6),
    (3, 7),
    (4, 5), (4, 6),
    (5, 7),
    (6, 7)
  )

  def applyProjection(vertex: (Double, Double, Double)): (Double, Double) = {
    ProjectionMatrix.multiplyVectorByMatrix(vertex)
  }

  def draw(cellSize: Int, pane: Pane): Unit = {
    val projectedVertices = vertices.map(v => applyProjection(v))

    val (centerX, centerY) = (
      projectedVertices.map(_._1).sum / projectedVertices.length,
      projectedVertices.map(_._2).sum / projectedVertices.length
    )

    val adjustVertices = projectedVertices.map { case (x,y) =>
      (x - centerX + GridConfig.getGridMiddle, y - centerY + GridConfig.getGridMiddle)
    }


    for ((start, end) <- edges) {
      val (x1, y1) = adjustVertices(start)
      val (x2, y2) = adjustVertices(end)

      DrawLine.draw(cellSize, pane, '@', new Point(x1.toInt, y1.toInt), new Point(x2.toInt,y2.toInt))
    }
  }
}

object ProjectionMatrix {
  val near = 0.1
  val far = 1000
  val fov = math.Pi / 4
  val aspect = 1.0
  val matrix: Array[Array[Double]] = Array(
    Array(1.0 / (aspect * math.tan(fov / 2)), 0, 0, 0),
    Array(0, 1.0 / math.tan(fov / 2), 0, 0),
    Array(0, 0, -(far + near) / (far - near), -1),
    Array(0, 0, -2 * far * near / (far - near), 0)
  )

  def multiplyVectorByMatrix(vector: (Double, Double, Double)) : (Double, Double) = {
    val (x,y,z) = vector
    val w = 1.0
    val resultX = (matrix(0)(0) * x) + (matrix(0)(1) * y) + (matrix(0)(2) * z) + (matrix(0)(3) * w)
    val resultY = (matrix(1)(0) * x) + (matrix(1)(1) * y) + (matrix(1)(2) * z) + (matrix(1)(3) * w)
    val resultW = (matrix(3)(0) * x) + (matrix(3)(1) * y) + (matrix(3)(2) * z) + (matrix(3)(3) * w)

    val projectedX = resultX / resultW
    val projectedY = resultY / resultW

    (projectedX, projectedY)
  }
}


object Window extends JFXApp3 {

  override def start(): Unit = {

    stage = new JFXApp3.PrimaryStage {
      title = WindowConfig.getScreenTitle

      val gridPane: Pane = Grid.createGrid(GridConfig.getCellSize, GridConfig.getGridSize, GridConfig.getGridSize)

      CubeDrawer.draw(GridConfig.getCellSize, gridPane)

      CharacterDraw.draw(GridConfig.getCellSize, gridPane, '*', GridConfig.getGridMiddle, GridConfig.getGridMiddle)

      scene = new Scene(WindowConfig.getScreenWidth, WindowConfig.getScreenHeight) {
        fill = WindowConfig.getScreenColor
        content = gridPane
      }
    }
  }
}