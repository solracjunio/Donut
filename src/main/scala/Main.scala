import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.layout.Pane
import scalafx.scene.text.Text
import scalafx.animation.AnimationTimer

object WindowConfig {
  private val _screenTitle = "Demo"
  private val _screenWidth = 1920
  private val _screenHeight = 1080
  private val _screenColor: Color = Color.Black

  //val getScreenTitle: () => String = () => _screenTitle
  def getScreenTitle: String = _screenTitle
  def getScreenWidth: Int = _screenWidth
  def getScreenHeight: Int = _screenHeight
  def getScreenColor: Color = _screenColor
}

object GridConfig {
  private val _cellSize = 20
  private val _gridSize = 50

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

  def draw(cellSize: Int, pane: Pane, rotationAngles: (Double, Double, Double)): Unit = {
    val rotatedVertices = RotationMatrices.applyRotation(vertices, rotationAngles)
    val projectedVertices = rotatedVertices.map(v => applyProjection(v))
    //val projectedVertices = vertices.map(v => applyProjection(v))

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

object RotationMatrices {
  def rotationMatrixX(angle: Double): Array[Array[Double]] = {
    val cosA = math.cos(angle)
    val sinA = math.sin(angle)
    Array(
      Array(1.0, 0.0, 0.0),
      Array(0.0, cosA, -sinA),
      Array(0.0, sinA, cosA)
    )
  }

  def rotationMatrixY(angle: Double): Array[Array[Double]] = {
    val cosB = math.cos(angle)
    val sinB = math.sin(angle)
    Array(
      Array(cosB, 0.0, sinB),
      Array(0.0, 1.0, 0.0),
      Array(-sinB, 0.0, cosB)
    )
  }

  def rotationMatrixZ(angle: Double): Array[Array[Double]] = {
    val cosC = math.cos(angle)
    val sinC = math.sin(angle)
    Array(
      Array(cosC, -sinC, 0.0),
      Array(sinC, cosC, 0.0),
      Array(0.0, 0.0, 1.0)
    )
  }

  def multiplyMatrices(a: Array[Array[Double]], b: Array[Array[Double]]): Array[Array[Double]] = {
    val aRows = a.length
    val aCols = a(0).length
    val bCols = b(0).length
    val result = Array.ofDim[Double](aRows, bCols)

    for (i <- 0 until aRows) {
      for (j <- 0 until bCols) {
        result(i)(j) = (0 until aCols).map(k => a(i)(k) * b(k)(j)).sum
      }
    }

    result
  }

  def applyRotation(vertices: Array[(Double, Double, Double)], rotationAngles: (Double, Double, Double)): Array[(Double, Double, Double)] = {
    val (alpha, beta, gamma) = rotationAngles
    val Rx = rotationMatrixX(alpha)
    val Ry = rotationMatrixY(beta)
    val Rz = rotationMatrixZ(gamma)
    val R = multiplyMatrices(Rz, multiplyMatrices(Ry, Rx))

    vertices.map { case (x, y, z) =>
      val resultX =  R(0)(0) * x + R(0)(1) * y + R(0)(2) * z
      val resultY =  R(1)(0) * x + R(1)(1) * y + R(1)(2) * z
      val resultZ =  R(2)(0) * x + R(2)(1) * y + R(2)(2) * z
      (resultX, resultY, resultZ)
    }
  }
}

object ProjectionMatrix {
  // Nova matriz de projeção ortográfica simples
  val matrix: Array[Array[Double]] = Array(
    Array(1.0, 0.0, 0.0),
    Array(0.0, 1.0, 0.0),
    Array(0.0, 0.0, 0.0)
  )

  def multiplyVectorByMatrix(vector: (Double, Double, Double)): (Double, Double) = {
    val (x, y, z) = vector
    val resultX = (matrix(0)(0) * x) + (matrix(0)(1) * y) + (matrix(0)(2) * z)
    val resultY = (matrix(1)(0) * x) + (matrix(1)(1) * y) + (matrix(1)(2) * z)
    // Como a matriz é simples, a coordenada Z não é usada
    (resultX, resultY)
  }
}

object Window extends JFXApp3 {

  var rotationAngles = (0.0, 0.0, 0.0)
  val rotationSpeed = 0.005

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = WindowConfig.getScreenTitle

      val gridPane: Pane = Grid.createGrid(GridConfig.getCellSize, GridConfig.getGridSize, GridConfig.getGridSize)

      val timer = AnimationTimer { _ =>
        gridPane.children.clear()

        rotationAngles = (
          rotationAngles._1 + rotationSpeed,
          rotationAngles._2 + rotationSpeed,
          rotationAngles._3 + rotationSpeed
        )

        CubeDrawer.draw(GridConfig.getCellSize, gridPane, rotationAngles)

        CharacterDraw.draw(GridConfig.getCellSize, gridPane, '*', GridConfig.getGridMiddle, GridConfig.getGridMiddle)
      }

      timer.start()

      scene = new Scene(WindowConfig.getScreenWidth, WindowConfig.getScreenHeight) {
        fill = WindowConfig.getScreenColor
        content = gridPane
      }
    }
  }
}