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
  private val _cellSize = 20

  def getCellSize: Int = _cellSize
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

object GridDrawPattern {
  def draw(cellSize: Int, pane: Pane, rows: Int, cols: Int): Unit = {
    for (row <- 0 until rows; col <- 0 until cols) {
      val char = if ((row + col) % 2 == 0) '@' else '*'
      CharacterDraw.draw(cellSize, pane, char, row, col)
    }
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

    while currentX != pointB.x || currentY != pointB.y  do
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

object Window extends JFXApp3 {

  override def start(): Unit = {

    stage = new JFXApp3.PrimaryStage {
      title = WindowConfig.getScreenTitle

      val gridSize = 100
      val gridPane: Pane = Grid.createGrid(GridConfig.getCellSize, gridSize, gridSize)

      val pointA = Point(0,0)
      val pointB = Point(10,0)
      val pointC = Point(10,10)
      val pointD = Point(0,10)

      DrawLine.draw(GridConfig.getCellSize, gridPane, '@', pointA, pointB)
      DrawLine.draw(GridConfig.getCellSize, gridPane, '@', pointB, pointC)
      DrawLine.draw(GridConfig.getCellSize, gridPane, '@', pointC, pointD)
      DrawLine.draw(GridConfig.getCellSize, gridPane, '@', pointD, pointA)


      //GridDrawPattern.draw(GridConfig.getCellSize, gridPane, gridSize, gridSize)
      //CharacterDraw.draw(GridConfig.getCellSize, gridPane, '@', 0, 0)
      //CharacterDraw.draw(GridConfig.getCellSize, gridPane, '*', pointA.getX, pointA.getY)

      scene = new Scene(WindowConfig.getScreenWidth, WindowConfig.getScreenHeight) {
        fill = WindowConfig.getScreenColor
        content = gridPane
      }
    }
  }
}