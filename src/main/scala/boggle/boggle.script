class Square(val xCoord: Int, val yCoord: Int)
{
  var letter: Char = randomLetter
  private var adjacentSquares: List[Square] = List()
  var isVisited: Boolean = false

  def randomLetter: Char = {
    import scala.util.Random
    var result = '.'
    do {
      result = new Random().nextPrintableChar()
    } while(!result.isLower)
    result
  }

  def getAdjacentSquares(board: Board) : List[Square] = {
    if(adjacentSquares.size == 0) {
      if(xCoord - 1 >= 1)
    adjacentSquares = adjacentSquares :+ board.getSquare(xCoord - 1, yCoord)
      if(xCoord + 1 <= board.xSize)
    adjacentSquares = adjacentSquares :+ board.getSquare(xCoord + 1, yCoord)
      if(yCoord - 1 >= 1)
    adjacentSquares = adjacentSquares :+ board.getSquare(xCoord, yCoord - 1)
      if(yCoord + 1 <= board.ySize) 
    adjacentSquares = adjacentSquares :+ board.getSquare(xCoord, yCoord + 1)
    }
    adjacentSquares.filter(!_.isVisited);
  }

  def visit(walker: BoardWalker): Unit = {
    isVisited = true
    walker.addLetter(letter)
    getAdjacentSquares(walker.board).map(_.visit(walker));
    walker.removeLetter
    isVisited = false
  }
}

class BoardWalker(val board: Board)
{
  var currentWord = List[Char]()
  var words = List[String]()
  
  def addLetter(letter: Char) = {
    currentWord = currentWord :+ letter
    words = words :+ currentWord.mkString("")
  }

  def removeLetter() = {
    currentWord = currentWord.dropRight(1);
  }
}

class Board(val xSize: Int, val ySize: Int)
{
  private var squares = List[Square]();

  for (j <- 1 to ySize) {
    for (i <- 1 to xSize) {
      squares = squares :+ new Square(i, j);
    } 
  }

  def getSquare(x: Int, y: Int): Square = {
    squares.apply(((y - 1) * xSize) + (x - 1))
  }

  def getWords : List[String] = {
    val walker = new BoardWalker(this)
    squares.map(_.visit(walker))
    walker.words
  }

  override def toString = {
    var result = ""
    for (j <- 1 to ySize) {
      for (i <- 1 to xSize) {
    result += getSquare(i, j).letter + " "
      } 
      result += "\n"
    }
    result
  }
}

val board = new Board(3, 2)
print(board.toString)
print(board.getWords.mkString("\n"))
