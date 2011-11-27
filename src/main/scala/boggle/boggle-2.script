class Square(val xCoord: Int, val yCoord: Int)
{
  var letter: Char = BoggleHelpers.randomLetter
  private var adjacentSquares: List[Square] = List()

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
    adjacentSquares
  }
}

class BoardWalker(val board: Board)
{
  var currentWord = List[Char]()
  var words = List[String]()
  var isVisited = Map[Square, Boolean]();
  
  def addLetter(letter: Char) = {
    currentWord = currentWord :+ letter
    words = words :+ currentWord.mkString("")
  }

  def removeLetter() = {
    currentWord = currentWord.dropRight(1);
  }

  def visit(square: Square): Unit = {
    isVisited += (square -> true)
    addLetter(square.letter)
    square.getAdjacentSquares(board).filter(!isVisited.get(_).getOrElse(false)).map(visit(_));
    removeLetter
    isVisited += (square -> false)
  }
}

class Board(val xSize: Int, val ySize: Int)
{
  var squares = List[Square]();

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
    squares.map(walker.visit(_))
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

object BoggleHelpers {
  def randomLetter: Char = {
    import scala.util.Random
    var result = '.'
    do {
      result = new Random().nextPrintableChar()
    } while(!result.isLower)
    result
  }
}

val board = new Board(3, 2)
print(board.toString)
print(board.getWords.mkString("\n"))
