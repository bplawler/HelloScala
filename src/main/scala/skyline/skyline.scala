class square(p1: Pair[Int, Int], p2: Pair[Int, Int], 
             p3: Pair[Int, Int], p4: Pair[Int, Int]) {
  val points = List(p1, p2, p3, p4)
  def on(s: skyline) = { s.push(this) }
  def off(s: skyline) = { s.pop(this) }
  def upperLeftY = { points.foldLeft((0,0))( (r,c)=>{
    if(c._1 < r._1 && c._2 > r._2) c else r
  } )._1 }
  def upperRightY = { points.foldLeft((0,0))( (r,c)=>{
    if(c._1 > r._1 && c._2 > r._2) c else r
  } ) }
}

class skyline
{
  import scala.collection.immutable.ListSet
  var points = List[Pair[Int, Int]]()
  var currentSquares = ListSet[square]()
  var currentY = 0

  def push(s: square) = {
    //if [ s.upperLeftY higher than current ] addNewPoints
    currentSquares = currentSquares + s
  }

  def pop(s: square) = { 
    currentSquares = currentSquares - s
    //if [ s.upperRightY higher than current ] addNewPoints
  }
}
