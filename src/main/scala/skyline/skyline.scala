/**
 * The square class is used to represent a square that is part of the
 * skyline to be traced out.  All of the squares are "buildings" along
 * the ground (y=0) so all we really need to know is where the building
 * starts (startX), where it ends (endX), and how tall it is (height).
 */
class square(val startX: Int, val height: Int, val endX: Int) {

 /**
  * Method that is called on a square to indicate that it is currently 
  * "on" and possibly contributing to the skyline.
  */
  def on(s: skyline) = { s.push(this) }

 /**
  * Method that is called on a square to indicate that it is currently
  * "off" and no longer a part of the skyline.
  */
  def off(s: skyline) = { s.pop(this) }
}

/**
 * The skyline class does the work of actually accumulating the points
 * that make up the skyline.  Buildings are pushed and popped from the
 * skyline, and each time this happens new corners are added to the 
 * skyline if the building being added or removed is tall enough.
 */
class skyline
{
  import scala.collection.immutable.ListSet
  var points = List[Int]()
  var currentSquares = ListSet[square]()
  def currentY = currentSquares.foldLeft(0)(_ max _.height)

  def push(s: square) = {
    if(s.height > currentY) {
      points = points :+ s.startX :+ s.height
    }
    currentSquares = currentSquares + s
  }

  def pop(s: square) = { 
    currentSquares = currentSquares - s
    if(s.height > currentY) {
      points = points :+ s.endX :+ currentY
    }
  }
}

class driver {
  val squares = List(new square(1,11,5), new square(2,6,7), new square(3,13,9), 
                     new square(12,7,16), new square(14,3,25), new square(19,18,22), 
                     new square(23,13,29), new square(24,4,28))

  // start with the list of squares...
  val result = squares.
    // From this list create a list of the triggers for pushing
    // and popping buildings from the skyline.  Each element of this is
    // and Pair containing the X-coordinate of the event and the 
    // function should be called when we are at this coordinate.
    foldLeft(List[(Int, skyline => Unit)]())((r,c) => { r :+ (c.startX, c.on _) :+ (c.endX, c.off _) } ).
    // Sort the list of pairs by the X-coordinate so that we
    // are able to iterate left to right over the building edges
    sortWith(_._1 < _._1).
    // using foldLeft(), send a new skyline instance into the 
    // ordered list of edges, to turn the buildings on and off
    // and create the skyline.
    foldLeft(new skyline())((r,c) => { c._2(r); r } )
}
