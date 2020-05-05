package edu.colorado.csci3155.project2

/* A class to maintain a canvas object.
*  A canvas is a collection of circles and polygons.
*/
import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Graphics2D}
/* A figure is a sealed trait. It can be a Polygon or a "MyCircle" */
sealed trait Figure {
    def rotate(angRad: Double): Figure
    def getBoundingBox: (Double, Double, Double, Double)
    def translate(shiftX: Double, shiftY: Double): Figure
    def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit
}

/*
 Class Polygon
   A polygon is defined by a list of its vertices
*/
case class Polygon(val cList: List[(Double, Double)]) extends Figure {
    //TODO: Define the bounding box of the polygon
    //      A bounding box is a 4-tuple (xMin, xMax, yMin, yMax)
    /*
        Its of the form [ xMin, xMax, yMin, yMax]
            member function will return a tuple
        so you need to go through the given coordinates and do the following
        1. find the xMin
        2. find the xMax
        3. find the yMin
        4. find the yMax
        pack these up as a tuple and return it

        need to give it the inital value.
     */
    override def getBoundingBox: (Double, Double, Double, Double ) = {
        // this will find you the minimum x-coordinate
        val minX = cList.foldLeft(cList(0)._1)((minSoFar, curr) => {
            if (curr._1 < minSoFar){
                curr._1
            }
            else{
                minSoFar
            }
        })
        // this will find you the maximum x-coordinate
        val maxX = cList.foldLeft(cList(0)._1)((maxSoFar, curr) => {
            if (curr._1 > maxSoFar){
                curr._1
            }
            else{
                maxSoFar
            }
        })
        // this will find the you minium y-coordinate
        val minY = cList.foldLeft(cList(0)._2)((minSoFar, curr) => {
            if (curr._2 < minSoFar){
                curr._2
            }
            else{
                minSoFar
            }
        })
        // this will find you the maximum y-coordinate
        val maxY = cList.foldLeft(cList(0)._2)((maxSoFar, curr) => {
            if (curr._2 > maxSoFar){
                curr._2
            }
            else{
                maxSoFar
            }
        })
        (minX, maxX, minY, maxY)
    }

    //TODO: Create a new polygon by shifting each vertex in cList by (x,y)
    //    Do not change the order in which the vertices appear
    override def translate(shiftX: Double, shiftY: Double): Polygon = {

        val initialList = List[(Double,Double)]()
        // get the new coordinates into a list then create a new polygon and return it

        // go through all the coordinates of the polygon, shift them then put it into a new list
        val translatedCoordinates = cList.foldLeft(initialList)((acc, curr) => {
            val shiftedX = curr._1 + shiftX
            val shiftedY = curr._2 + shiftY
            acc ++ List((shiftedX, shiftedY))
        })
        val translatedPolygon = new Polygon(translatedCoordinates)
        translatedPolygon
    }

    // goes through the coordinate list and rotates all points into a new list
    override def rotate(theta: Double): Polygon = {
        val initialList = List[(Double,Double)]()
        val rotatedCoordinates = cList.foldLeft(initialList)((acc, curr) => {
            val x = curr._1
            val y = curr._2

            val xPrime = (x * (scala.math.cos(theta))) - (y * (scala.math.sin(theta)))
            val yPrime = (x * (scala.math.sin(theta))) + (y * (scala.math.cos(theta)))
            acc ++ List((xPrime, yPrime))
        })

        val rotatedPolygon = new Polygon(rotatedCoordinates)
        rotatedPolygon
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val xPoints: Array[Int] = new Array[Int](cList.length)
        val yPoints: Array[Int] = new Array[Int](cList.length)
        for (i <- 0 until cList.length){
            xPoints(i) = ((cList(i)._1 + shiftX )* scaleX).toInt
            yPoints(i) = ((cList(i)._2 + shiftY) * scaleY).toInt
        }
        g.drawPolygon(xPoints, yPoints, cList.length)
    }
}

/*
  Class MyCircle
  Define a circle with a given center c and radius r
 */
case class MyCircle(val c: (Double, Double), val r: Double) extends Figure {
    //TODO: Define the bounding box for the circle
    //    A bounding box is a 4-tuple (xMin, xMax, yMin, yMax)
    /*
        A circle coordinates are given as the center points (x1, y1)  then the radius given by one coordinate (r0)
            bounds will be given by
                minX = x1 - r0
                maxX = x1 + r0
                minY = y1 - r0
                maxY = y1 + r0
     */
    override def getBoundingBox: (Double, Double, Double, Double) = {
        val minX = c._1 - r
        val maxX = c._1 + r
        val minY = c._2 - r
        val maxY = c._2 + r
        (minX, maxX, minY, maxY)
    }

    //TODO: Create a new circle by shifting the center
    /*
        translation of a circle is just moving the origin, radius does not change
     */
    override def translate(shiftX: Double, shiftY: Double): MyCircle = {
        val shiftedOrigin  = (c._1 + shiftX, c._2 + shiftY)

        val newCircle = new MyCircle(shiftedOrigin, r)
        newCircle
    }

    // will rotate a circle shape and return a new one
    /*
        rotating a circle is simply
            x_prime = x*cos(theta) - y*sin(theta)
            y_prime = xsin(theta) + ycos(theta)
     */
     override def rotate(theta: Double): MyCircle = {
        val x = c._1
        val y = c._2
        val xPrime = (x * (scala.math.cos(theta)) - (y *  (scala.math.sin(theta))))
        val yPrime = (x*(scala.math.sin(theta))) + (y * (scala.math.cos(theta)))

        val rotatedCircle = new MyCircle((xPrime, yPrime) , r)
        rotatedCircle
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val centerX = ((c._1 + shiftX) * scaleX) .toInt
        val centerY = ((c._2 + shiftY) * scaleY) .toInt
        val radX = (r * scaleX).toInt
        val radY = (r * math.abs(scaleY)).toInt
        //g.draw(new Ellipse2D.Double(centerX, centerY, radX, radY))
        g.drawOval(centerX-radX, centerY-radY, 2*radX, 2*radY)
    }
}
/*
  Class : MyCanvas
  Define a canvas through a list of figure objects. Figure objects can be circles or polygons.
 */
class MyCanvas (val listOfObjects: List[Figure]) {
    // TODO: Write a function to get the boundingbox for the entire canvas.
    //     Hint: use existing getBoundingBox functions defined in each figure.
    //     A bounding box is defined as (xMin, xMax, yMin, yMax)
    //  (xMin, xMax, yMin, yMax)
    def getBoundingBox: (Double, Double, Double, Double) = {
        // going to assume that it is always going to be safe to call this function with a non empty canvas

        val initialCoordinate_minX = listOfObjects(0).getBoundingBox._1
        val minX = listOfObjects.foldLeft(initialCoordinate_minX)((minx,currentPolygon) => {
            val coordinate = currentPolygon.getBoundingBox._1
            if (coordinate < minx){
                coordinate
            }else{
                minx
            }
        })

        val initialCoordinate_maxX = listOfObjects(0).getBoundingBox._2
        val maxX = listOfObjects.foldLeft(initialCoordinate_maxX)((maxX, currentPolygon) => {
            val coordinate = currentPolygon.getBoundingBox._2
            if (coordinate > maxX){
                coordinate
            }else{
                maxX
            }
        })

        val initialCoordinate_minY = listOfObjects(0).getBoundingBox._3
        val minY = listOfObjects.foldLeft(initialCoordinate_minY)((minY, currentPolygon) => {
            val points = currentPolygon.getBoundingBox
            if (points._3 < minY){
                points._3
            }else{
                minY
            }
        })

        val initialCoordinate_maxY = listOfObjects(0).getBoundingBox._4
        val maxY = listOfObjects.foldLeft(initialCoordinate_maxY)((maxY, currentPolygon) => {
            val points = currentPolygon.getBoundingBox
            if (points._4 > maxY){
                points._4
            }else{
                maxY
            }
        })
        (minX, maxX, minY, maxY)
    }

    //TODO: Write a function to translate each figure in the canvas by shiftX, shiftY
    def translate(shiftX: Double, shiftY: Double): MyCanvas = {
        // just have to go through each polygon and shift its translate all of them into a new list of shapes, then return a new canvas

        // create a new List[Figure]
        val initialList = List[Figure]()
        val translatedPolygons = listOfObjects.foldLeft(initialList)((acc, currentPolygon) => {
            acc ++ List(currentPolygon.translate(shiftX, shiftY))
        })
        val newCanvas = new MyCanvas(translatedPolygons)
        newCanvas
    }

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the objects in myc2 to the right of the objects in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeRight(myc2: MyCanvas):MyCanvas = {
        val boundingBox_c1 = this.getBoundingBox
        val boundingBox_c2 = myc2.getBoundingBox

        val yShift = ((boundingBox_c1._4 - boundingBox_c1._3)/2) - ((boundingBox_c2._4 - boundingBox_c2._3) / 2)
        val xShift = boundingBox_c1._2 - boundingBox_c1._1

        val translatedC2 = myc2.translate(xShift,yShift)
        val finalCanvas = this.overlap(translatedC2)
        finalCanvas
    }

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the figures in myc2 on top of the figures in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeTop(myc2: MyCanvas): MyCanvas = {
        // essentially, just translate everything on c2 over everything on c1's coordinates
        // then just create a brand new canvas that contains all of the fresh polygons with their respective coordinates

        // remember, this a tuple in the following form: (xMin, xMax, yMin, yMax)
        val boundingBox_c1 = this.getBoundingBox
        val boundingBox_c2 = myc2.getBoundingBox

        val xShift = ((boundingBox_c1._2 - boundingBox_c1._1)/2) - ((boundingBox_c2._2 - boundingBox_c2._1) / 2)
        val yShift = boundingBox_c1._4 - boundingBox_c1._3

        val translatedC2 = myc2.translate(xShift,yShift)
        val finalCanvas = this.overlap(translatedC2)
        finalCanvas
    }

    //TODO: Write a function that will rotate each figure in the canvas using
    // the angle `ang` defined in radians.
    // Suggestion: first write rotation functions for polygon and circle.
    // rotating a polygon is simply rotating each vertex.
    // rotating a circle is simply rotating the center with radius unchanged.
    def rotate(angRad: Double): MyCanvas = {
        // create a new list of polygons that have been rotated then create a newCanvas full of them and return it
        val initialList = List[Figure]()
        val rotatedPolygons = listOfObjects.foldLeft(initialList)((acc, curr) => {
            val rotatedPolygon = curr.rotate(angRad)
            acc ++ List(rotatedPolygon)
        })
        val newCanvas = new MyCanvas(rotatedPolygons)
        newCanvas
    }
    //Function: overlap
    // This function takes a list of objects from this canvas
    // and contatenates with a list of objects from canvas c2.
    // The result is a new MyCanvas object with the concatenated
    // list of objects.
    def overlap(c2: MyCanvas): MyCanvas = {
        new MyCanvas(listOfObjects ++ c2.listOfObjects)
    }

    // Function to draw the canvas. Do not edit.
    def render(g: Graphics2D, xMax: Double, yMax: Double) = {
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
        val shiftx = -lx1
        val shifty = -uy1
        val scaleX = xMax/(ux1 - lx1  + 1.0)
        val scaleY = yMax/(uy1 - ly1 + 1.0)
        listOfObjects.foreach(f => f.render(g,scaleX, -scaleY, shiftx, shifty))
    }

    // DO NOT EDIT THE CODE BELOW
    override def toString: String = {
        listOfObjects.foldLeft[String] ("") { case (acc, fig) => acc ++ fig.toString }
    }
    // DO NOT EDIT
    def getListOfObjects: List[Figure] = listOfObjects

    // DO NOT EDIT
    def numPolygons: Int =
        listOfObjects.count {
            case Polygon(_) => true
            case _ => false }

    //DO NOT EDIT
    def numCircles: Int = {
        listOfObjects.count {
            case MyCircle(_,_) => true
            case _ => false }
    }
    //DO NOT EDIT
    def numVerticesTotal: Int = {
        listOfObjects.foldLeft[Int](0) ((acc, f) =>
            f match {
                case Polygon(lst1) => acc + lst1.length
                case _ => acc
            }
        )
    }
}
//     