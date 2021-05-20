def loadFile(filename: String) =
  val doc = scala.xml.XML.loadFile(filename)
  val bounds = Bound.fromXML((doc \\ "bounds")(0))
  val points = for {
    point  <- doc \\ "trkpt"
  } yield {
    Point.fromXML(point)
  }
  (bounds,points)

def processFile(filename: String) =
  val (bounds, points) = loadFile(filename)
  println(s"${filename}\t${points.length}\t${bounds.minlat}\t${bounds.minlon}\t${bounds.maxlat}\t${bounds.maxlon}")
 
def main(args:Array[String]): Unit = 
  if (args.length < 1) {
    println("The command takes as arguments the path of a gpx files.")
  } else {
    val routes = args.map(loadFile).toSeq
    val overlap = for {
      r1 <- routes
      r2 <- routes
    } yield (r1._1,r2._1, r1._1.overlap(r2._1))
    println(overlap)
  }
