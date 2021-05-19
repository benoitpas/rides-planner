
class Point(lat:Double, long:Double)

object Point:
  def fromXML(xml:scala.xml.Node) = {
    val lat = (xml \@ "lat").toDouble
    val long = (xml \@ "lon").toDouble
    Point(lat, long)
  }


def main(args:Array[String]): Unit = 
  if (args.length != 1) {
    println("The command takes one argument, the path of a gpx file.")
  } else {
    val doc = scala.xml.XML.loadFile(args(0))
    val points = for {
      point  <- doc \\ "trkpt"
    } yield {
      Point.fromXML(point)
    }
    println(points.length)
  }
