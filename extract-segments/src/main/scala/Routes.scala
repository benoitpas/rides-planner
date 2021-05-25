
case class Point(lat: Double, lon: Double)

object Point:
  def fromXML(xml: scala.xml.Node) =
    val lat = (xml \@ "lat").toDouble
    val lon = (xml \@ "lon").toDouble
    Point(lat, lon)

case class Bounds(minlat: Double,minlon: Double,maxlat: Double,maxlon: Double):
  def area = (maxlon - minlon) * (maxlat-minlat)
  def overlap(that: Bounds) = 
    val (iMinLat,iMaxLat) = (math.max(this.minlat, that.minlat), math.min(this.maxlat, that.maxlat))
    val (iMinLon,iMaxLon) = (math.max(this.minlon, that.minlon), math.min(this.maxlon, that.maxlon))
    val w = if (iMaxLon > iMinLon) then iMaxLon - iMinLon else 0
    val h = if (iMaxLat > iMinLat) then iMaxLat - iMinLat else 0
    (w * h) / (this.area + that.area) * 2

object Bounds:
  def fromXML(xml: scala.xml.Node) =
    val minlat = (xml \@ "minlat").toDouble
    val minlon = (xml \@ "minlon").toDouble
    val maxlat = (xml \@ "maxlat").toDouble
    val maxlon = (xml \@ "maxlon").toDouble
    Bounds(minlat, minlon, maxlat, maxlon)

case class Segment(bounds: Bounds, points: List[Point])

object Segment:
  def fromFile(filename: String) =
    val doc = scala.xml.XML.loadFile(filename)
    val bounds = Bounds.fromXML((doc \\ "bounds")(0))
    val points = for {
      point  <- doc \\ "trkpt"
    } yield {
      Point.fromXML(point)
    }
    Segment(bounds,points.toList)

  def findCoef(p1: Point, p2: Point)  = 
    val rate = (p1.lat - p2.lat) / (p1.lon - p2.lon)
    val lat0 = ((p1.lat + p2.lat) - rate * (p1.lon + p2.lon)) / 2
    (lat0, rate)

  def intersect(s1:(Point,Point), s2:(Point,Point)) = 
    val (lat01, rate1) = findCoef(s1._1, s1._2)
    val (lat02, rate2) = findCoef(s2._1, s2._2)
    val sign1 = Math.signum(- s1._1.lat + rate2 * s1._1.lon + lat02)
    val sign2 = Math.signum(- s1._2.lat + rate2 * s1._2.lon + lat02)
    if ( sign1 * sign2 < 0) {
      val iLon = (lat01 - lat02) / (rate2 - rate1)
      val iLat = ((rate1+rate2) * iLon + lat01 + lat02) / 2
      Some(Point(iLat,iLon))
    } else {
      None
    }
  
  def nonOverlapping(points:List[Point]) = {
    def next(points:List[Point]): List[List[Point]]=
      points match {
        case Nil => List(Nil)
        case p::Nil => List(p::Nil)
        case p1::p2::Nil => List(p2::p1::Nil)
        case p1::p2::rest => {
          val seg0 = (p1,p2)
          val otherSegs = p2::rest.init zip rest
          otherSegs.foldLeft(List(List(p2,p1)))((a,seg) => a match {
            case first::Nil => Segment.intersect(seg0,seg) match {
                case None => List(seg._2::first)
                case Some(iPoint) => List(seg._2::iPoint::Nil,iPoint::(first.init ++ List(iPoint)) ,iPoint::p1::Nil)
            }
            case first::rest => (seg._2::first)::rest
          })
        }
      }

     next(points)/* match {
      case List(l) => {
        val n2 = next(l.tail)
        (n2.init ++ (l.head::n2.last))
      }
      case r => {
        r
      }
    }*/
  }
