
case class Point(lat: Double, lon: Double)

object Point:
  def fromXML(xml: scala.xml.Node) =
    val lat = (xml \@ "lat").toDouble
    val lon = (xml \@ "lon").toDouble
    Point(lat, lon)

case class Bound(minlat: Double,minlon: Double,maxlat: Double,maxlon: Double):
  def area = (maxlon - minlon) * (maxlat-minlat)
  def overlap(that:Bound) = 
    val (iMinLat,iMaxLat) = if (this.minlat < that.maxlat || that.minlat < this.maxlat) 
      (math.max(this.minlat, that.minlat), math.min(this.maxlat, that.maxlat))
    else 
      (0.0,0.0)

    val (iMinLon,iMaxLon) = if (this.minlon < that.maxlon || that.minlon < this.maxlon) 
      (math.max(this.minlon, that.minlon), math.min(this.maxlon, that.maxlon))
    else 
      (0.0, 0.0)
    val w = iMaxLon - iMinLon
    val h = iMaxLat - iMinLat
    (w * h) / (this.area + that.area) * 2

object Bound:
  def fromXML(xml: scala.xml.Node) =
    val minlat = (xml \@ "minlat").toDouble
    val minlon = (xml \@ "minlon").toDouble
    val maxlat = (xml \@ "maxlat").toDouble
    val maxlon = (xml \@ "maxlon").toDouble
    Bound(minlat, minlon, maxlat, maxlon)
