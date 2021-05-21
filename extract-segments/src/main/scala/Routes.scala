
case class Point(lat: Double, lon: Double)

object Point:
  def fromXML(xml: scala.xml.Node) =
    val lat = (xml \@ "lat").toDouble
    val lon = (xml \@ "lon").toDouble
    Point(lat, lon)

case class Bound(minlat: Double,minlon: Double,maxlat: Double,maxlon: Double):
  def area = (maxlon - minlon) * (maxlat-minlat)
  def overlap(that:Bound) = 
    val (iMinLat,iMaxLat) = (math.max(this.minlat, that.minlat), math.min(this.maxlat, that.maxlat))
    val (iMinLon,iMaxLon) = (math.max(this.minlon, that.minlon), math.min(this.maxlon, that.maxlon))
    val w = if (iMaxLon > iMinLon) then iMaxLon - iMinLon else 0
    val h = if (iMaxLat > iMinLat) then iMaxLat - iMinLat else 0
    (w * h) / (this.area + that.area) * 2

object Bound:
  def fromXML(xml: scala.xml.Node) =
    val minlat = (xml \@ "minlat").toDouble
    val minlon = (xml \@ "minlon").toDouble
    val maxlat = (xml \@ "maxlat").toDouble
    val maxlon = (xml \@ "maxlon").toDouble
    Bound(minlat, minlon, maxlat, maxlon)
