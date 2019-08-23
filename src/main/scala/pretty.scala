import model._
import effects._

import cats._
import cats.implicits._

object pretty {
  def formatHashtag(hashtag: Hashtag): String =
    s"#$hashtag"

  def formatPost(post: PublishedPost): String = {
    val productText = post.post.productId.map(p => s" featuring $p").getOrElse("")
    val hashtagText = formatHashtagList(post.post.hashtags)
    val date = new java.util.Date(post.post.timestamp)
    val dateText = new java.text.SimpleDateFormat("hh:mm d-MMM-YYYY").format(date)
    s"Update from ${post.post.brandId} at $dateText$productText: ${post.post.body.text} $hashtagText"
  }

  def formatUserList(list: List[UserId]): String =
    list.mkString(", ")

  def formatBrandList(list: List[BrandId]): String =
    list.mkString(", ")

  def formatProductList(list: List[ProductId]): String =
    list.mkString(", ")

  def formatHashtagList(list: List[Hashtag]): String =
    list.map(formatHashtag).mkString(" ")

  def formatPostList(list: List[PublishedPost]): String =
    list.map(formatPost).mkString("\n")

  def formatMap[F[_]: Applicative, T](map: Map[String, T], f: T => F[String]): F[String] =
    for {
      entries <- map.toList.map(e => f(e._2).map(e._1 -> _)).sequence
      formatted = entries.map(e => s"${e._1}:\n${e._2}")
    } yield formatted.mkString("\n\n")

  def formatPostLists(lists: Map[String, List[PublishedPost]]): String =
    formatMap[Id, List[PublishedPost]](lists, formatPostList)
}
