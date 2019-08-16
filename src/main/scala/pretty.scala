import model._
import effects._

import cats._
import cats.implicits._

object pretty {
  def formatUser(user: User): String =
    user.name

  def formatBrand(brand: Brand): String =
    brand.name

  def formatProduct(product: Product): String =
    product.name

  def formatHashtag(hashtag: Hashtag): String =
    s"#${hashtag.name}"

  def formatPost(post: PublishedPost): NFIO[String] =
    for {
      c <- ask
      brand <- c.brandStore.get(post.post.brand)
      brandText = formatBrand(brand)
      product <- post.post.product.map(c.productStore.get).sequence
      productText = product.map(p => s" featuring ${formatProduct(p)}").getOrElse("")
      hashtagText = formatHashtagList(post.post.hashtags)
      date = new java.util.Date(post.post.timestamp)
      dateText = new java.text.SimpleDateFormat("hh:mm d-MMM-YYYY").format(date)
    } yield s"Update from $brandText at $dateText$productText: ${post.post.content} $hashtagText"

  def formatUserList(list: List[User]): String =
    list.map(formatUser).mkString(", ")

  def formatBrandList(list: List[Brand]): String =
    list.map(formatBrand).mkString(", ")

  def formatProductList(list: List[Product]): String =
    list.map(formatProduct).mkString(", ")

  def formatHashtagList(list: List[Hashtag]): String =
    list.map(formatHashtag).mkString(" ")

  def formatPostList(list: List[PublishedPost]): NFIO[String] =
    for {
      posts <- list.map(formatPost).sequence
    } yield posts.mkString("\n")

  def formatMap[F[_]: Applicative, T](map: Map[String, T], f: T => F[String]): F[String] =
    for {
      entries <- map.toList.map(e => f(e._2).map(e._1 -> _)).sequence
      formatted = entries.map(e => s"${e._1}:\n${e._2}")
    } yield formatted.mkString("\n\n")

  def formatPostLists(lists: Map[String, List[PublishedPost]]): NFIO[String] =
    formatMap[NFIO, List[PublishedPost]](lists, formatPostList)
}
