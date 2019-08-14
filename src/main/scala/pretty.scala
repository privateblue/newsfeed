import model._
import effect._

import cats.implicits._

object pretty {
  def formatPost(post: PublishedPost): NFIO[String] =
    for {
      c <- ask
      author <- c.userStore.get(post.post.author)
      brand <- c.brandStore.get(post.post.brand)
      product <- post.post.product.map(c.productStore.get).sequence
      productText = product.map(p => s" featuring ${p.name}").getOrElse("")
      hashtagText = post.post.hashtags.map(t => s"#${t.name}").mkString(" ", " ", " ")
      date = new java.util.Date(post.post.timestamp)
      dateText = new java.text.SimpleDateFormat("hh:mm d-MMM-YYYY").format(date)
    } yield s"Update from ${brand.name} at $dateText$productText: ${post.post.content}$hashtagText"

  def formatPostList(list: List[PublishedPost]): NFIO[String] =
    for {
      posts <- list.map(formatPost).sequence
    } yield posts.mkString("\n")

  def formatPostLists(lists: Map[String, List[PublishedPost]]): NFIO[String] =
    for {
      entries <- lists.toList.map(e => formatPostList(e._2).map(e._1 -> _)).sequence
      formatted = entries.map(e => s"${e._1}:\n${e._2}")
    } yield formatted.mkString("\n\n")
}
