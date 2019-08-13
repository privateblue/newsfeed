import model._
import db._
import effect._

import io.getstream.client.Client
import io.getstream.core.models._
import io.getstream.core.options._

import cats.effect.IO

import cats.data.EitherT

import scala.collection.JavaConverters._

object feeds {

  val client = Client.builder("zffns4bft8ct", "a3xwb4tf7r2n7hx52xk8fd9buv2zte4acehrtaj8yajx27drkwhyfs2r5jymsgbn").build();

  def brandFeedName(brand: Brand): FeedID =
    new FeedID("brand:" + brand.brandId)

  def hashtagFeedName(hashtag: Hashtag): FeedID =
    new FeedID("hashtag:" + hashtag.name)

  def userFeedName(user: User): FeedID =
    new FeedID("user:" + user.userId)

  def brandFeedOf(post: Post): NFIO[FeedID] =
    BrandStore.get(post.brand).map(brandFeedName)

  def hashtagFeedsOf(post: Post): NFIO[List[FeedID]] =
    pure(post.hashtags.map(hashtagFeedName))

  def add(post: Post): NFIO[Activity] =
    for {
      feed <- brandFeedOf(post)
      brandFeed = client.flatFeed(feed)
      activity <- activityFrom(post)
      result <- lift(brandFeed.addActivity(activity).toIO)
    } yield result

  def activityFrom(post: Post): NFIO[Activity] =
    for {
      // calculate list of hashtag feeds to also add activity to
      feeds <- hashtagFeedsOf(post)
      recipients = feeds.asJava
      // set author, verb, content, postId
      a = Activity.builder()
        .actor(post.author)
        .verb("post")
        .`object`(post.content)
        .foreignID(post.postId)
        .to(recipients)
        // set brand and hashtags as custom fields
        .extraField("brand", post.brand)
        .extraField("hashtags", post.hashtags.map(_.name).asJava)
      // optionally set product (if present) as a custom field
      awp =
        post.product
          .map(pid => a.extraField("product", pid))
          .getOrElse(a)
    } yield awp.build()

  def getBrandFeed(brand: Brand, from: Int, limit: Int): NFIO[List[Post]] = {
    val feed = brandFeedName(brand)
    getFeed(feed, from, limit)
  }

  def getHashtagFeed(hashtag: Hashtag, from: Int, limit: Int): NFIO[List[Post]] = {
    val feed = hashtagFeedName(hashtag)
    getFeed(feed, from, limit)
  }

  def getUserFeed(user: User, from: Int, limit: Int): NFIO[List[Post]] = {
    val feed = userFeedName(user)
    getFeed(feed, from, limit)
  }

  def getFeed(feedId: FeedID, from: Int, limit: Int): NFIO[List[Post]] = {
    val feed = client.flatFeed(feedId)
    for {
      as <- lift(feed.getActivities(new Limit(limit), new Offset(from)).toIO)
    } yield as.asScala.toList.map(postFrom)
  }

  def postFrom(activity: Activity): Post =
    Post(
      postId = activity.getForeignID(),
      content = activity.getObject(),
      author = activity.getActor(),
      brand = activity.getExtra().get("brand").asInstanceOf[String],
      product = Option(activity.getExtra().get("product").asInstanceOf[String]),
      hashtags =
        activity.getExtra().get("hashtags").asInstanceOf[java.util.List[String]]
          .asScala.toList
          .map(Hashtag.apply)
    )

  def followBrand(follower: User, brand: Brand): NFIO[Unit] = {
    val feed = brandFeedName(brand)
    followFeed(follower, feed)
  }

  def followHashtag(follower: User, hashtag: Hashtag): NFIO[Unit] = {
    val feed = hashtagFeedName(hashtag)
    followFeed(follower, feed)
  }

  def followFeed(follower: User, feedId: FeedID): NFIO[Unit] = {
    val timeline = userFeedName(follower)
    val followerTimeline = client.flatFeed(timeline)
    val feed = client.flatFeed(feedId)
    for {
      _ <- lift(followerTimeline.follow(feed).toIO)
    } yield ()
  }

}
