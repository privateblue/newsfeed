import model._
import db._
import effect._

import io.getstream.core.models._
import io.getstream.core.options._

import cats.effect.IO

import cats.data.Kleisli

import scala.collection.JavaConverters._

object feeds {

  def brandFeedName(brand: Brand): FeedID =
    new FeedID("brand:" + brand.brandId)

  def hashtagFeedName(hashtag: Hashtag): FeedID =
    new FeedID("hashtag:" + hashtag.name)

  def userFeedName(user: User): FeedID =
    new FeedID("user:" + user.userId)

  def brandFeedOf(post: Post): NFIO[FeedID] =
    for {
      c <- ask
      brand <- c.brandStore.get(post.brand)
    } yield brandFeedName(brand)

  def hashtagFeedsOf(post: Post): NFIO[List[FeedID]] =
    pure(post.hashtags.map(hashtagFeedName))

  def add(post: Post): NFIO[PublishedPost] =
    for {
      c <- ask
      feed <- brandFeedOf(post)
      brandFeed = c.streamClient.flatFeed(feed)
      activity <- activityFrom(post)
      result <- liftIO(brandFeed.addActivity(activity).toIO)
    } yield publishedPostFrom(result)

  def activityFrom(post: Post): NFIO[Activity] =
    for {
      // calculate list of hashtag feeds to also add activity to
      feeds <- hashtagFeedsOf(post)
      recipients = feeds.asJava
      // set author, verb, content, postId, time
      a = Activity.builder()
        .actor(post.author)
        .verb("post")
        .`object`(post.content)
        .foreignID(post.postId)
        .time(new java.util.Date(post.timestamp))
        // set hashtags as targets
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

  def getBrandFeed(brand: Brand, from: Int, limit: Int): NFIO[List[PublishedPost]] = {
    val feed = brandFeedName(brand)
    getFeed(feed, from, limit)
  }

  def getHashtagFeed(hashtag: Hashtag, from: Int, limit: Int): NFIO[List[PublishedPost]] = {
    val feed = hashtagFeedName(hashtag)
    getFeed(feed, from, limit)
  }

  def getUserFeed(user: User, from: Int, limit: Int): NFIO[List[PublishedPost]] = {
    val feed = userFeedName(user)
    getFeed(feed, from, limit)
  }

  def getFeed(feedId: FeedID, from: Int, limit: Int): NFIO[List[PublishedPost]] =
    for {
      c <- ask
      feed = c.streamClient.flatFeed(feedId)
      as <- liftIO(
        feed.getEnrichedActivities(
          new Limit(limit),
          new Offset(from),
          new EnrichmentFlags().withReactionCounts()
        ).toIO
      )
    } yield as.asScala.toList.map(publishedPostFrom)

  def publishedPostFrom(activity: Activity): PublishedPost =
    PublishedPost(
      activityId = activity.getID(),
      likeCount = 0,
      post = Post(
        postId = activity.getForeignID(),
        content = activity.getObject(),
        author = activity.getActor(),
        brand = activity.getExtra().get("brand").asInstanceOf[String],
        product = Option(activity.getExtra().get("product").asInstanceOf[String]),
        timestamp = activity.getTime().getTime(),
        hashtags =
          activity.getExtra().get("hashtags").asInstanceOf[java.util.List[String]]
            .asScala.toList
            .map(Hashtag.apply)
      )
    )
  // must be duplicated for EnrichedActivity, because the getstream client
  // library doesn't implement any conversions b/w Activity and EnrichedActivity
  def publishedPostFrom(activity: EnrichedActivity): PublishedPost =
    PublishedPost(
      activityId = activity.getID(),
      likeCount = activity.getReactionCounts().get("like").intValue(),
      post = Post(
        postId = activity.getForeignID(),
        content = activity.getObject().getID(),
        author = activity.getActor().getID(),
        brand = activity.getExtra().get("brand").asInstanceOf[String],
        product = Option(activity.getExtra().get("product").asInstanceOf[String]),
        timestamp = activity.getTime().getTime(),
        hashtags =
          activity.getExtra().get("hashtags").asInstanceOf[java.util.List[String]]
            .asScala.toList
            .map(Hashtag.apply)
      )
    )

  def followBrand(follower: User, brand: Brand): NFIO[Unit] = {
    val feed = brandFeedName(brand)
    followFeed(follower, feed)
  }

  def followHashtag(follower: User, hashtag: Hashtag): NFIO[Unit] = {
    val feed = hashtagFeedName(hashtag)
    followFeed(follower, feed)
  }

  def followFeed(follower: User, feedId: FeedID): NFIO[Unit] =
    for {
      c <- ask
      timeline = userFeedName(follower)
      followerTimeline = c.streamClient.flatFeed(timeline)
      feed = c.streamClient.flatFeed(feedId)
      _ <- liftIO(followerTimeline.follow(feed).toIO)
    } yield ()

  def likePost(user: User, post: PublishedPost): NFIO[Unit] =
    for {
      c <- ask
      like = new Reaction.Builder()
        .kind("like")
        .activityID(post.activityId)
        .build()
      _ <- liftIO(c.streamClient.reactions().add(user.userId, like).toIO)
    } yield ()

}
