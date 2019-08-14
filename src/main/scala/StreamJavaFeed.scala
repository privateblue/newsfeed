import model._
import effect._

import io.getstream.client.Client
import io.getstream.core.LookupKind
import io.getstream.core.models.{FeedID, Activity, EnrichedActivity, Reaction}
import io.getstream.core.options.{Limit, Offset, EnrichmentFlags}

import cats.implicits._

import scala.collection.JavaConverters._

case class StreamJavaFeed(key: String, secret: String) extends Feed {

  private val client = Client.builder(key, secret).build()

  type FeedId = io.getstream.core.models.FeedID

  override def userFeedId(user: User): FeedID =
    new FeedID("user:" + user.userId)

  override def brandFeedId(brand: Brand): FeedID =
    new FeedID("brand:" + brand.brandId)

  override def hashtagFeedId(hashtag: Hashtag): FeedID =
    new FeedID("hashtag:" + hashtag.name)

  override def add(post: Post): NFIO[PublishedPost] =
    for {
      c <- ask
      brand <- c.brandStore.get(post.brand)
      feedId = brandFeedId(brand)
      brandFeed = client.flatFeed(feedId)
      activity = activityFrom(post)
      result <- liftIO(brandFeed.addActivity(activity).toIO)
    } yield publishedPostFrom(result)

  override def remove(post: PublishedPost): NFIO[Unit] =
    for {
      c <- ask
      brand <- c.brandStore.get(post.post.brand)
      feedId = brandFeedId(brand)
      brandFeed = client.flatFeed(feedId)
      _ <- liftIO(brandFeed.removeActivityByID(post.publishId).toIO)
      feedIds = post.post.hashtags.map(hashtagFeedId)
      _ <- liftIO(
          feedIds.map(fid =>
            client.flatFeed(fid).removeActivityByID(post.publishId).toIO
          ).sequence
        )
    } yield ()

  private def activityFrom(post: Post): Activity = {
    // calculate list of hashtag feeds to also add activity to
    val recipients = post.hashtags.map(hashtagFeedId).asJava
    // set author, verb, content, postId, time
    val a = Activity.builder()
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
    val awp =
      post.product
        .map(pid => a.extraField("product", pid))
        .getOrElse(a)
    awp.build()
  }

  private def publishedPostFrom(activity: Activity): PublishedPost =
    PublishedPost(
      publishId = activity.getID(),
      nonUniqueLikeCount = 0,
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
  private def publishedPostFrom(activity: EnrichedActivity): PublishedPost =
    PublishedPost(
      publishId = activity.getID(),
      nonUniqueLikeCount =
        Option(activity.getReactionCounts().get("like"))
          .map(_.intValue())
          .getOrElse(0),
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

  override def get(feedId: FeedID, from: Int, limit: Int): NFIO[List[PublishedPost]] =
    for {
      c <- ask
      feed = client.flatFeed(feedId)
      as <- liftIO(
        feed.getEnrichedActivities(
          new Limit(limit),
          new Offset(from),
          new EnrichmentFlags().withReactionCounts()
        ).toIO
      )
    } yield as.asScala.toList.map(publishedPostFrom)

  override def follow(follower: User, feedId: FeedID): NFIO[Unit] =
    for {
      c <- ask
      timeline = userFeedId(follower)
      followerTimeline = client.flatFeed(timeline)
      feed = client.flatFeed(feedId)
      _ <- liftIO(followerTimeline.follow(feed).toIO)
    } yield ()

  override def unfollow(follower: User, feedId: FeedID): NFIO[Unit] =
    for {
      c <- ask
      timeline = userFeedId(follower)
      followerTimeline = client.flatFeed(timeline)
      feed = client.flatFeed(feedId)
      _ <- liftIO(followerTimeline.unfollow(feed).toIO)
    } yield ()

  override def like(user: User, post: PublishedPost): NFIO[Unit] =
    for {
      c <- ask
      like = new Reaction.Builder()
        .kind("like")
        .activityID(post.publishId)
        .build()
      _ <- liftIO(client.reactions().add(user.userId, like).toIO)
    } yield ()

  override def unlike(user: User, post: PublishedPost): NFIO[Unit] =
    for {
      c <- ask
      reactions <- liftIO(
        client.reactions().filter(LookupKind.ACTIVITY, post.publishId, "like").toIO
      )
      _ <- liftIO(
          reactions.asScala.toList
            .filter(_.getUserID() == user.userId)
            .map(r =>
              client.reactions().delete(r.getId()).toIO
            ).sequence
        )
    } yield ()

  override def likes(post: PublishedPost): NFIO[List[User]] =
    for {
      c <- ask
      reactions <- liftIO(
        client.reactions().filter(LookupKind.ACTIVITY, post.publishId, "like").toIO
      )
      userIds = reactions.asScala.toList.map(_.getUserID()).distinct
      users <- userIds.map(c.userStore.get).sequence
    } yield users

}
