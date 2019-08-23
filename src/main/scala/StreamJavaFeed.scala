import model._
import effects._

import io.getstream.client.Client
import io.getstream.core.LookupKind
import io.getstream.core.models.{FeedID, Activity, EnrichedActivity, Reaction, FollowRelation}
import io.getstream.core.options.{Limit, Offset, EnrichmentFlags}

import argonaut._, Argonaut._

import cats.implicits._

import scala.collection.JavaConverters._

case class StreamJavaFeeds(key: String, secret: String) extends Newsfeeds {

  private val client = Client.builder(key, secret).build()

  type FeedId = io.getstream.core.models.FeedID

  protected val userSlug = "user"
  protected val brandSlug = "brand"
  protected val hashtagSlug = "hashtag"

  protected override def userFeedId(userId: UserId): FeedID =
    new FeedID(userSlug, userId)

  protected override def brandFeedId(brandId: BrandId): FeedID =
    new FeedID(brandSlug, brandId)

  protected override def hashtagFeedId(hashtag: Hashtag): FeedID =
    new FeedID(hashtagSlug, hashtag)

  override def get(postId: PostId): NFIO[PostView] =
    ???

  override def add(post: Post): NFIO[PostView] = {
    val feedId = brandFeedId(post.brandId)
    val brandFeed = client.flatFeed(feedId)
    val activity = activityFrom(post)
    brandFeed.addActivity(activity).toIO.to[NFIO]
      .map(publishedPostFrom)
  }

  override def remove(postView: PostView): NFIO[Unit] = {
    val feedId = brandFeedId(postView.post.brandId)
    val brandFeed = client.flatFeed(feedId)
    val hashtagFeedIds = postView.post.hashtags.map(hashtagFeedId)
    for {
      _ <- brandFeed.removeActivityByForeignID(postView.post.postId).toIO.to[NFIO]
      _ <- hashtagFeedIds.map(fid =>
          client.flatFeed(fid).removeActivityByForeignID(postView.post.postId).toIO.to[NFIO]
        ).sequence
    } yield ()
  }

  private def activityFrom(post: Post): Activity = {
    // calculate list of hashtag feeds to also add activity to
    val recipients = post.hashtags.map(hashtagFeedId).asJava
    // set author, verb, content, postId, time
    val a = Activity.builder()
      .actor(post.authorId)
      .verb("post")
      .`object`(EncodeJson.of[PostBody].encode(post.body).nospaces)
      .foreignID(post.postId)
      .time(new java.util.Date(post.timestamp))
      // set hashtags as targets
      .to(recipients)
      // set brand and hashtags as custom fields
      .extraField("brand", post.brandId)
      .extraField("hashtags", post.hashtags.asJava)
    // optionally set product (if present) as a custom field
    val awp =
      post.productId
        .map(pid => a.extraField("product", pid))
        .getOrElse(a)
    awp.build()
  }

  private def publishedPostFrom(activity: Activity): PostView =
    PostView(
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
  private def publishedPostFrom(activity: EnrichedActivity): PostView =
    PostView(
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

  protected override def get(feedId: FeedID, from: Int, limit: Int): NFIO[List[PublishedPost]] =
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

  protected override def follow(follower: User, feedId: FeedID): NFIO[Unit] =
    for {
      c <- ask
      timeline = userFeedId(follower)
      followerTimeline = client.flatFeed(timeline)
      feed = client.flatFeed(feedId)
      _ <- liftIO(followerTimeline.follow(feed).toIO)
    } yield ()

  protected override def unfollow(follower: User, feedId: FeedID): NFIO[Unit] =
    for {
      c <- ask
      timeline = userFeedId(follower)
      followerTimeline = client.flatFeed(timeline)
      feed = client.flatFeed(feedId)
      _ <- liftIO(followerTimeline.unfollow(feed).toIO)
    } yield ()

  protected override def followN(follower: User, feedIds: List[FeedId]): NFIO[Unit] =
    for {
      c <- ask
      timeline = userFeedId(follower)
      follows = feedIds.map(fid => new FollowRelation(timeline.toString, fid.toString))
      _ <- liftIO(client.batch().followMany(follows.asJava).toIO)
    } yield ()

  // The following two methods rely on the internal structure of FeedID, that is
  // basically a convention, encoded in the first few lines of this class. This
  // is pretty unfortunate.

  protected override def followers(feedId: FeedId, from: Int, limit: Int): NFIO[List[User]] =
    for {
      c <- ask
      feed = client.flatFeed(feedId)
      follows <- liftIO(feed.getFollowers(new Limit(limit), new Offset(from)).toIO)
      users <- follows.asScala.toList.map { f =>
          val userId = new FeedID(f.getSource()).getUserID()
          c.userStore.get(userId)
        }.sequence
    } yield users

  override def followed(user: User, from: Int, limit: Int): NFIO[(List[Brand], List[Hashtag])] = {
    val timelineId = userFeedId(user)
    val timeline = client.flatFeed(timelineId)
    for {
      c <- ask
      followed <- liftIO(timeline.getFollowed(new Limit(limit), new Offset(from)).toIO)
      feedIds = followed.asScala.toList.map(f => new FeedID(f.getTarget()))
      brands <- feedIds.collect {
          case fid if fid.getSlug() == brandSlug => c.brandStore.get(fid.getUserID())
        }.sequence
      hashtags = feedIds.collect {
          case fid if fid.getSlug() == hashtagSlug => Hashtag(fid.getUserID())
        }
    } yield (brands, hashtags)
  }

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
