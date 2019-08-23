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

  override def get(postId: PostId): NFIO[PublishedPost] =
    ???

  override def add(post: Post): NFIO[PublishedPost] = {
    val feedId = brandFeedId(post.brandId)
    val brandFeed = client.flatFeed(feedId)
    val activity = activityFrom(post)
    for {
      activity <- brandFeed.addActivity(activity).toIO.to[NFIO]
      publishedPost <- publishedPostFrom(activity)
    } yield publishedPost
  }

  override def remove(post: PublishedPost): NFIO[Unit] = {
    val feedId = brandFeedId(post.post.brandId)
    val brandFeed = client.flatFeed(feedId)
    val hashtagFeedIds = post.post.hashtags.map(hashtagFeedId)
    for {
      _ <- brandFeed.removeActivityByForeignID(post.post.postId).toIO.to[NFIO]
      _ <- hashtagFeedIds.map(fid =>
          client.flatFeed(fid).removeActivityByForeignID(post.post.postId).toIO.to[NFIO]
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

  private def publishedPostFrom(activity: Activity): NFIO[PublishedPost] =
    for {
      postBody <- erroring(Parse.decodeEither[PostBody](activity.getObject()))
    } yield PublishedPost(
      publishId = activity.getID(),
      post = Post(
        postId = activity.getForeignID(),
        timestamp = activity.getTime().getTime(),
        permalink = s"/${activity.getForeignID()}",
        authorId = activity.getActor(),
        brandId = activity.getExtra().get("brand").asInstanceOf[String],
        productId = Option(activity.getExtra().get("product").asInstanceOf[String]),
        body = postBody,
        hashtags =
          activity.getExtra().get("hashtags").asInstanceOf[java.util.List[String]]
            .asScala.toList
      )
    )

  // must be duplicated for EnrichedActivity, because the getstream client
  // library doesn't implement any conversions b/w Activity and EnrichedActivity
  private def publishedPostFrom(activity: EnrichedActivity): NFIO[PublishedPost] =
    // getting the like count from the activity:
    // likeCount =
    //   Option(activity.getReactionCounts().get("like"))
    //     .map(_.intValue())
    //     .getOrElse(0)
    for {
      postBody <- erroring(Parse.decodeEither[PostBody](activity.getObject().getID()))
    } yield PublishedPost(
      publishId = activity.getID(),
      post = Post(
        postId = activity.getForeignID(),
        timestamp = activity.getTime().getTime(),
        permalink = s"/${activity.getForeignID()}",
        authorId = activity.getActor().getID(),
        brandId = activity.getExtra().get("brand").asInstanceOf[String],
        productId = Option(activity.getExtra().get("product").asInstanceOf[String]),
        body = postBody,
        hashtags =
          activity.getExtra().get("hashtags").asInstanceOf[java.util.List[String]]
            .asScala.toList
      )
    )

  protected override def getFeed(feedId: FeedID, from: Int, limit: Int): NFIO[List[PublishedPost]] = {
    val feed = client.flatFeed(feedId)
    for {
      as <- feed.getEnrichedActivities(
          new Limit(limit),
          new Offset(from),
          new EnrichmentFlags().withReactionCounts()
        ).toIO.to[NFIO]
      publishedPosts <- as.asScala.toList.map(publishedPostFrom).sequence
    } yield publishedPosts
  }

  protected override def follow(follower: UserId, feedId: FeedID): NFIO[Unit] = {
    val timeline = userFeedId(follower)
    val followerTimeline = client.flatFeed(timeline)
    val feed = client.flatFeed(feedId)
    for {
      _ <- followerTimeline.follow(feed).toIO.to[NFIO]
    } yield ()
  }

  protected override def unfollow(follower: UserId, feedId: FeedID): NFIO[Unit] = {
    val timeline = userFeedId(follower)
    val followerTimeline = client.flatFeed(timeline)
    val feed = client.flatFeed(feedId)
    for {
      _ <- followerTimeline.unfollow(feed).toIO.to[NFIO]
    } yield ()
  }

  protected override def batchFollow(follows: Map[UserId, List[FeedId]]): NFIO[Unit] = {
    val fs = follows.flatMap {
      case (follower, feedIds) =>
        feedIds.map(fid => new FollowRelation(userFeedId(follower).toString, fid.toString))
    }
    for {
      _ <- client.batch().followMany(fs.asJava).toIO.to[NFIO]
    } yield ()
  }

  // this method rely on the internal structure of FeedID, that is basically a
  // convention, encoded in the first few lines of this class. This is pretty
  // unfortunate.
  override def followed(user: UserId, from: Int, limit: Int): NFIO[(List[BrandId], List[Hashtag])] = {
    val timelineId = userFeedId(user)
    val timeline = client.flatFeed(timelineId)
    for {
      followed <- timeline.getFollowed(new Limit(limit), new Offset(from)).toIO.to[NFIO]
      feedIds = followed.asScala.toList.map(f => new FeedID(f.getTarget()))
      brandIds = feedIds.collect {
          case fid if fid.getSlug() == brandSlug => fid.getUserID()
        }
      hashtags = feedIds.collect {
          case fid if fid.getSlug() == hashtagSlug => fid.getUserID()
        }
    } yield (brandIds, hashtags)
  }

  def hasFollowedBrand(userId: UserId, brandId: BrandId): NFIO[Boolean] =
    ???

  def followedBrandCount(userId: UserId): NFIO[Int] =
    ???

  def followedHashtagCount(userId: UserId): NFIO[Int] =
    ???

  def brandFollowerCount(brandId: BrandId): NFIO[Int] =
    ???

  override def like(userId: UserId, post: PublishedPost): NFIO[Unit] =
    for {
      c <- ask
      like = new Reaction.Builder()
        .kind("like")
        .activityID(post.publishId)
        .build()
      _ <- client.reactions().add(userId, like).toIO.to[NFIO]
    } yield ()

    def hasLiked(userId: UserId, postId: PostId): NFIO[Boolean] =
      ???

}
