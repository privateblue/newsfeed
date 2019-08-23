import model._
import effects._

trait Newsfeeds {

  // feed identifiers

  type FeedId

  protected def userFeedId(userId: UserId): FeedId

  protected def brandFeedId(brandId: BrandId): FeedId

  protected def hashtagFeedId(hashtag: Hashtag): FeedId

  // get / add / remove posts

  def get(postId: PostId): NFIO[PublishedPost]

  def add(post: Post): NFIO[PublishedPost]

  def remove(post: PublishedPost): NFIO[Unit]

  // read feeds

  protected def getFeed(feedId: FeedId, from: Int, limit: Int): NFIO[List[PublishedPost]]

  def userFeed(userId: UserId, from: Int, limit: Int): NFIO[List[PublishedPost]] =
    getFeed(userFeedId(userId), from, limit)

  def brandFeed(brandId: BrandId, from: Int, limit: Int): NFIO[List[PublishedPost]] =
    getFeed(brandFeedId(brandId), from, limit)

  def hashtagFeed(hashtag: Hashtag, from: Int, limit: Int): NFIO[List[PublishedPost]] =
    getFeed(hashtagFeedId(hashtag), from, limit)

  // follow / unfollow feeds

  protected def follow(follower: UserId, feedId: FeedId): NFIO[Unit]

  protected def unfollow(follower: UserId, feedId: FeedId): NFIO[Unit]

  protected def batchFollow(follows: Map[UserId, List[FeedId]]): NFIO[Unit]

  def followBrand(follower: UserId, brandId: BrandId): NFIO[Unit] =
    follow(follower, brandFeedId(brandId))

  def unfollowBrand(follower: UserId, brandId: BrandId): NFIO[Unit] =
    unfollow(follower, brandFeedId(brandId))

  def followBrands(follows: Map[UserId, List[BrandId]]): NFIO[Unit] = {
    val feedIdFollows = follows.map {
      case (userId, brandIds) => userId -> brandIds.map(brandFeedId)
    }
    batchFollow(feedIdFollows)
  }

  def followHashtag(follower: UserId, hashtag: Hashtag): NFIO[Unit] =
    follow(follower, hashtagFeedId(hashtag))

  def unfollowHashtag(follower: UserId, hashtag: Hashtag): NFIO[Unit] =
    unfollow(follower, hashtagFeedId(hashtag))

  def followHashtags(follows: Map[UserId, List[Hashtag]]): NFIO[Unit] = {
    val feedIdFollows = follows.map {
      case (userId, hashtags) => userId -> hashtags.map(hashtagFeedId)
    }
    batchFollow(feedIdFollows)
  }

  def followed(userId: UserId, from: Int, limit: Int): NFIO[(List[BrandId], List[Hashtag])]

  def hasFollowedBrand(userId: UserId, brandId: BrandId): NFIO[Boolean]

  def followedBrandCount(userId: UserId): NFIO[Int]

  def followedHashtagCount(userId: UserId): NFIO[Int]

  def brandFollowerCount(brandId: BrandId): NFIO[Int]

  // like posts

  def like(user: UserId, post: PublishedPost): NFIO[Unit]

  def hasLiked(userId: UserId, postId: PostId): NFIO[Boolean]

}
