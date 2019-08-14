import model._
import effect._

trait Feed {

  // feed identifiers

  type FeedId

  def userFeedId(user: User): FeedId

  def brandFeedId(brand: Brand): FeedId

  def hashtagFeedId(hashtag: Hashtag): FeedId

  // add / remove posts

  def add(post: Post): NFIO[PublishedPost]

  def remove(post: PublishedPost): NFIO[Unit]

  // read feeds

  protected def get(feedId: FeedId, from: Int, limit: Int): NFIO[List[PublishedPost]]

  def userFeed(user: User, from: Int, limit: Int): NFIO[List[PublishedPost]] =
    get(userFeedId(user), from, limit)

  def brandFeed(brand: Brand, from: Int, limit: Int): NFIO[List[PublishedPost]] =
    get(brandFeedId(brand), from, limit)

  def hashtagFeed(hashtag: Hashtag, from: Int, limit: Int): NFIO[List[PublishedPost]] =
    get(hashtagFeedId(hashtag), from, limit)

  // follow / unfollow feeds

  protected def follow(follower: User, feedId: FeedId): NFIO[Unit]

  protected def unfollow(follower: User, feedId: FeedId): NFIO[Unit]

  def followUser(follower: User, user: User): NFIO[Unit] =
    follow(follower, userFeedId(user))

  def followBrand(follower: User, brand: Brand): NFIO[Unit] =
    follow(follower, brandFeedId(brand))

  def followHashtag(follower: User, hashtag: Hashtag): NFIO[Unit] =
    follow(follower, hashtagFeedId(hashtag))

  def unfollowUser(follower: User, user: User): NFIO[Unit] =
    unfollow(follower, userFeedId(user))

  def unfollowBrand(follower: User, brand: Brand): NFIO[Unit] =
    unfollow(follower, brandFeedId(brand))

  def unfollowHashtag(follower: User, hashtag: Hashtag): NFIO[Unit] =
    unfollow(follower, hashtagFeedId(hashtag))

  // like / unlike posts

  def like(user: User, post: PublishedPost): NFIO[Unit]

  // TODO def unlike(user: User, post: PublishedPost): NFIO[Unit]

}
