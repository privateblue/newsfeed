import model._
import context._
import effects._
import db._
import pretty._

import cats.implicits._

object examples {

  val alice = User("user-1", "Alice")
  val bob = User("user-2", "Bob")
  val chris = User("user-3", "Chris")

  val apple = Brand("brand-1", "Apple", alice.userId)
  val avocado = Brand("brand-2", "Avocado", alice.userId)
  val beet = Brand("brand-3", "Beet", bob.userId)
  val banana = Brand("brand-4", "Banana", bob.userId)

  val macintosh = Product("product-1", "Macintosh", apple.brandId, List())

  val initializeDB =
    for {
      c <- ask
      _ <- c.userStore.put(alice)
      _ <- c.userStore.put(bob)
      _ <- c.userStore.put(chris)
      _ <- c.brandStore.put(apple)
      _ <- c.brandStore.put(avocado)
      _ <- c.brandStore.put(beet)
      _ <- c.brandStore.put(banana)
      _ <- c.productStore.put(macintosh)
    } yield ()

  val now = System.currentTimeMillis

  val post1 = Post(
    postId = "post-1",
    content = "Check out this new apple",
    author = alice.userId,
    brand = apple.brandId,
    product = Some(macintosh.productId),
    timestamp = now,
    hashtags = List(Hashtag("Fresh"), Hashtag("JustIn"), Hashtag("Health"))
  )

  val post2 = Post(
    postId = "post-2",
    content = "Happy Monday from Avocado",
    author = alice.userId,
    brand = avocado.brandId,
    product = None,
    timestamp = now,
    hashtags = List(Hashtag("ThankGodItsMonday"), Hashtag("Health"))
  )

  val newsfeeds = StreamJavaFeeds(
    key = "zffns4bft8ct",
    secret = "a3xwb4tf7r2n7hx52xk8fd9buv2zte4acehrtaj8yajx27drkwhyfs2r5jymsgbn"
  )

  val scenario1 = for {
    _ <- initializeDB

    // Alice publishes a posts
    post1Published <- newsfeeds.add(post1)

    // Retrieving Apple brand feed
    appleFeed <- newsfeeds.brandFeed(apple, 0, 10)

    // Retrieving #Fresh hashtag feed
    freshFeed <- newsfeeds.hashtagFeed(Hashtag("Fresh"), 0, 10)
    // Retrieving #JustIn hashtag feed
    justInFeed <- newsfeeds.hashtagFeed(Hashtag("JustIn"), 0, 10)
    // Retrieving #Health hashtag feed
    healthFeed <- newsfeeds.hashtagFeed(Hashtag("Health"), 0, 10)

    result <- formatPostLists(Map(
        "Apple" -> appleFeed,
        "#Fresh" -> freshFeed,
        "#JustIn" -> justInFeed,
        "#Health" -> healthFeed
      ))

    // Cleaning up by removing posts from every feed they were published to
    _ <- newsfeeds.remove(post1Published)
  } yield result

  val scenario2 = for {
    _ <- initializeDB

    // Alice publishes two posts
    post1Published <- newsfeeds.add(post1)
    post2Published <- newsfeeds.add(post2)

    // Bob follows a brand and a hashtag
    _ <- newsfeeds.followBrand(bob, apple)
    _ <- newsfeeds.followHashtag(bob, Hashtag("ThankGodItsMonday"))

    // Bob retrieves his timeline
    bobFeed <- newsfeeds.userFeed(bob, 0, 10)

    result <- formatPostLists(Map(
        "Bob's timeline" -> bobFeed
      ))

    // Cleaning up by removing posts from every feed they were published to
    _ <- newsfeeds.remove(post1Published)
    _ <- newsfeeds.remove(post2Published)
  } yield result

  val scenario3 = for {
    _ <- initializeDB

    // Alice publishes a post
    post1Published <- newsfeeds.add(post1)

    // Bob follows the brand
    _ <- newsfeeds.followBrand(bob, apple)

    // Bob likes the post
    _ <- newsfeeds.like(bob, post1Published)

    // Then he likes it again somehow
    _ <- newsfeeds.like(bob, post1Published)

    // Bob retrieves his timeline
    bobFeed <- newsfeeds.userFeed(bob, 0, 10)

    // Let's take the first (and only) post from Bob's timeline
    post1InFeed = bobFeed.head

    // Let's first take the number of likes returned by getstream.io
    nonUniqueLikeCount = post1InFeed.nonUniqueLikeCount

    // Let's count likes ourselves too, by retrieving all of them,
    // and deduplicating in the background
    likes <- newsfeeds.likes(post1InFeed)
    likeCount = likes.size

    result = (nonUniqueLikeCount, likeCount)

    // Cleaning up by removing posts from every feed they were published to
    _ <- newsfeeds.remove(post1Published)
  } yield result

  val context = AppContext(
    userStore = InMemoryStore[UserId, User](_.userId),
    brandStore = InMemoryStore[BrandId, Brand](_.brandId),
    productStore = InMemoryStore[ProductId, Product](_.productId)
  )

  def main(args: Array[String]): Unit =
    run(scenario3, context)
      .fold(println, println)

}
