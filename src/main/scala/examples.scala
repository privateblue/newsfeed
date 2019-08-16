import model._
import context._
import effects._
import db._
import pretty._
import utils._

import cats.implicits._

object examples {

  val alice = User("user-1", "Alice")
  val bob = User("user-2", "Bob")
  val chris = User("user-3", "Chris")

  val users = List(alice, bob, chris)

  val apple = Brand("brand-1", "Apple", alice.userId)
  val avocado = Brand("brand-2", "Avocado", alice.userId)
  val beet = Brand("brand-3", "Beet", bob.userId)
  val banana = Brand("brand-4", "Banana", bob.userId)
  val coconut = Brand("brand-5", "Coconut", chris.userId)

  val brands = Map(
    alice.userId -> List(apple, avocado),
    bob.userId -> List(beet, banana),
    chris.userId -> List(coconut)
  )

  val macintosh = Product("product-1", "Macintosh", apple.brandId, List())

  val products = Map(
    apple.brandId -> List(macintosh)
  )

  val initializeDB =
    for {
      c <- ask
      _ <- users.map(c.userStore.put).sequence
      _ <- brands.values.toList.flatten.map(c.brandStore.put).sequence
      _ <- products.values.toList.flatten.map(c.productStore.put).sequence
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

  val thousandHashtags =
    1.to(1000).toList.map(n => Hashtag(s"Hashtag-$n"))

  val thousandPosts =
    1.to(1000).toList.map { n =>
      val author = pickOne(users)
      val brand = pickOne(brands(author.userId))
      val product = pickUpTo(1, products.get(brand.brandId).toList.flatten).headOption
      Post(
        postId = s"post-$n",
        content = s"Post no. $n",
        author = author.userId,
        brand = brand.brandId,
        product = product.map(_.productId),
        timestamp = now,
        hashtags = pickUpTo(5, thousandHashtags)
      )
    }

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

  val scenario4 = for {
    _ <- initializeDB

    // Publishing 1000 random posts to random hashtags
    publishedPosts <- thousandPosts.map(newsfeeds.add).sequence

    // Getting the last 10 posts of a brand feed
    page1 <- newsfeeds.brandFeed(apple, 0, 10)

    // Getting the previous 10 posts
    page2 <- newsfeeds.brandFeed(apple, 10, 10)

    // Getting again the previous 10 posts
    page3 <- newsfeeds.brandFeed(apple, 20, 10)

    result <- formatPostLists(Map(
        "Apple feed page 1" -> page1,
        "Apple feed page 2" -> page2,
        "Apple feed page 3" -> page3,
      ))

    // No cleanup this time, as deleting 1000 posts exceeds an API rate limit
  } yield result

  val context = AppContext(
    userStore = InMemoryStore[UserId, User](_.userId),
    brandStore = InMemoryStore[BrandId, Brand](_.brandId),
    productStore = InMemoryStore[ProductId, Product](_.productId)
  )

  def main(args: Array[String]): Unit =
    run(scenario4, context)
      .fold(println, println)

}