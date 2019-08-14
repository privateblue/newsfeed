import model._
import context._
import effect._
import db._

import cats.implicits._

object example {

  val alice = User("a", "Alice")
  val bob = User("b", "Bob")
  val chris = User("c", "Chris")

  val apple = Brand("apple", "Apple", alice.userId)
  val avocado = Brand("avocado", "Avocado", alice.userId)
  val beet = Brand("beet", "Beet", bob.userId)
  val banana = Brand("banana", "Banana", bob.userId)

  val macintosh = Product("m", "Macintosh", apple.brandId, List())

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
    postId = "p1",
    content = "Check out this new apple",
    author = alice.userId,
    brand = apple.brandId,
    product = Some(macintosh.productId),
    timestamp = now,
    hashtags = List(Hashtag("Fresh"), Hashtag("JustIn"), Hashtag("Health"))
  )

  val post2 = Post(
    postId = "p2",
    content = "Happy Monday from Avocado",
    author = alice.userId,
    brand = avocado.brandId,
    product = None,
    timestamp = now,
    hashtags = List(Hashtag("ThankGodItsMonday"), Hashtag("Health"))
  )

  val context = AppContext(
    userStore = InMemoryStore[UserId, User](_.userId),
    brandStore = InMemoryStore[BrandId, Brand](_.brandId),
    productStore = InMemoryStore[ProductId, Product](_.productId)
  )

  val newsfeeds = StreamJavaFeeds(
    key = "zffns4bft8ct",
    secret = "a3xwb4tf7r2n7hx52xk8fd9buv2zte4acehrtaj8yajx27drkwhyfs2r5jymsgbn"
  )

  val program = for {
    _ <- initializeDB
    c <- ask

    // Alice publishes two posts
    post1Published <- newsfeeds.add(post1)
    post2Published <- newsfeeds.add(post2)

    // Bob follows a brand and a hashtag
    _ <- newsfeeds.followBrand(bob, apple)
    _ <- newsfeeds.followHashtag(bob, Hashtag("ThankGodItsMonday"))

    // Bob likes a post on his timeline
    _ <- newsfeeds.like(bob, post1Published)

    // Likes it again, to see at the end if deduplication works
    _ <- newsfeeds.like(bob, post1Published)

    // Chris likes two posts on her timeline
    _ <- newsfeeds.like(chris, post1Published)
    _ <- newsfeeds.like(chris, post2Published)

    // Bob retrieves his timeline
    bobFeed <- newsfeeds.userFeed(bob, 0, 10)

    // Getting likes for each post in Bob's timeline (one api call per post!)
    bobFeedWithLikeCounts <- bobFeed.map(
      p => newsfeeds.likes(p).map(p -> _.size)
    ).sequence

    // Cleaning up by removing posts from every feed they were published to
    _ <- newsfeeds.remove(post1Published)
    _ <- newsfeeds.remove(post2Published)

  } yield bobFeedWithLikeCounts

  def main(args: Array[String]): Unit =
    run(program, context)
      .fold(println, l => println(l.mkString("\n")))
}
