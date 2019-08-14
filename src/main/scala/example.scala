import model._
import context._
import effect._
import db._

import cats.implicits._

object example {

  val context = AppContext(
    userStore = InMemoryStore[UserId, User](_.userId),
    brandStore = InMemoryStore[BrandId, Brand](_.brandId),
    productStore = InMemoryStore[ProductId, Product](_.productId),
    feed = StreamJavaFeed(
      key = "zffns4bft8ct",
      secret = "a3xwb4tf7r2n7hx52xk8fd9buv2zte4acehrtaj8yajx27drkwhyfs2r5jymsgbn"
    )
  )

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

  val program = for {
    _ <- initializeDB
    c <- ask

    post1Published <- c.feed.add(post1)
    post2Published <- c.feed.add(post2)
    _ <- c.feed.followBrand(bob, apple)
    _ <- c.feed.followHashtag(bob, Hashtag("ThankGodItsMonday"))
    _ <- c.feed.like(bob, post1Published)
    _ <- c.feed.like(bob, post1Published) // liking again to see if deduplication works
    _ <- c.feed.like(chris, post1Published)
    _ <- c.feed.like(chris, post2Published)
    _ <- c.feed.unlike(bob, post1Published)

    bobFeed <- c.feed.userFeed(bob, 0, 10)

    // getting likes for each post in the feed (one api call per post!)
    bobFeedWithCounts <- bobFeed.map(p => c.feed.likes(p).map(p -> _.size)).sequence

    _ <- c.feed.remove(post1Published)
    _ <- c.feed.remove(post2Published)
  } yield bobFeedWithCounts

  def main(args: Array[String]): Unit =
    run(program, context)
      .fold(println, l => println(l.mkString("\n")))
}
