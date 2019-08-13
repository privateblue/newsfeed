import model._
import context._
import effect._
import db._
import feeds._

import io.getstream.client.Client

object example {

  val context = AppContext(
    userStore = InMemoryStore[UserId, User](_.userId),
    brandStore = InMemoryStore[BrandId, Brand](_.brandId),
    productStore = InMemoryStore[ProductId, Product](_.productId),
    streamClient = Client.builder(
      "zffns4bft8ct",
      "a3xwb4tf7r2n7hx52xk8fd9buv2zte4acehrtaj8yajx27drkwhyfs2r5jymsgbn"
    ).build()
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

  val post1 = Post(
    postId = "p1",
    content = "Check out this new apple",
    author = alice.userId,
    brand = apple.brandId,
    product = Some(macintosh.productId),
    hashtags = List(Hashtag("Fresh"), Hashtag("JustIn"), Hashtag("Health"))
  )

  val post2 = Post(
    postId = "p2",
    content = "Happy Monday from Avocado",
    author = alice.userId,
    brand = avocado.brandId,
    product = None,
    hashtags = List(Hashtag("ThankGodItsMonday"), Hashtag("Health"))
  )

  val program = for {
    _ <- initializeDB
    _ <- add(post1)
    _ <- add(post2)
    _ <- followBrand(bob, apple)
    _ <- followHashtag(bob, Hashtag("ThankGodItsMonday"))
    bobFeed <- getUserFeed(bob, 0, 10)
  } yield bobFeed

  def main(args: Array[String]): Unit =
    run(program, context)
      .fold(println, println)
}
