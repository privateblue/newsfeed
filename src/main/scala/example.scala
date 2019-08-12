import model._
import effect._
import db._
import feeds._

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
      _ <- BrandStore.put(apple)
      _ <- BrandStore.put(avocado)
      _ <- BrandStore.put(beet)
      _ <- BrandStore.put(banana)
      _ <- ProductStore.put(macintosh)
    } yield ()

  val post1 = Post(
    postId = "p1",
    content = "Check out this new apple",
    author = alice.userId,
    subject = ProductPost(macintosh.productId),
    hashtags = List(Hashtag("Fresh"), Hashtag("JustIn"), Hashtag("Health"))
  )

  val post2 = Post(
    postId = "p2",
    content = "Happy Monday from Avocado",
    author = alice.userId,
    subject = BrandPost(avocado.brandId),
    hashtags = List(Hashtag("ThankGodItsMonday"), Hashtag("Health"))
  )

  val program = for {
    _ <- initializeDB
    a1 <- add(post1)
    a2 <- add(post2)
  } yield (a1.getID(), a2.getID())

  def main(args: Array[String]): Unit =
    run(program).fold(println, println)
}
