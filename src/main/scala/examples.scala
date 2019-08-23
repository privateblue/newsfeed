import model._
import context._
import effects._
import db._
import pretty._
import utils._

import cats._
import cats.implicits._

object examples {

  val alice = "user-1"
  val bob = "user-2"
  val chris = "user-3"

  val users = List(alice, bob, chris)

  val apple = "brand-1"
  val avocado = "brand-2"
  val beet = "brand-3"
  val banana = "brand-4"
  val coconut = "brand-5"

  val brands = Map(
    alice -> List(apple, avocado),
    bob -> List(beet, banana),
    chris -> List(coconut)
  )

  val macintosh = "product-1"

  val products = Map(
    apple -> List(macintosh)
  )

  val now = System.currentTimeMillis

  val post1 = Post(
    postId = "post-1",
    timestamp = now,
    permalink = "/post-1",
    authorId = alice,
    brandId = apple,
    productId = Some(macintosh),
    body = PostBody(
      text = "Check out this new apple",
      images = List()
    ),
    hashtags = List("Fresh", "JustIn", "Health")
  )

  val post2 = Post(
    postId = "post-2",
    timestamp = now,
    permalink = "/post-2",
    authorId = alice,
    brandId = avocado,
    productId = None,
    body = PostBody(
      text = "Happy Monday from Avocado",
      images = List()
    ),
    hashtags = List("ThankGodItsMonday", "Health")
  )

  val thousandUsers =
    1.to(1000).toList.map(n => s"user-$n")

  val thousandHashtags =
    1.to(1000).toList.map(n => s"Hashtag-$n")

  val hundredHashtags = thousandHashtags.take(100)

  def thousandPostsFrom(s: Int) =
    s.to(s + 999).toList.map { n =>
      val author = pickOne(users)
      val brand = pickOne(brands(author))
      val product = pickUpTo(1, products.get(brand).toList.flatten).headOption
      Post(
        postId = s"post-$n",
        timestamp = now,
        permalink = s"/post-$n",
        authorId = author,
        brandId = brand,
        productId = product,
        body = PostBody(
          text = s"Post no. $n",
          images = List()
        ),
        hashtags = pickUpTo(5, thousandHashtags)
      )
    }

  val newsfeeds = StreamJavaFeeds(
    key = "zffns4bft8ct",
    secret = "a3xwb4tf7r2n7hx52xk8fd9buv2zte4acehrtaj8yajx27drkwhyfs2r5jymsgbn"
  )

  // Publishing posts to multiple feeds at the same time
  val scenario1 = for {
    // Alice publishes a posts
    post1Published <- newsfeeds.add(post1)

    // Retrieving Apple brand feed
    appleFeed <- newsfeeds.brandFeed(apple, 0, 10)

    // Retrieving #Fresh hashtag feed
    freshFeed <- newsfeeds.hashtagFeed("Fresh", 0, 10)
    // Retrieving #JustIn hashtag feed
    justInFeed <- newsfeeds.hashtagFeed("JustIn", 0, 10)
    // Retrieving #Health hashtag feed
    healthFeed <- newsfeeds.hashtagFeed("Health", 0, 10)

    result = formatPostLists(Map(
        "Apple" -> appleFeed,
        "#Fresh" -> freshFeed,
        "#JustIn" -> justInFeed,
        "#Health" -> healthFeed
      ))

    // Cleaning up by removing posts from every feed they were published to
    _ <- newsfeeds.remove(post1Published)
  } yield result

  // Following brands and hashtags
  val scenario2 = for {
    // Alice publishes two posts
    post1Published <- newsfeeds.add(post1)
    post2Published <- newsfeeds.add(post2)

    // Bob follows a brand and a hashtag
    _ <- newsfeeds.followBrand(bob, apple)
    _ <- newsfeeds.followHashtag(bob, "ThankGodItsMonday")

    // Bob retrieves his timeline
    bobFeed <- newsfeeds.userFeed(bob, 0, 10)

    result = formatPostLists(Map(
        "Bob's timeline" -> bobFeed
      ))

    // Cleaning up by removing posts from every feed they were published to
    _ <- newsfeeds.remove(post1Published)
    _ <- newsfeeds.remove(post2Published)
  } yield result

  // Liking posts
  // val scenario3 = for {
  //   // Alice publishes a post
  //   post1Published <- newsfeeds.add(post1)
  //
  //   // Bob follows the brand
  //   _ <- newsfeeds.followBrand(bob, apple)
  //
  //   // Bob likes the post
  //   _ <- newsfeeds.like(bob, post1Published)
  //
  //   // Then he likes it again somehow
  //   _ <- newsfeeds.like(bob, post1Published)
  //
  //   // Bob retrieves his timeline
  //   bobFeed <- newsfeeds.userFeed(bob, 0, 10)
  //
  //   // Let's take the first (and only) post from Bob's timeline
  //   post1InFeed = bobFeed.head
  //
  //   // Let's first take the number of likes returned by getstream.io
  //   nonUniqueLikeCount = post1InFeed.nonUniqueLikeCount
  //
  //   // Let's count likes ourselves too, by retrieving all of them,
  //   // and deduplicating in the background
  //   likes <- newsfeeds.likes(post1InFeed)
  //   likeCount = likes.size
  //
  //   result = (nonUniqueLikeCount, likeCount)
  //
  //   // Cleaning up by removing posts from every feed they were published to
  //   _ <- newsfeeds.remove(post1Published)
  // } yield result

  // Pagination of feeds
  val scenario4 = for {
    // Publishing 1000 random posts to random hashtags
    publishedPosts <- thousandPostsFrom(1).map(newsfeeds.add).sequence

    // Getting the last 10 posts of a brand feed
    page1 <- newsfeeds.brandFeed(apple, 0, 10)

    // Getting the previous 10 posts
    page2 <- newsfeeds.brandFeed(apple, 10, 10)

    // Getting again the previous 10 posts
    page3 <- newsfeeds.brandFeed(apple, 20, 10)

    result = formatPostLists(Map(
        "Apple feed page 1" -> page1,
        "Apple feed page 2" -> page2,
        "Apple feed page 3" -> page3,
      ))

    // No cleanup this time, as deleting 1000 posts exceeds an API rate limit
  } yield result

  // Listing followed brands / hashtag by a user
  val scenario5 = for {
    // Alice publishes two posts
    post1Published <- newsfeeds.add(post1)
    post2Published <- newsfeeds.add(post2)

    // Bob follows a few brands and hashtags
    _ <- newsfeeds.followBrand(bob, apple)
    _ <- newsfeeds.followBrand(bob, beet)
    _ <- newsfeeds.followBrand(bob, coconut)
    _ <- newsfeeds.followHashtag(bob, "ThankGodItsMonday")

    // Chris follows the Beet brand too
    _ <- newsfeeds.followBrand(chris, beet)

    // Getting the first 10 brands and hashtags Bob follows
    followed <- newsfeeds.followed(bob, 0, 10)
    (brandsFollowed, hashtagsFollowed) = followed

    result = formatMap[Id, String](Map(
        "Brands Bob follows" -> formatBrandList(brandsFollowed),
        "Hashtags Bob follows" -> formatHashtagList(hashtagsFollowed)
      ), x => x)

    // Cleaning up by removing posts from every feed they were published to
    _ <- newsfeeds.remove(post1Published)
    _ <- newsfeeds.remove(post2Published)
  } yield result

  // Posting to a 100 hashtags at the same time
  val scenario6 = for {
    // Bob follows 1000 different hashtags
    _ <- newsfeeds.followHashtags(Map(bob -> hundredHashtags))

    // Publishing a post to a 100 hashtags at the same time
    publishedPost <- newsfeeds.add(Post(
      postId = "post-3002",
      timestamp = now,
      permalink = "/post-3002",
      authorId = alice,
      brandId = apple,
      productId = Some(macintosh),
      body = PostBody(
        text = "This post is posted to a hundred hashtags",
        images = List()
      ),
      hashtags = hundredHashtags
    ))

    // Getting the last post of these hundred hashtag feeds
    latestPostsOfHundredHashtags <- hundredHashtags.map(h =>
      newsfeeds.hashtagFeed(h, 0, 1).map((s"Latest post of #$h" -> _))
    ).sequence

    // Bob retrieves his timeline
    bobFeed <- newsfeeds.userFeed(bob, 0, 10)

    result = formatPostLists(latestPostsOfHundredHashtags.toMap ++ Map(
      "Bob's timeline" -> bobFeed
    ))

    // Cleaning up by removing the post from every feed it was published to
    _ <- newsfeeds.remove(publishedPost)
  } yield result

  // Posting to a 1000 followers at the same time
  val scenario7 = for {
    // 1000 users follow the brand Apple
    _ <- thousandUsers.map(u => newsfeeds.followBrand(u, apple)).sequence

    post = Post(
      postId = "post-3003",
      timestamp = now,
      permalink = "/post-3003",
      authorId = alice,
      brandId = apple,
      productId = Some(macintosh),
      body = PostBody(
        text = "This post is read by a 1000 followers",
        images = List()
      ),
      hashtags = hundredHashtags
    )

    // Apple publishes a post
    publishedPost <- newsfeeds.add(post)

    // We retrieve the latest post of every user's timeline
    userLatestPosts <- thousandUsers.map(u => newsfeeds.userFeed(u, 0, 1)).sequence

    // We retrieve the latest post of every hashtag feed
    hashtagLatestPosts <- hundredHashtags.map(h => newsfeeds.hashtagFeed(h, 0, 1)).sequence

    // Cleaning up by removing the post from every feed it was published to
    _ <- newsfeeds.remove(publishedPost)

    // We retrieve the latest post of every user's timeline again, after cleanup
    userLatestPostsAfterCleanup <- thousandUsers.map(u => newsfeeds.userFeed(u, 0, 1)).sequence

    // We retrieve the latest post of every hashtag feed again, after cleanup
    hashtagLatestPostsAfterCleanup <- hundredHashtags.map(h => newsfeeds.hashtagFeed(h, 0, 1)).sequence

    result = formatMap[Id, Boolean](Map(
      "All followers see the post" -> userLatestPosts.forall(
        ps => ps.headOption.map(_.post == post).getOrElse(false)
      ),
      "All hashtags show the post" -> hashtagLatestPosts.forall(
        ps => ps.headOption.map(_.post == post).getOrElse(false)
      ),
      "Post removed from all followers" -> userLatestPostsAfterCleanup.forall(
        ps => ps.headOption.map(_.post != post).getOrElse(false)
      ),
      "Post removed from all hashtags" -> hashtagLatestPostsAfterCleanup.forall(
        ps => ps.headOption.map(_.post != post).getOrElse(false)
      )
    ), x => x.toString)
  } yield result

  val context = AppContext()

  def main(args: Array[String]): Unit =
    run(scenario7, context)
      .fold(println, println)

}
