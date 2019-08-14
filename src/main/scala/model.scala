object model {

  type UserId = String

  case class User(
    userId: UserId,
    name: String
  )

  type BrandId = String

  case class Brand(
    brandId: BrandId,
    name: String,
    supplier: UserId
  )

  type ProductId = String

  case class Product(
    productId: ProductId,
    name: String,
    brand: BrandId,
    hashtags: List[Hashtag]
  )

  case class Hashtag(
    name: String
  )

  type PostId = String

  case class Post(
    postId: PostId,
    content: String,
    author: UserId,
    brand: BrandId,
    product: Option[ProductId],
    timestamp: Long,
    hashtags: List[Hashtag]
  )

  case class PublishedPost(
    publishId: String,
    likeCount: Int,
    post: Post
  )

}
