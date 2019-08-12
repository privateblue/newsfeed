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

  sealed trait PostSubject
  case class BrandPost(brandId: BrandId) extends PostSubject
  case class ProductPost(productId: ProductId) extends PostSubject

  case class Post(
    postId: PostId,
    content: String,
    author: UserId,
    subject: PostSubject,
    hashtags: List[Hashtag]
  )
}
