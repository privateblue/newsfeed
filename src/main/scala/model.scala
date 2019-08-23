import argonaut._, Argonaut._

object model {

  type UserId = String

  type BrandId = String

  case class BrandInfo(
    brandId: BrandId,
    name: String,
    storeUri: String
  )

  type ProductId = String

  type ImageUri = String

  type Hashtag = String

  type PostId = String

  case class PostBody(
    text: String,
    images: List[ImageUri]
  )

  object PostBody {
    implicit val postBodyCodec =
      casecodec2(PostBody.apply, PostBody.unapply)("text", "images")
  }

  case class Post(
    postId: PostId,
    timestamp: Long,
    permalink: String,
    authorId: UserId,
    brandId: BrandId,
    productId: Option[ProductId],
    body: PostBody,
    hashtags: List[Hashtag]
  )

  case class PublishedPost(
    publishId: String,
    post: Post
  )

  case class PostView(
    viewerId: UserId,
    hasViewerLiked: Boolean,
    hasViewerFollowedBrand: Boolean,
    likeCount: Int,
    brandInfo: BrandInfo,
    publishedPost: PublishedPost
  )

}
