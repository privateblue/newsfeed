import model._
import effect._

object db {

  object BrandStore {
    var store = Map.empty[BrandId, Brand]
    def get(brandId: BrandId): NFIO[Brand] =
      store.get(brandId).map(pure).getOrElse(error("Brand not found"))
    def put(brand: Brand): NFIO[Unit] =
      pure(store = store + (brand.brandId -> brand))
  }

  object ProductStore {
    var store = Map.empty[ProductId, Product]
    def get(productId: ProductId): NFIO[Product] =
      store.get(productId).map(pure).getOrElse(error("Product not found"))
    def put(product: Product): NFIO[Unit] =
      pure(store = store + (product.productId -> product))
  }

  def brandOf(post: Post): NFIO[Brand] =
    for {
      brandId <- post.subject match {
        case BrandPost(brandId) => pure(brandId)
        case ProductPost(productId) =>
          val product = ProductStore.get(productId)
          product.map(_.brand)
      }
      brand <- BrandStore.get(brandId)
    } yield brand

  def productOf(post: Post): NFIO[Option[Product]] =
    post.subject match {
      case ProductPost(productId) => ProductStore.get(productId).map(Some.apply)
      case BrandPost(_) => pure(None)
    }

}
