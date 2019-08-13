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

}
