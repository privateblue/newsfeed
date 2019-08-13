import model._
import db._

object context {

  case class AppContext(
    userStore: Store[UserId, User],
    brandStore: Store[BrandId, Brand],
    productStore: Store[ProductId, Product],
    streamClient: io.getstream.client.Client
  )

}
