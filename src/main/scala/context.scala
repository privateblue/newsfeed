import model._
import db._

object context {

  case class AppContext(
    brandStore: Store[BrandId, BrandInfo],
  )

}
