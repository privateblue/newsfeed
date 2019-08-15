import model._
import effects._

object db {

  trait Store[Id, Item] {
    def get(id: Id): NFIO[Item]
    def put(item: Item): NFIO[Unit]
  }

  case class InMemoryStore[Id, Item](getId: Item => Id) extends Store[Id, Item] {
    private var store = Map.empty[Id, Item]
    def get(id: Id): NFIO[Item] =
      store.get(id).map(pure).getOrElse(error(s"Id $id not found"))
    def put(item: Item): NFIO[Unit] =
      pure(store = store + (getId(item) -> item))
  }

}
