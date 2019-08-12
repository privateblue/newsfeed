import cats.effect.IO

import cats.data.EitherT
import cats.instances.either._
import cats.syntax.applicative._

import java8.util.concurrent._
import java8.util.function.BiFunction
import java.util.concurrent.CancellationException

object effect {

  type NFIO[A] = EitherT[IO, String, A]
  def pure[A](a: A): NFIO[A] = a.pure[NFIO]
  def error[A](msg: String): NFIO[A] = EitherT.leftT[IO, A](msg)
  def lift[A](io: IO[A]): NFIO[A] = EitherT.right(io)
  def run[A](nfio: NFIO[A]): Either[String, A] = nfio.value.unsafeRunSync()

  // https://github.com/typelevel/cats-effect/issues/160#issuecomment-509218922
  def fromJavaFuture[A](makeCf: => CompletableFuture[A]): IO[A] =
    IO.cancelable(cb => {
      val cf = makeCf
      cf.handle[Unit](new BiFunction[A, Throwable, Unit] {
        override def apply(result: A, err: Throwable): Unit = {
          err match {
            case null =>
              cb(Right(result))
            case _: CancellationException =>
              ()
            case ex: CompletionException if ex.getCause ne null =>
              cb(Left(ex.getCause))
            case ex =>
              cb(Left(ex))
          }
        }
      })
      IO(cf.cancel(true))
    })

  implicit class CompletableFutureOps[A](f: CompletableFuture[A]) {
    def toIO = fromJavaFuture(f)
  }

}
