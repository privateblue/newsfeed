import context._

import cats.data.{EitherT, Kleisli}
import cats.instances.either._
import cats.syntax.applicative._

import cats.effect.IO

import java8.util.concurrent._
import java8.util.function.BiFunction
import java.util.concurrent.CancellationException

object effects {

  type NFIO[A] = EitherT[Kleisli[IO, AppContext, ?], String, A]

  def pure[A](a: A): NFIO[A] =
    a.pure[NFIO]

  def error[A](msg: String): NFIO[A] =
    EitherT.leftT[Kleisli[IO, AppContext, ?], A](msg)

  def erroring[A](err: Either[String, A]): NFIO[A] =
    EitherT.fromEither[Kleisli[IO, AppContext, ?]](err)

  def ask: NFIO[AppContext] =
    EitherT.right(Kleisli.ask[IO, AppContext])

  def run[A](nfio: NFIO[A], context: AppContext): Either[String, A] =
    nfio.value.run(context).unsafeRunSync()

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
