package nih

import cats.effect as ce
import cats.implicits.catsSyntaxFlatMapOps
import cats.syntax.either.*

import scala.annotation.unchecked.uncheckedVariance
import scala.util.{Failure, Success}

class IOsCatsModule extends IOsModule {

  override opaque type Position = (String, String, Int)
  object Position extends PositionModule {

    override def apply(location: String, file: String, line: Int): Position = (location, file, line)

    override def unapply(position: Position): (String, String, Int) = position

    inline given instance: Position = (
      sourcecode.Pkg.generate.value,
      sourcecode.File.generate.value,
      sourcecode.Line.generate.value
    )
  }
  given PositionMethods: PositionMethods with
    extension (position: Position)
      def location: String = position._1
      def file:     String = position._2
      def line:     Int    = position._3
    end extension
  end PositionMethods

  final override opaque type IO[-In, +Err, +Out] = In => ce.IO[Either[Err, Out]]
  object IO extends IOModule {

    // TODO: use Position, setup Context propagation and ThreadLocal
    private inline def liftIOEither[Err, Out](inline io: ce.IO[Either[Err, Out]]): IO[Any, Err, Out] = _ => io

    override def success[Out](out: Out)(using Position): IO[Any, Nothing, Out] = liftIOEither(ce.IO.pure(Right(out)))

    override def failure[Err](err: Err)(using Position): IO[Any, Err, Nothing] = liftIOEither(ce.IO.pure(Left(err)))

    override def never(using Position): IO[Any, Nothing, Nothing] = liftIOEither(ce.IO.never)

    override def fromEither[Err, Out](either: => Either[Err, Out])(using Position): IO[Any, Err, Out] = liftIOEither(
      ce.IO.pure(either)
    )

    override def fromOption[Err, Out](option: => Option[Out], onNone: => Err)(using Position): IO[Any, Err, Out] =
      liftIOEither(
        option match
          case Some(value) => ce.IO.pure(Right(value))
          case None        => ce.IO.pure(Left(onNone))
      )

    override def fromTry[Out](tryy: => scala.util.Try[Out])(using Position): IO[Any, Throwable, Out] = liftIOEither(
      tryy match
        case Failure(exception) => ce.IO.pure(Left(exception))
        case Success(value)     => ce.IO.pure(Right(value))
    )
  }
  given IOMethods: IOMethods with
    extension [In, Err, Out](io: IO[In, Err, Out])(using Position)
      def foldIO[In0 <: In, Err2, Out2](
        failure: Err => IO[In0, Err2, Out2],
        success: Out => IO[In0, Err2, Out2]
      )(using Instantiable[Err]): IO[In0, Err2, Out2] = in =>
        io(in).flatMap {
          case Left(err)  => failure(err)(in)
          case Right(out) => success(out)(in)
        }

      def fork: IO[In, Nothing, Fiber[Err, Out]] = in =>
        io(in)
          .flatMap {
            case Left(error)  => ce.IO.raiseError(FiberError(error))
            case Right(value) => ce.IO.pure(value)
          }
          .start
          .map(_.asRight[Nothing])

      // ???
      def memoize: IO[Any, Nothing, IO[In, Err, Out]] = ???
    end extension
  end IOMethods

  private case class FiberError[E](e: E) extends Throwable with scala.util.control.NoStackTrace

  final override opaque type Fiber[+Err, +Out] = ce.FiberIO[Out @uncheckedVariance]
  object Fiber extends FiberModule {

    override def interruptAll(fibers: Iterable[Fiber[Any, Any]]): IO[Any, Nothing, Unit] = ???

    override def joinAll[Err](fibers: Iterable[Fiber[Err, Any]]): IO[Any, Err, Unit] = ???
  }
  given FiberMethods: FiberMethods with
    extension [Err, Out](fiber: Fiber[Err, Out])(using Position)

      def interrupt: IO[Any, Nothing, Unit] = _ => fiber.cancel.map(_.asRight[Nothing])

      def join[Err2 >: Err](onCancel: => Err2, onThrow: Throwable => Err2): IO[Any, Err2, Out] = _ =>
        fiber.join.flatMap[Either[Err2, Out]] {
          case ce.Outcome.Canceled()                 => ce.IO(onCancel.asLeft[Out])
          case ce.Outcome.Errored(FiberError(error)) => ce.IO.pure(error.asInstanceOf[Err2].asLeft[Out])
          case ce.Outcome.Errored(error)             => ce.IO.pure(onThrow(error).asLeft[Out])
          case ce.Outcome.Succeeded(io)              => io.map(_.asInstanceOf[Out].asRight[Err2])
        }
    end extension
  end FiberMethods

  final override type Ref[A] = ce.Ref[ce.IO, A]
  object Ref extends RefModule {

    override def of[A](a: => A)(using Position): IO[Any, Nothing, Ref[A]] = _ => ce.IO.ref(a).map(_.asRight[Nothing])
  }
  given RefMethods: RefMethods with
    extension [A](ref: Ref[A])(using Position)

      def get: IO[Any, Nothing, A] = _ => ref.get.map(_.asRight[Nothing])

      def modify[B](f: A => (A, B)): IO[Any, Nothing, B] = _ => ref.modify(f).map(_.asRight[Nothing])
    end extension
  end RefMethods

  final override type Local[A] = ce.IOLocal[A]
  object Local extends LocalModule {

    override def of[A](value: => A)(using Position): Scope[Any, Nothing, Local[A]] = _ =>
      ce.Resource.eval(ce.IOLocal(value)).map(_.asRight[Nothing])
  }
  given LocalMethods: LocalMethods with
    extension [A](local: Local[A])(using Position)

      def get: IO[Any, Nothing, A] = _ => local.get.map(_.asRight[Nothing])

      def set(a: A): IO[Any, Nothing, Unit] = _ => local.set(a).map(_.asRight[Nothing])
    end extension
  end LocalMethods

  final override type Scope[-In, +Err, +Out] = In => ce.Resource[ce.IO, Either[Err, Out]]
  object Scope extends ScopeModule {}
  given ScopeMethods: ScopeMethods with
    def nope(): Unit = ()
  end ScopeMethods
}
