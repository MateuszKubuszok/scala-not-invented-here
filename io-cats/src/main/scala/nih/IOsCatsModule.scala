package nih

import cats.effect as ce

import scala.annotation.unchecked.uncheckedVariance

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

    override def success[Out](out: Out)(using Position): IO[Any, Nothing, Out] = _ => ce.IO.pure(Right(out))

    override def failure[Err](err: Err)(using Position): IO[Any, Err, Nothing] = _ => ce.IO.pure(Left(err))
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

      // TODO: fork

      // TODO: memoize
    end extension
  end IOMethods

  final override opaque type Fiber[+Err, +Out] = ce.Fiber[ce.IO, Err @uncheckedVariance, Out @uncheckedVariance]
  object Fiber extends FiberModule {

    override def interruptAll(fibers: Iterable[Fiber[Any, Any]]): IO[Any, Nothing, Unit] = ???

    override def joinAll[Err](fibers: Iterable[Fiber[Err, Any]]): IO[Any, Err, Unit] = ???
  }
  given FiberMethods: FiberMethods with
    extension [Err, Out](fiber: Fiber[Err, Out])

      def interrupt: IO[Any, Nothing, Either[Err, Out]] = ???

      def join: IO[Any, Err, Out] = ???
    end extension
  end FiberMethods
}
