package nih

import scala.annotation.unchecked.uncheckedVariance

class IOsZIOModule extends IOsModule {

  private val cacheCanFail:           Conversion[Instantiable[Nothing], zio.CanFail[Any]] = _ => zio.CanFail
  private inline given proveCanFail[E]: Conversion[Instantiable[E], zio.CanFail[E]]         = cacheCanFail

  override opaque type Position = zio.Trace
  object Position extends PositionModule {

    override def apply(location: String, file: String, line: Int): Position = zio.Trace(location, file, line)

    override def unapply(position: Position): (String, String, Int) = zio.Trace.unapply(position).getOrElse(???)

    inline given instance: Position = zio.internal.stacktracer.Tracer.newTrace.asInstanceOf[Position]
  }
  given PositionMethods: PositionMethods with
    extension (position: Position)
      def location: String = Position.unapply(position)._1
      def file:     String = Position.unapply(position)._2
      def line:     Int    = Position.unapply(position)._3
    end extension
  end PositionMethods

  final override opaque type IO[-In, +Err, +Out] = zio.ZIO[In, Err, Out]
  object IO extends IOModule {

    override def success[Out](out: Out)(using Position): IO[Any, Nothing, Out] = zio.ZIO.succeed(out)

    override def failure[Err](err: Err)(using Position): IO[Any, Err, Nothing] = zio.ZIO.fail(err)
  }
  given IOMethods: IOMethods with
    extension [In, Err, Out](io: IO[In, Err, Out])(using Position)
      def foldIO[In0 <: In, Err2, Out2](
        failure: Err => IO[In0, Err2, Out2],
        success: Out => IO[In0, Err2, Out2]
      )(using Instantiable[Err]): IO[In0, Err2, Out2] = io.foldZIO(failure, success)

      // TODO: fork

      // TODO: memoize
    end extension
  end IOMethods

  final override opaque type Fiber[+Err, +Out] = zio.Fiber[Err@uncheckedVariance, Out@uncheckedVariance]
  object Fiber extends FiberModule {

    override def interruptAll(fibers: Iterable[Fiber[Any, Any]]): IO[Any, Nothing, Unit] = ???

    override def joinAll[Err](fibers: Iterable[Fiber[Err, Any]]): IO[Any, Err, Unit] = ???
  }
  given FiberMethods: FiberMethods with
    extension[Err, Out] (fiber: Fiber[Err, Out])

      def interrupt: IO[Any, Nothing, Either[Err, Out]] = ???

      def join: IO[Any, Err, Out] = ???
    end extension
  end FiberMethods
}
