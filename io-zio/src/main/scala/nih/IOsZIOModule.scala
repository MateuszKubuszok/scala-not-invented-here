package nih

import scala.annotation.unchecked.uncheckedVariance

class IOsZIOModule extends IOsModule {

  private val cacheCanFail:             Conversion[Instantiable[Nothing], zio.CanFail[Any]] = _ => zio.CanFail
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

    override def never(using Position): IO[Any, Nothing, Nothing] = zio.ZIO.never

    override def fromEither[Err, Out](either: => Either[Err, Out])(using Position): IO[Any, Err, Out] =
      zio.ZIO.fromEither(either)

    override def fromOption[Err, Out](option: => Option[Out], onNone: => Err)(using Position): IO[Any, Err, Out] =
      zio.ZIO.fromOption(option).mapError(_ => onNone)

    override def fromTry[Out](tryy: => scala.util.Try[Out])(using Position): IO[Any, Throwable, Out] =
      zio.ZIO.fromTry(tryy)
  }
  given IOMethods: IOMethods with
    extension [In, Err, Out](io: IO[In, Err, Out])(using Position)
      def foldIO[In0 <: In, Err2, Out2](
        failure: Err => IO[In0, Err2, Out2],
        success: Out => IO[In0, Err2, Out2]
      )(using Instantiable[Err]): IO[In0, Err2, Out2] = io.foldZIO(failure, success)

      def fork: IO[In, Nothing, Fiber[Err, Out]] = io.fork

      def memoize: IO[Any, Nothing, IO[In, Err, Out]] = io.memoize
    end extension
  end IOMethods

  final override opaque type Fiber[+Err, +Out] = zio.Fiber[Err @uncheckedVariance, Out @uncheckedVariance]
  object Fiber extends FiberModule {

    override def interruptAll(fibers: Iterable[Fiber[Any, Any]]): IO[Any, Nothing, Unit] = ???

    override def joinAll[Err](fibers: Iterable[Fiber[Err, Any]]): IO[Any, Err, Unit] = ???
  }
  given FiberMethods: FiberMethods with
    extension [Err, Out](fiber: Fiber[Err, Out])(using Position)

      def interrupt: IO[Any, Nothing, Unit] = fiber.interrupt.ignore

      def join[Err2 >: Err](onCancel: => Err2, onThrow: Throwable => Err2): IO[Any, Err2, Out] = ???
    end extension
  end FiberMethods

  final override type Ref[A] = zio.Ref[A]
  object Ref extends RefModule {

    override def of[A](a: => A)(using Position): IO[Any, Nothing, Ref[A]] = zio.Ref.make(a)
  }
  given RefMethods: RefMethods with
    extension [A](ref: Ref[A])(using Position)

      def get: IO[Any, Nothing, A] = ref.get

      def modify[B](f: A => (A, B)): IO[Any, Nothing, B] = ref.modify(f(_).swap)
    end extension
  end RefMethods

  final override type Local[A] = zio.FiberRef[A]
  object Local extends LocalModule {

    override def of[A](value: => A)(using Position): Scope[Any, Nothing, Local[A]] = zio.FiberRef.make(value)
  }
  given LocalMethods: LocalMethods with
    extension [A](local: Local[A])(using Position)

      def get: IO[Any, Nothing, A] = local.get

      def set(a: A): IO[Any, Nothing, Unit] = local.set(a)
    end extension
  end LocalMethods

  final override type Scope[-In, +Err, +Out] = zio.ZIO[In & zio.Scope, Err, Out]
  object Scope extends ScopeModule {}
  given ScopeMethods: ScopeMethods with
    def nope(): Unit = ()
  end ScopeMethods
}
