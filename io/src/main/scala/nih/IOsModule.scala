package nih

trait IOsModule {

  enum Instantiable[-A]:
    private case Impl
  object Instantiable:
    inline given prove[A](using scala.util.NotGiven[A =:= Nothing]): Instantiable[A] = Impl

  // Position

  type Position

  val Position: PositionModule
  trait PositionModule { this: Position.type =>

    def apply(location: String, file: String, line: Int): Position

    def unapply(position: Position): (String, String, Int)

    inline given instance: Position
  }

  given PositionMethods: PositionMethods
  trait PositionMethods {
    extension (position: Position)
      def location: String
      def file:     String
      def line:     Int
    end extension
  }

  // IO

  type IO[-In, +Err, +Out]

  val IO: IOModule
  trait IOModule { this: IO.type =>

    def success[Out](out: Out)(using Position): IO[Any, Nothing, Out]

    def failure[Err](err: Err)(using Position): IO[Any, Err, Nothing]

    def never(using Position): IO[Any, Nothing, Nothing]

    def fromEither[Err, Out](either: => Either[Err, Out])(using Position): IO[Any, Err, Out]

    def fromOption[Err, Out](option: => Option[Out], onNone: => Err)(using Position): IO[Any, Err, Out]

    def fromTry[Out](tryy: => scala.util.Try[Out])(using Position): IO[Any, Throwable, Out]

    // TODO: sequence

    // TODO: raceAll

    // TODO: suspend

    final def unit(using Position): IO[Any, Nothing, Unit] = success(())

    final def ref[A](a: => A)(using Position): IO[Any, Nothing, Ref[A]] = Ref.of(a)

    final def local[A](a: => A)(using Position): Scope[Any, Nothing, Local[A]] = Local.of(a)
  }

  given IOMethods: IOMethods
  trait IOMethods {
    extension [In, Err, Out](io: IO[In, Err, Out])(using Position)

      // abstract

      def foldIO[In0 <: In, Err2, Out2](
        failure: Err => IO[In0, Err2, Out2],
        success: Out => IO[In0, Err2, Out2]
      )(using Instantiable[Err]): IO[In0, Err2, Out2]

      // TODO: fork

      // TODO: memoize

      // defined using above and module

      def flatMap[In0 <: In, Err2 >: Err, Out2](f: Out => IO[In0, Err2, Out2]): IO[In0, Err2, Out2] =
        foldIO(IO.failure, f)

      def flatMapError[In0 <: In, Err2, Out2 >: Out](f: Err => IO[In0, Err2, Out2])(using
        Instantiable[Err]
      ): IO[In0, Err2, Out2] =
        foldIO(f, IO.success)

      def flip: IO[In, Out, Err] =
        foldIO(err => IO.success(err), out => IO.failure(out))

      def fold[Out2](failure: Err => Out2, success: Out => Out2)(using Instantiable[Err]): IO[In, Nothing, Out2] =
        foldIO(err => IO.success(failure(err)), out => IO.success(success(out)))

      def isSuccess: IO[In, Nothing, Boolean] =
        fold(_ => false, _ => true)

      def isFailure: IO[In, Nothing, Boolean] =
        fold(_ => true, _ => false)

      def map[Out2](f: Out => Out2): IO[In, Err, Out2] =
        flatMap(out => IO.success(f(out)))

      def mapError[Err2](f: Err => Err2)(using Instantiable[Err]): IO[In, Err2, Out] =
        flatMapError(err => IO.failure(f(err)))

      // TODO: mapBoth

      // TODO: flatten/flattenError/flattenBoth

      // TODO: zip

      // TODO: race

      // TODO: repeat

      // TODO: tap
    end extension
  }

  // Fiber

  type Fiber[+Err, +Out]

  val Fiber: FiberModule
  trait FiberModule { this: Fiber.type =>

    def interruptAll(fibers: Iterable[Fiber[Any, Any]]): IO[Any, Nothing, Unit]

    def joinAll[Err](fibers: Iterable[Fiber[Err, Any]]): IO[Any, Err, Unit]
  }

  given FiberMethods: FiberMethods
  trait FiberMethods {
    extension [Err, Out](fiber: Fiber[Err, Out])(using Position)

      def interrupt: IO[Any, Nothing, Unit]

      def join[Err2 >: Err](onCancel: => Err2, onThrow: Throwable => Err2): IO[Any, Err2, Out]
    end extension
  }

  // Ref

  type Ref[A]

  val Ref: RefModule
  trait RefModule { this: Ref.type =>

    def of[A](a: => A)(using Position): IO[Any, Nothing, Ref[A]]
  }

  given RefMethods: RefMethods
  trait RefMethods {
    extension [A](ref: Ref[A])(using Position)

      def get: IO[Any, Nothing, A]

      def modify[B](f: A => (A, B)): IO[Any, Nothing, B]

      def getAndUpdate(f: A => A): IO[Any, Nothing, A] = modify(a => (f(a), a))

      def updateAndGet(f: A => A): IO[Any, Nothing, A] = modify { a =>
        val newA = f(a)
        (newA, newA)
      }
    end extension
  }

  // Local

  type Local[A]

  val Local: LocalModule
  trait LocalModule { this: Local.type =>

    // TODO: fork-join from zio.FibelRef
    def of[A](value: => A)(using Position): Scope[Any, Nothing, Local[A]]

    // TODO: IOGlobal
    def fromCurrentThreadUnsafe[A](local: Local[A])(using Position): A = ???
  }

  given LocalMethods: LocalMethods
  trait LocalMethods {
    extension [A](local: Local[A])(using Position)

      def get: IO[Any, Nothing, A]

      def set(a: A): IO[Any, Nothing, Unit]
    end extension
  }

  // Scope

  // TODO: Resource?
  type Scope[-In, +Err, +Out]

  val Scope: ScopeModule
  trait ScopeModule { this: Scope.type =>

  }

  given ScopeMethods: ScopeMethods
  trait ScopeMethods {
    extension[In, Err, Out] (scope: Scope[In, Err, Out])(using Position)

      def todo: Unit = ()
    end extension
  }

  // TODO: Stream
}
