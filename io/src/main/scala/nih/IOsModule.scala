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

    // TODO: never

    // TODO: unit

    // TODO: apply

    // TODO: fromEither

    // TODO: fromOption

    // TODO: sequence

    // TODO: raceAll

    // TODO: suspend
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

      // TODO: flatten

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

    extension [Err, Out](fiber: Fiber[Err, Out])

      def interrupt: IO[Any, Nothing, Either[Err, Out]]

      def join: IO[Any, Err, Out]
    end extension
  }

  // TODO: Ref
  
  // TODO: Resource?
}
