package nih

trait IOsModule {

  // Position

  // TODO: pass

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

    def success[Out](out: Out): IO[Any, Nothing, Out]

    def failure[Err](err: Err): IO[Any, Err, Nothing]
  }

  given IOMethods: IOMethods
  trait IOMethods {

    extension [In, Err, Out](io: IO[In, Err, Out])(using Position)

      // TODO: CanFail[E]

      // abstract

      def flatMap[In0 <: In, Err2 >: Err, Out2](f: Out => IO[In0, Err2, Out2]): IO[In0, Err2, Out2]

      def flatMapError[In0 <: In, Err2, Out2 >: Out](f: Err => IO[In0, Err2, Out2]): IO[In0, Err2, Out2]

      def foldIO[In0 <: In, Err2, Out2](
        failure: Err => IO[In0, Err2, Out2],
        success: Out => IO[In0, Err2, Out2]
      ): IO[In0, Err2, Out2]

      // TODO: memoize

      // defined using above and module

      def flip: IO[In, Out, Err] =
        foldIO(err => IO.success(err), out => IO.failure(out))

      def fold[Out2](failure: Err => Out2, success: Out => Out2): IO[In, Nothing, Out2] =
        foldIO(err => IO.success(failure(err)), out => IO.success(success(out)))

      def isSuccess: IO[In, Nothing, Boolean] =
        fold(_ => false, _ => true)

      def isFailure: IO[In, Nothing, Boolean] =
        fold(_ => true, _ => false)

      def map[Out2](f: Out => Out2): IO[In, Err, Out2] =
        flatMap(out => IO.success(f(out)))

      def mapError[Err2](f: Err => Err2): IO[In, Err2, Out] =
        flatMapError(err => IO.failure(f(err)))
    end extension
  }

  // TODO: Fiber
}
