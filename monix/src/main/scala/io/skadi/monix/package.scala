package io.skadi

import _root_.monix.eval._

package object monix {

  /**
    * Safe initialization of `StatefulTrace` type class of Monix Task that relies on `TaskLocal` mechanism.
    * Check corresponding documentation before using `https://monix.io/docs/current/execution/local.html`.
    *
    * It's very important to keep in mind that `TaskLocal` doesn't provide auto-isolation out of the box
    * for concurrent processes!
    *
    * Until `https://github.com/monix/monix/issues/1302` is resolved, one must be very careful mangling with
    * same span in race conditions, such as parallel/concurrent evaluation.
    *
    * It's generally recommended to create separate & isolated spans for concurrent processes instead of sharing the
    * same state between multiple threads/fibers. Another option is to run `TaskLocal.isolate` explicitly, but it's not
    * for the faint of heart.
    *
    * Hint: to drastically improve Quality of Life when dealing with implicits inside of for-yields, take a look at
    * this useful Scala compiler plugin: `https://github.com/oleg-py/better-monadic-for`
    *
    * @param init Initial value of environment (state) that keeps span
    */
  def initTaskStatefulTrace[Env: HasSpan](init: => Env): Task[StatefulTrace[Task]] =
    TaskLocal.lazyDefault(Coeval(init)).map(taskStatefulTrace[Env])

  private def taskStatefulTrace[Env](local: TaskLocal[Env])(implicit hasSpan: HasSpan[Env]): StatefulTrace[Task] =
    new StatefulTrace[Task] {
      def setSpan(span: Span): Task[Unit] =
        local.read.flatMap(env => local.write(hasSpan.set(Some(span), env)))

      def getSpan: Task[Option[Span]] =
        local.read.map(hasSpan.get)

      def withSpan[A](span: Span)(fa: Task[A]): Task[A] =
        local.read.flatMap(env => local.bind(hasSpan.set(Some(span), env))(fa))

      def modifySpan(fn: Span => Span): Task[Unit] = local.read.flatMap { env =>
        hasSpan.get(env) match {
          case Some(span) => setSpan(fn(span))
          case _          => Task.unit
        }
      }
    }

}
