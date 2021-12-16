package io.skadi

import _root_.monix.eval._

package object monix {

  /**
    * Safe initialization of `Trace` type class of Monix Task that relies on `TaskLocal` mechanism.
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
  def initTaskTrace[Env](init: => Env)(implicit hasSpan: HasSpan[Task, Env]): Task[Trace[Task]] =
    TaskLocal.lazyDefault(Coeval(init)).map(taskStatefulTrace[Env])

  private def taskStatefulTrace[Env](local: TaskLocal[Env])(implicit hasSpan: HasSpan[Task, Env]): Trace[Task] =
    new Trace[Task] {

      def getSpan: Task[Option[SpanRef[Task]]] =
        local.read.map(hasSpan.get)

      def withSpan[A](span: SpanRef[Task])(fa: Task[A]): Task[A] =
        local.read.flatMap(env => local.bind(hasSpan.set(Some(span), env))(fa))

      def modifySpan(fn: SpanRef[Task] => Task[Unit]): Task[Unit] = local.read.flatMap { env =>
        hasSpan.get(env) match {
          case Some(ref) => fn(ref)
          case _         => Task.unit
        }
      }
    }

}
