package io.skadi.examples.http4s

import cats.effect.{ExitCode, Resource}
import io.skadi.monix.initTaskTrace
import io.skadi._
import _root_.monix.eval.{Task, TaskApp}
import _root_.monix.execution.Scheduler
import org.http4s.Headers
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext

object Main extends TaskApp with Http4sDsl[Task] {

  override protected def scheduler: Scheduler = Scheduler.traced

  implicit val tracerClock: TracerClock[Task] = TracerClock.create[Task]

  def run(args: List[String]): Task[ExitCode] =
    (for {
      implicit0(trace: Trace[Task]) <- Resource.liftF(initTaskTrace[Option[SpanRef[Task]]](None))
      implicit0(tracer: Tracer[Task] with TraceCarrier[Task, Headers]) <- TracerResource.allocate
      client <- BlazeClientBuilder[Task](ExecutionContext.global).resource
      helloWorldClient = new HelloWorldClient[Task](client)
      helloWorldApp = new HelloWorldApp[Task](helloWorldClient)
      _ <- BlazeServerBuilder[Task](ExecutionContext.global)
        .bindHttp(8080)
        .withHttpApp {
          Router("/" -> TraceMiddleware(helloWorldApp.routes)).orNotFound
        }
        .resource
    } yield ()).use(_ => Task.never[ExitCode])
}
