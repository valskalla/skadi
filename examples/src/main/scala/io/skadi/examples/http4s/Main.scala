package io.skadi.examples.http4s

import cats.effect.{ExitCode, Resource}
import io.skadi.{Span, StatefulTrace, TraceCarrier, Tracer, TracerClock}
import io.skadi.monix.initTaskStatefulTrace
import monix.eval.{Task, TaskApp}
import monix.execution.Scheduler
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
      implicit0(trace: StatefulTrace[Task]) <- Resource.liftF(initTaskStatefulTrace[Option[Span]](None))
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
