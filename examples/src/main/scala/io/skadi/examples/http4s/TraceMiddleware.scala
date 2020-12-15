package io.skadi.examples.http4s

import cats.Monad
import cats.data.{Kleisli, OptionT}
import io.skadi.{TraceCarrier, Tracer}
import org.http4s.{Headers, HttpRoutes, Request}

/**
  * This middleware extracts span context from the headers if present, and set it as the parent of the "hello-world" span
  */
object TraceMiddleware {

  def apply[F[_]: Monad](
      service: HttpRoutes[F]
  )(implicit tracer: Tracer[F], traceCarrier: TraceCarrier[F, Headers]): HttpRoutes[F] = Kleisli { req: Request[F] =>
    OptionT {
      tracer.continueFrom(req.headers).trace("hello-world") {
        service.run(req).value
      }
    }
  }

}
