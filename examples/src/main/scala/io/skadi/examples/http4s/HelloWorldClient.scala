package io.skadi.examples.http4s

import cats.Monad
import cats.syntax.all._
import io.skadi.{TraceCarrier, Tracer}
import org.http4s.client.Client
import org.http4s.implicits._
import org.http4s.{EntityDecoder, Headers, Request}

class HelloWorldClient[F[_]: Monad](client: Client[F])(
    implicit tracer: Tracer[F],
    traceCarrier: TraceCarrier[F, Headers],
    ed: EntityDecoder[F, String]
) {

  def call: F[Unit] =
    tracer.trace("call-self") {
      traceCarrier.getCarrier.flatMap { maybeContext =>
        val req =
          Request[F](uri = uri"http://localhost:8080/hello/self", headers = maybeContext.getOrElse(Headers.empty))
        client.expect[String](req).void
      }
    }

}
