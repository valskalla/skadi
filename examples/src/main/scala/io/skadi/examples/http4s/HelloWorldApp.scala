package io.skadi.examples.http4s

import cats.Monad
import cats.effect.Sync
import cats.syntax.all._
import io.skadi.{StatefulTrace, Tag, Trace, Tracer}
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router

class HelloWorldApp[F[_]: StatefulTrace: Trace: Monad: Sync](client: HelloWorldClient[F])(implicit tracer: Tracer[F])
    extends Http4sDsl[F] {

  def routes: HttpRoutes[F] = Router("/" -> {
    HttpRoutes.of[F] {
      case GET -> Root / "hello" / name =>
        Ok {
          name match {
            case "self" => "Self-call".pure[F]
            case _ =>
              tracer.setTag("name", Tag.string(name)) >> client.call.as(s"Hello, $name")
          }
        }
    }
  })

}
