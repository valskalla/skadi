package io.skadi.examples.http4s

import cats.Monad
import cats.effect.Sync
import cats.syntax.all._
import io.skadi.{Tag, Trace}
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router

class HelloWorldApp[F[_]: Monad: Sync](client: HelloWorldClient[F])(implicit trace: Trace[F]) extends Http4sDsl[F] {

  def routes: HttpRoutes[F] = Router("/" -> {
    HttpRoutes.of[F] {
      case GET -> Root / "hello" / name =>
        Ok {
          name match {
            case "self" => "Self-call".pure[F]
            case _ =>
              trace.setTag("name", Tag.string(name)) >> client.call.as(s"Hello, $name")
          }
        }
    }
  })

}
