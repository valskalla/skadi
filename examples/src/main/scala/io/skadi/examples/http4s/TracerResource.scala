package io.skadi.examples.http4s

import cats.effect.Resource
import io.jaegertracing.Configuration
import io.skadi.opentracing.SkadiOpentracing
import io.skadi.{AsCarrier, Trace, TraceCarrier, Tracer, TracerClock}
import monix.eval.Task
import org.http4s.{Header, Headers}

object TracerResource {

  /**
    * Creates `io.skadi.Tracer` resource on top of Jaeger tracing client with constant sampler that always reports
    * traces
    */
  def allocate(
      implicit trace: Trace[Task],
      clock: TracerClock[Task]
  ): Resource[Task, Tracer[Task] with TraceCarrier[Task, Headers]] = {
    val acquire = Task.eval {
      new Configuration("skadi-example")
        .withSampler(new Configuration.SamplerConfiguration().withType("const").withParam(1))
        .getTracer
    }

    Resource.fromAutoCloseable(acquire).map(tracer => SkadiOpentracing[Task](tracer).tracer.throughHttpHeaders[Headers])
  }

  /**
    * AsCarrier defines how to convert from `Map[String, String]` to `Headers` and back, required by OpenTracing
    */
  implicit private val asCarrierHeaders: AsCarrier[Map[String, String], Headers] =
    new AsCarrier[Map[String, String], Headers] {
      def from(map: Map[String, String]): Headers =
        Headers {
          map.map {
            case (name, value) => Header(name, value)
          }.toList
        }

      def to(carrier: Headers): Map[String, String] =
        carrier.toList.map(header => header.name.value -> header.value).toMap
    }
}
