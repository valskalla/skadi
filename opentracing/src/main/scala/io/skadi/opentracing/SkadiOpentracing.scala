/*
 * Copyright (c) 2020 Sergey Kolbasov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.skadi.opentracing

import cats.effect.Sync
import io.opentracing.{Tracer => OpenTracer}
import io.skadi.opentracing.impl.OpentracingTracer
import io.skadi.{Trace, _}

/**
  * Abstraction over `io.opentracing.Tracer`. Should work with any implementation of `io.opentracing.Tracer`, would it be
  * Jaeger or Lightstep.
  *
  * IMPORTANT NOTICE.
  *
  * Underlying OpenTracing API mixes up data & behavior by providing `Span` with `finish()` operation and explicitly
  * stating that any operations with span after it's finished would cause an undefined behavior.
  *
  * That decision has a far reaching consequences for any abstraction built on top of OpenTracing API.
  *
  * It's impossible to create a child span without providing implementation-dependant `io.opentracing.SpanContext`, therefore:
  *  - OpenTracing Span can't be created lazily on report. It must be initialized right away, so children could get a context
  *  - Span reporting is not referentially-transparent. After span is finished, undefined behavior appears.
  *    Please, avoid re-using the same instance of `io.skadi.Span` for multiple operations, as it encapsulates running
  *    `io.opentracing.Span` inside, and therefore might be non-referentially transparent (implementation-dependent).
  *
  * Best practice of avoiding shooting in one's leg is to behave & use API provided by `io.skadi.Tracer`.
  * Skadi in general is doing its best to keep everything lawful & functional, but to the limits.
  *
  * Another fancy way to catch a runtime exception is to provide `io.skadi.Span` that was built
  * outside of [[SkadiOpentracing#tracer]] or [[SkadiOpentracing#traceCarrier]].
  * Please, don't do it as again everything depends on vendor implementation of io.opentracing.Tracer`.
  */
case class SkadiOpentracing[F[_]](private val openTracer: OpenTracer)(implicit F: Sync[F], clock: TracerClock[F], trace: Trace[F]) {
  self =>

  /**
    * Create an instance of `io.skadi.Tracer` on top of provided `io.opentracing.Tracer`
    */
  def tracer: Tracer[F] = new OpentracingTracer(openTracer)

  /**
    * Returns builder of `io.skadi.TraceCarrier` on top of provided `io.opentracing.Tracer`
    */
  def traceCarrier: TraceCarrierBuilder[F] = new TraceCarrierBuilder(openTracer)

}
