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

package io.skadi

import cats.~>

/**
  * Abstraction over extraction of span's context from some carrier (i.e. HTTP headers),
  * as well as creating a carrier from the current span's context (if any).
  *
  * Usually kept at the application boundaries to extract information from the input and continue trace by adding
  * a child span, or to inject span into carrier and pass it further to another process
  */
trait TraceCarrier[F[_], Carrier] {
  def fromCarrier(carrier: Carrier): F[Option[Context]]
  def getCarrier: F[Option[Carrier]]
}

object TraceCarrier {

  implicit class TraceCarrierOps[F[_], Carrier](traceCarrier: TraceCarrier[F, Carrier]) {
    def mapK[G[_]](nat: F ~> G): TraceCarrier[G, Carrier] = new TraceCarrier[G, Carrier] {
      def fromCarrier(carrier: Carrier): G[Option[Context]] = nat(traceCarrier.fromCarrier(carrier))

      def getCarrier: G[Option[Carrier]] = nat(traceCarrier.getCarrier)
    }
  }

}
