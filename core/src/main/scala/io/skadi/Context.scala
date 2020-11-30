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

/**
  * Opaque value encapsulated inside of `Span`. Completely implementation-dependent and usually exposed to user
  * on application boundaries to run traces across processes.
  */
trait Context

// $COVERAGE-OFF$
object Context extends Context {
  val empty: Context = Context
}
// $COVERAGE-ON$
