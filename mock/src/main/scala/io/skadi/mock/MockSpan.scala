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

package io.skadi.mock

import io.skadi.mock.MockSpan.MockContext
import io.skadi.{Context, Span}

case class MockSpan(id: Long, data: Span.Data, parent: Option[MockContext]) extends Span {
  def update(data: Span.Data): Span = copy(data = data)
  val context: MockSpan.MockContext = MockContext(id, parent)
}

object MockSpan {

  case class MockContext(id: Long, parent: Option[MockContext]) extends Context

  object MockContext {

    def fromMap(map: Map[String, String]): Option[MockContext] =
      map.get("context").flatMap(_.toLongOption).map(MockContext(_, None))

    def toMap(mockContext: MockContext): Map[String, String] =
      Map("context" -> mockContext.id.toString)

  }
}
