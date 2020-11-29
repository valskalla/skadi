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
import java.time.Instant

/**
  * Basic interface for span.
  * Completely implementation-dependent, and doesn't guarantee referential transparency out of the box.
  *
  * See implementations-related documentation for details on ref transparency.
  */
trait Span {

  /**
    * Return span with the new name set
    */
  def withName(name: String): Span

  /**
    * Return span with new tag added. Overwrites existing tag if it already exists under that name
    */
  def withTag(name: String, tag: Tag): Span

  /**
    * Return span with new tags added. Overwrites existing tags if some already exists with same names
    */
  def withTags(tags: (String, Tag)*): Span

  /**
    * Return span with new log recorded
    */
  def withLog(traceLog: TraceLog): Span

  /**
    * Return span with new logs recorded
    */
  def withLogs(traceLogs: List[TraceLog]): Span

  /**
    * Mark span with an exception, assuming it's a failed one
    */
  def withException(e: Throwable): Span

  /**
    * Set stop time for the span. Should be done automatically by the tracer
    */
  def withStopTime(time: Instant): Span

  /**
    * Retrieve span's context. See [[Context]] for details
    */
  def context: Context

}

object Span {

  // $COVERAGE-OFF$
  val noop: Span = new Span {
    def withName(name: String): Span = this
    def withTag(name: String, tag: Tag): Span = this
    def withTags(tags: (String, Tag)*): Span = this
    def withLog(traceLog: TraceLog): Span = this
    def withLogs(traceLogs: List[TraceLog]): Span = this
    def withException(e: Throwable): Span = this
    def withStopTime(time: Instant): Span = this
    def context: Context = Context.noop
  }
  // $COVERAGE-ON$

}
