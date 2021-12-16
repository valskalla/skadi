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
  * Details are implementation-dependent, but all implementations must be referentially-transparent on updating Span data
  */
trait Span {

  /**
    * Span information, such as name, tags etc
    */
  def data: Span.Data

  /**
    * Span context is an opaque container that is set by implementation. Usually appears at the edge of application
    */
  def context: Context

  /**
    * Update span information and return back that span
    */
  def update(data: Span.Data): Span

  /**
    * Returns current span name
    */
  def name: String = data.name

  /**
    * Returns current span tags
    */
  def tags: Map[String, Tag] = data.tags

  /**
    * Returns current span logs
    */
  def logs: List[TraceLog] = data.logs

  /**
    * Returns current span exception if set
    */
  def exception: Option[Throwable] = data.exception

  /**
    * Returns span start time
    */
  def startTime: Instant = data.startTime

  /**
    * Returns span finished time if set
    */
  def stopTime: Option[Instant] = data.stopTime

  /**
    * Returns span with the new name set
    */
  def withName(name: String): Span = update(data.copy(name = name))

  /**
    * Returns span with new tag added. Overwrites existing tag if it already exists under that name
    */
  def withTag(name: String, tag: Tag): Span = update(data.copy(tags = data.tags.updated(name, tag)))

  /**
    * Returns span with new tags added. Overwrites existing tags if some already exist with the same names
    */
  def withTags(tags: (String, Tag)*): Span = update(data.copy(tags = data.tags ++ tags))

  /**
    * Returns span with new log recorded
    */
  def withLog(traceLog: TraceLog): Span = update(data.copy(logs = traceLog :: data.logs))

  /**
    * Returns span with new logs recorded
    */
  def withLogs(traceLogs: List[TraceLog]): Span = update(data.copy(logs = traceLogs ::: data.logs))

  /**
    * Mark span with an exception, assuming it's a failed one
    */
  def withException(e: Throwable): Span = update(data.copy(exception = Some(e)))

  /**
    * Set stop time for the span. Should be done automatically by the tracer
    */
  def withStopTime(time: Instant): Span = update(data.copy(stopTime = Some(time)))

}

object Span {

  case object Noop extends Span {
    val data: Data = Data("noop", Map.empty, List.empty, Instant.EPOCH, None, None)

    val context: Context = Context.Empty

    def update(data: Data): Span = Noop
  }

  case class Data(
      name: String,
      tags: Map[String, Tag],
      logs: List[TraceLog],
      startTime: Instant,
      exception: Option[Throwable],
      stopTime: Option[Instant]
  )

}
