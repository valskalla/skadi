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

import scala.collection.JavaConverters._

package object opentracing {

  private[skadi] def jmapToScala[K, V](hashMap: java.util.Map[K, V]): Map[K, V] =
    hashMap.asScala.toMap

  private[skadi] def jEntiryIterableToScalaMap[K, V](iter: java.lang.Iterable[java.util.Map.Entry[K, V]]): Map[K, V] =
    iter.asScala.map(x => x.getKey() -> x.getValue()).toMap

  private[skadi] def smapToJava[K, V](map: Map[K, V]): java.util.Map[K, V] =
    map.asJava

  private[skadi] def jlistToScala[V](list: java.util.List[V]): List[V] =
    list.asScala.toList

}
