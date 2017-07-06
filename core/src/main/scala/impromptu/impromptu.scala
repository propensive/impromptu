/* Impromptu, version 1.0.0. Copyright 2017 Jon Pretty, Propensive Ltd.
 *
 * The primary distribution site is: http://co.ntextu.al/
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 */
package impromptu

import scala.util.Try
import scala.concurrent._
import scala.language.implicitConversions

object Async {
  def apply[Return](action: => Return)(implicit execCtx: ExecutionContext): Async[Return, _] =
    new Async[Return, Nothing](Seq(), env => action)
  
  def after[Before <: Async[_, _], Return](deps: Dependency[Before]*)(
      action: Env[Before] => Return)(implicit execCtx: ExecutionContext): Async[Return, Before] =
    new Async[Return, Before](deps.map(_.async), action)

  implicit def autoWrap(async: Async[_, _]): Dependency[async.type] = Dependency(async)
  
  final case class Dependency[-A <: Async[_, _]] private (async: Async[_, _])
  final case class Env[+Before] private (values: Map[Async[_, _], Future[_]])
}

class Async[+Return, Before] private (val deps: Seq[Async[_, _]],
    val action: Async.Env[Before] => Return)(implicit execCtx: ExecutionContext) {
  
  def apply()(implicit env: Async.Env[this.type]): Return =
    env.values(this).value.get.get.asInstanceOf[Return]

  lazy val future: Future[Return] = {
    val results: Seq[(Async[_, _], Future[_])] = deps.map { d => (d, d.future) }
    Future.sequence(results.map(_._2)).map { _ => action(Async.Env(results.toMap)) }
  }

  def await(): Return = Await.result(future, duration.Duration.Inf)
}

