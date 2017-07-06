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

/** factory object for [[Async]] instances */
object Async {

  /** creates a new [[Async]] instance */
  def apply[Return](action: => Return)(implicit execCtx: ExecutionContext): Async[Return, _] =
    new Async[Return, Nothing](Seq(), env => action)
  
  /** constructs a new asynchronous computation which should run after the specified prerequisites
    * 
    * @param dependencies  the prerequisite dependencies of the computation
    * @param action        the computation to run
    * @param execCtx       the execution context in which to run the computation
    * @tparam Before  the intersection type of the singleton types of the dependencies
    * @tparam Return  the type of the result of the computation
    * @return a newly-constructed, unevaluated [[Async]] instance
    */
  def after[Before <: Async[_, _], Return](dependencies: Dependency[Before]*)(
      action: Env[Before] => Return)(implicit execCtx: ExecutionContext): Async[Return, Before] =
    new Async[Return, Before](dependencies.map(_.async), action)

  /** automatically wraps any [[Async]] values in the contravariant [[Dependency]] type to ensure
    * that their type parameters infer to the intersection type
    *
    * @param async  the [[Async]] value to wrap
    * @return the [[Async]] value wrapped as a [[Dependency]] */
  implicit def autoWrap(async: Async[_, _]): Dependency[async.type] = Dependency(async)
  
  /** wrapper class for [[Async]] values when used in parameter positions where an intersection
    * type needs to be inferred
    *
    * @param async  the [[Async]] value to be wrapped
    * @tparam A  the singleton type of the [[Async]] value
    */
  final case class Dependency[-A <: Async[_, _]] private (async: Async[_, _])

  /** the environment in which the computation is executed, encapsulating all prior state
    */
  final case class Env[+Before] private ()

  /** the single [[Env]] instance which will work for all cases
    */
  private final val AnyEnv: Env[Nothing] = Env[Nothing]()
}

/** an asynchronously computed value, much like a lazy [[Future]]
  * 
  * @param dependencies  the dependencies which must be computer prior to this [[Async]] value
  * @param action        the action which computes the value
  * @tparam Return  the type of the value to be computed
  * @tparam Before  an intersection type of the singleton types of all the dependencies of this
  *                 [[Async]] value
  */
final class Async[+Return, Before] private (val dependencies: Seq[Async[_, _]],
    val action: Async.Env[Before] => Return)(implicit execCtx: ExecutionContext) {
  
  /** returns the precomputed result of the previous dependent value, without blocking
    *
    * @param env  the implicit environment whose presence in-scope guarantees the value may be
    *             accessed without blocking
    * @return the precomputed value from the earlier asynchronous computation
    */
  def apply()(implicit env: Async.Env[this.type]): Return = future.value.get.get

  /** the lazily-evaluated [[Future]] corresponding to this asynchronous value */
  lazy val future: Future[Return] =
    Future.sequence(dependencies.map(_.future)).map { _ => action(Async.AnyEnv) }

  /** returns the result from evaluating the future, blocking if necessary
    *
    * @return the result of the asynchronous computation
    */
  def await(): Return = Await.result(future, duration.Duration.Inf)
}
