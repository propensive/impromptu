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
import language.implicitConversions
import scala.reflect.runtime.universe.TypeTag
import scala.annotation.unchecked.{uncheckedVariance => uv}
import scala.annotation.implicitNotFound

/** the `impromptu` package object */
object `package` {
  /** convenience method for constructing a new [[Handler]]
    * 
    * @param cases  the cases to be handled
    * @tparam Accept  the intersection type of the message types to be handled
    * @tparam State   the least-upper-bound return type of all the cases
    * @return a new [[Handler]]
    */
  def handle[Accept, State](cases: Actor.Case[Accept, State]*): Actor.Handler[Accept, State] =
    Actor.Handler(cases.map { c => c.index -> c.fn }.toMap)
 
  /** constructs an intermediate [[Apply]] factory object for defining [[Case]]s
    *
    * @tparam Msg  the message type to handle
    * @return a new [[Apply]] instance for constructing a new [[Case]]
    */
  def on[Msg] = Apply[Msg]()
  
  /** intermediate factory object for constructing new [[Case]]s for a particular message type
    *
    * @tparam Msg  the fixed message type for which to construct a new [[Case]]
    */
  case class Apply[Msg] private () {
    def apply[State](action: Msg => State)(implicit tag: TypeTag[Msg]): Actor.Case[Msg, State] =
      Actor.Case(Actor.TypeIndex[Msg](tag), action)
  }
}

/** factory for creating [[Actor]]s */
object Actor {
  /** creates a new [[Actor]]
    * 
    * @param init     the initial value of the [[Actor]]'s state
    * @param action   the action which creates the [[Handler]] which will process messages
    * @param execCtx  the [[ExecutionContext]] on which to execute the action
    * @tparam Accept  the intersection type of all the types the actor can accept
    * @tparam State   the type of the [[Actor]]'s state
    * @return a new [[Actor]], ready to start receiving messages
    */
  def apply[Accept, State](init: State)(action: State => Handler[Accept, State])(implicit
                           execCtx: ExecutionContext): Actor[State, Accept] =
    new Actor[State, Accept](init, action)
  
  /** represents the functions which operate on a number of different message types
    *
    * @param fns  the functions 
    * @tparam Accept  the intersection type of all the message types the [[Actor]] can accept
    * @tparam State   the type of the [[Actor]]'s state
    */
  final case class Handler[-Accept, +State] private (fns: Map[TypeIndex[_], Accept => State]) {
    def handle[Msg: TypeTag](msg: Msg): State =
      fns(TypeIndex(implicitly[TypeTag[Msg]])).asInstanceOf[Msg => State](msg)
  }

  /** an index for [[TypeTag]]s which allows them to be looked up in a [[Map]]
    *
    * @tparam T  the type being indexed
    * @param tag  the [[TypeTag]] for the type to be indexed
    */
  final case class TypeIndex[T] private (tag: TypeTag[T]) {
    override def equals(that: Any): Boolean = that match {
      case that: TypeIndex[_] => tag.tpe =:= that.tag.tpe
      case _ => false
    }
    override def hashCode: Int = tag.tpe.hashCode
  }

  /** one case, corresponding to one type, to be matched in a [[Handler]]
    *
    * @tparam Msg    the type of message being handled by this [[Case]]
    * @tparam State  the return type from this case, corresponding to the type of the enclosing
    *                [[Actor]]'s state
    */
  case class Case[-Msg, +State] private (index: TypeIndex[Msg @uv], fn: Msg => State)
}

/** an actor
  *
  * @param state    the state of the actor
  * @param handler  constructs a new handler based on the actor's state
  * @param execCtx  the execution context in which computations should be done
  * @tparam State   the type of the actor's state
  * @tparam Accept  the intersection type of the messages the actor can receive
  */
class Actor[State, Accept] private (val state: State,
                                    val handler: State => Actor.Handler[Accept, State])(implicit
                                    execCtx: ExecutionContext) {
  private[this] var currentFuture = Future(state)

  /** type alias for providing more appropriate error messages */
  @implicitNotFound("this actor cannot accept messages of type ${Msg}")
  type In[Msg, Accept] = Accept <:< Msg

  /** sends a message to this actor
    *
    * @param msg  the message to send
    * @param ev   evidence that the actor can receive messages of this type
    * @tparam Msg  the type of the message to send
    * @return a unit
    */
  def send[Msg: TypeTag](msg: Msg)(implicit ev: Msg In Accept): Unit = synchronized {
    currentFuture = currentFuture.flatMap { oldState =>
      Future(handler(oldState).handle(msg))
    }
  }
}

