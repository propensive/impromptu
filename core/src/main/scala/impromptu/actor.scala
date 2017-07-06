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
import annotation.unchecked.{uncheckedVariance => uv}

object Actor {
  def apply[Accept, State](init: State)(action: State => Handler[Accept, State])(implicit
                           execCtx: ExecutionContext): Actor[State, Accept] =
    new Actor[State, Accept](init, action)
  
  case class Handler[-Accept, +State](fns: Map[TypeIndex[_], Accept => State]) {
    def handle[Msg: TypeTag](msg: Msg): State =
      fns(TypeIndex(implicitly[TypeTag[Msg]])).asInstanceOf[Msg => State](msg)
  }

  case class TypeIndex[T](tag: TypeTag[T]) {
    override def equals(that: Any): Boolean = that match {
      case that: TypeIndex[_] => tag.tpe =:= that.tag.tpe
      case _ => false
    }
    override def hashCode: Int = tag.tpe.hashCode
  }

  case class Case[-Type, +State](index: TypeIndex[Type @uv], fn: Type => State)
}

object handle {
  def apply[Accept, State](cases: Actor.Case[Accept, State]*): Actor.Handler[Accept, State] =
    Actor.Handler(cases.map { c => c.index -> c.fn }.toMap)
}

class Actor[State, Accept] private (val state: State,
                                    val handler: State => Actor.Handler[Accept, State])(implicit
                                    execCtx: ExecutionContext) {
  private[this] var currentFuture = Future(state)
  def send[Msg: TypeTag](msg: Msg)(implicit ev: Accept <:< Msg): Unit = synchronized {
    currentFuture = currentFuture.flatMap { oldState =>
      Future(handler(oldState).handle(msg))
    }
  }
}

object on {
  def apply[Type] = Apply[Type]()
  case class Apply[Type]() {
    def apply[State](action: Type => State)(implicit tag: TypeTag[Type]): Actor.Case[Type, State] =
      Actor.Case(Actor.TypeIndex[Type](tag), action)
  }
}
