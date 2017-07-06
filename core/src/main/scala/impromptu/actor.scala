package impromptu

import scala.util.Try
import scala.concurrent._, ExecutionContext.Implicits.global
import language.implicitConversions
import scala.reflect.runtime.universe.TypeTag
import annotation.unchecked.{uncheckedVariance => uv}

case class TypeIndex[T](tag: TypeTag[T]) {
  override def equals(that: Any): Boolean = that match {
    case that: TypeIndex[_] => tag.tpe =:= that.tag.tpe
    case _ => false
  }
  override def hashCode: Int = tag.tpe.hashCode
}

object Actor {
  def apply[Accept, State](init: State)(action: State => Handler[Accept, State]): Actor[State, Accept] =
    new Actor[State, Accept](init, action)
}

object handle {
  def apply[Accept, State](cases: Case[Accept, State]*): Handler[Accept, State] =
    Handler(cases.map { c => c.index -> c.fn }.toMap)
}

case class Handler[-Accept, +State](fns: Map[TypeIndex[_], Accept => State]) {
  def handle[Msg: TypeTag](msg: Msg): State =
    fns(TypeIndex(implicitly[TypeTag[Msg]])).asInstanceOf[Msg => State](msg)
}

class Actor[State, Accept] private (val state: State, val handler: State => Handler[Accept, State]) {
  private[this] var currentFuture = Future(state)
  def send[Msg: TypeTag](msg: Msg)(implicit ev: Accept <:< Msg): Unit = synchronized {
    currentFuture = currentFuture.flatMap { oldState =>
      Future(handler(oldState).handle(msg))
    }
  }
}

case class Case[-Type, +State](index: TypeIndex[Type @uv], fn: Type => State)

object on {
  def apply[Type] = Apply[Type]()
  case class Apply[Type]() {
    def apply[State](action: Type => State)(implicit tag: TypeTag[Type]): Case[Type, State] =
      Case(TypeIndex[Type](tag), action)
  }
}

object Test {

  case class Ping()
  case class Pong()

  lazy val ping: Actor[Int, Ping] = Actor(0) { count =>
    handle(
      on[Ping] { case Ping() =>
        if(count%100 == 0) logger.send(s"Counted $count")
        pong.send(Pong())
        count + 1
      }
    )
  }
 
  lazy val pong = Actor(Set[String]()) { entries =>
    handle(
      on[Pong] { i =>
        ping.send(Ping())
        entries
      }
    )
  }

  lazy val logger = Actor(()) { _ =>
    handle(
      on[String] { msg => println(msg) }
    )
  }

}
