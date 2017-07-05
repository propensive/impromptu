package impromptu

import scala.util.Try
import scala.concurrent._, ExecutionContext.Implicits.global
import language.implicitConversions

object Actor {
  def apply[Accept, Return](action: Env[Actor[_, _, _]] => Handler[Accept, Return]): Actor[Return, Accept, Actor[_, _, _]] =
    new Actor[Return, Accept, Actor[_, _, _]](Seq(), action)

  def listensTo[Before <: Actor[_, _, _], Accept, Return](deps: Dependency[Before]*)(
      action: Env[Before] => Handler[Accept, Return]): Actor[Return, Accept, Before] =
    new Actor[Return, Accept, Before](deps.map(_.actor), action)

  implicit def autoWrap(actor: Actor[_, _, _]): Dependency[actor.type] = Dependency(actor)

  final case class Dependency[-A <: Actor[_, _, _]] private (actor: Actor[_, _, _])
  final case class Env[+Before] private (values: Map[Actor[_, _, _], Future[_]]) {
  }
}

object handle {
  def apply[Before, Accept, Return](action: Case[Accept, Return]*)(implicit env: Actor.Env[_ >: Before]): Handler[Accept, Return] =
    Handler(action.map(_.fn))
}

case class Handler[-Accept, +Return](fns: Seq[Accept => Return])

class Actor[+Return, Accept, Before] private (val deps: Seq[Actor[_, _, _]], val action: Actor.Env[Before] => Handler[Accept, Return]) {
  def send[Msg](msg: Msg)(implicit env: Actor.Env[this.type], ev: Accept <:< Msg): Unit = ???
  def seed[Msg](msg: Msg)(implicit ev: Accept <:< Msg) = ???
}

case class Case[-Type, +Return](fn: Type => Return)

object on {
  def apply[Type] = Apply[Type]()
  case class Apply[Type]() {
    def apply[Return](action: Type => Return): Case[Type, Return] = Case(action)
  }
}

object Test {

  lazy val ping = Actor.listensTo(pong) { implicit env =>
    pong.send("ping")
    pong.send(42)
    handle(
      on[String] { s => println("Hello") },
      on[Int] { i => i + 1 }
    )
  }
  
  lazy val pong = Actor { implicit env =>
    handle(
      on[String] { s => "pong" },
      on[Int] { i => i + 1 }
    )
  }

}
