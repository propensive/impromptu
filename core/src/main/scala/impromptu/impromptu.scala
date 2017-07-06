package impromptu

import scala.util.Try
import scala.concurrent._
import scala.language.implicitConversions

object Async {
  def apply[Return](action: => Return): Async[Return, _] =
    new Async[Return, Nothing](Seq(), env => action)
  
  def after[Before <: Async[_, _], Return](deps: Dependency[Before]*)(
      action: Env[Before] => Return): Async[Return, Before] =
    new Async[Return, Before](deps.map(_.async), action)

  implicit def autoWrap(async: Async[_, _]): Dependency[async.type] = Dependency(async)
  
  final case class Dependency[-A <: Async[_, _]] private (async: Async[_, _])
  final case class Env[+Before] private (values: Map[Async[_, _], Future[_]])
}

class Async[+Return, Before] private (val deps: Seq[Async[_, _]],
    val action: Async.Env[Before] => Return) {
  
  def apply()(implicit env: Async.Env[this.type]): Return =
    env.values(this).value.get.get.asInstanceOf[Return]

  lazy val future: Future[Return] =
    Future.sequence(deps.map { d => d -> d.future }).map { _ => action(Async.Env(results.toMap)) }

  def await(): Return = Await.result(future, duration.Duration.Inf)
}

