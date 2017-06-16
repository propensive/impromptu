package impromptu

import scala.util.Try
import scala.concurrent._, ExecutionContext.Implicits.global
import scala.language.implicitConversions

object Async {
  def apply[Return](action: => Return): Async[Return, _] =
    new Async[Return, Nothing](Seq(), env => action)
  
  def post[Before <: Async[_, _]](deps: Dependency[Before]*) =
    new Dependencies[Before](deps.map(_.async))

  object Dependency {
    implicit def autoWrap(async: Async[_, _]): Dependency[async.type] = new Dependency(async)
  }
  
  class Dependency[-A <: Async[_, _]](val async: Async[_, _])

  class Dependencies[Before](asyncs: Seq[Async[_, _]]) {
    def apply[Return](action: Env[Before] => Return): Async[Return, Before] =
      new Async[Return, Before](asyncs, action)
  }

  case class Env[+Before](values: Map[Async[_, _], Future[_]])
}

class Async[+Return, Before](val deps: Seq[Async[_, _]], val action: Async.Env[Before] => Return) {
  def apply()(implicit env: Async.Env[this.type]): Return = env.values(this).value.get.get.asInstanceOf[Return]

  lazy val future: Future[Return] = {
    val results: Map[Async[_, _], Future[_]] = deps.map { d => d -> d.future }.toMap
    Future.sequence(results.map(_._2)).map { _ => action(Async.Env(results)) }
  }

  def await(): Return = Await.result(future, duration.Duration.Inf)
}

