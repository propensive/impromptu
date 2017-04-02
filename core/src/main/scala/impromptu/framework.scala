package impromptu

import scala.util.Try
import scala.concurrent._

import scala.language.implicitConversions

class LazyFuture[+T](action: () => Future[T]) {
  lazy val future: Future[T] = action()
}

abstract class Env[+Before] {
  def execState: Task.ExecState
  def name: String
  def get[Return](task: Task[Return, _]): Return
}

object Task {
  def apply[Return](action: => Return): Task[Return, _] =
    new Task[Return, Nothing](Right(Seq()), env => action)
  
  def requiring[Before <: Task[_, _]](dependencies: Dependency[Before]*) =
    new Prerequisites[Before](dependencies.map(_.task))

  def listen[Before <: Task[_, _], Return](dependency: Dependency[Before])(action: Env[Before] => Return) =
    new Task[Return, Before](Right(Seq()), action)

  object Dependency {
    implicit def autoWrap(task: Task[_, _]): Dependency[task.type] = new Dependency(task)
  }
  
  class Dependency[-D <: Task[_, _]](val task: Task[_, _])

  class ExecState() {
    @volatile private var futures: Map[Task[_, _], LazyFuture[Any]] = Map()
    @volatile private var refCounts: Map[Task[_, _], Int] = Map()

    private[impromptu] def future[Return](task: Task[Return, _]): Option[LazyFuture[Return]] =
      synchronized { futures.get(task).map(_.asInstanceOf[LazyFuture[Return]]) }

  }
}

class Task[+Return, Before](val dependencies: Either[Task[_, _], Seq[Task[_, _]]], val action: Env[Before] => Return) {
  def apply()(implicit env: Env[this.type]): Return = env.get(this)
}


class Prerequisites[Before](tasks: Seq[Task[_, _]]) {
  def apply[Return](action: Env[Before] => Return): Task[Return, Before] =
    new Task[Return, Before](Right(tasks), action)
}
