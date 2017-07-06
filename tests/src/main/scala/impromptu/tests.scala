package impromptu.tests

import impromptu._

object Test {

  case class Ping()
  case class Pong()

  lazy val ping: Actor[Int, Ping] = Actor(0) { count =>
    handle(
      on[Ping] { case Ping() =>
        if(count%1000 == 0) logger.send(s"Counted $count")
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
