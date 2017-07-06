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
package impromptu.tests

import impromptu._
import scala.concurrent.ExecutionContext.Implicits.global

object Test extends App {

  case class Ping()
  case class Pong()

  lazy val ping: Actor[Int, Ping] = Actor(0) { count =>
    handle(
      on[Ping] { case Ping() =>
        if(count%1000 == 0) logger.send(s"Counted $count")
        if(count < 100000) pong.send(Pong())
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

  ping.send(Ping())

}
