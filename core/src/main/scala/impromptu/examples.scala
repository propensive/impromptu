package impromptu

object Examples {

  val task1 = Task {
    println("task1")
  }

  val task2 = Task {
    println("task2")
  }

  val task3 = Task.requiring(task1)/* { implicit env =>
    println(task1())
    println("task3")
  }

  val task4 = Task.listen(task2) { implicit env =>
    println(task2())
    println("task4")
  }*/

}
