package impromptu

import scala.concurrent.ExecutionContext.Implicits.global

object Examples extends App {

  def processRequest(req: String) = {
    val parseRequest = Async {
      "REQUEST "+req
    }

    val storeInDatabase = Async.post(parseRequest) { implicit env =>
      println("Storing in DB")
      Thread.sleep(3000)
      println("Finished storing")
      1000
    }

    val generateInvoice = Async.post(storeInDatabase, parseRequest) { implicit env =>
      println("Generating invoice!")
      Thread.sleep(2000)
      println(storeInDatabase())
      println("Finished generating invoice")
      "INVOICE!"
    }

    val response = Async.post(storeInDatabase, generateInvoice) { implicit env =>

      println("Stored in database: "+storeInDatabase())
      println("Invoiced: "+generateInvoice())
      "Done."
    }

    response.future
  }

  processRequest("foo")

  Thread.sleep(5000)

}
