package edu.cua.computus

import org.scalatra.test.scalatest._

class ComputusServletTests extends ScalatraFunSuite {

  addServlet(classOf[ComputusServlet], "/*")

  test("GET / on ComputusServlet should return status 200") {
    get("/") {
      status should equal (200)
    }
  }

}
