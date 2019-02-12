package edu.cua.computus

import org.scalatra._

class ComputusServlet extends ScalatraServlet {

  get("/") {
    views.html.hello()
  }

}
