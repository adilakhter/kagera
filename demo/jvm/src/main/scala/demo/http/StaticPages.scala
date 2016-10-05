package demo.http

import scalatags.Text.all._

object StaticPages {
  val dashboard =
    html(
      head(
        script(src := "resources/demo-fastopt.js"),
        script(src := "resources/demo-jsdeps.js")
      ),
      body(
        onload := "io.kagera.frontend.Client().main(document.getElementById('contents'))",
        div(id := "contents")
      )
    )
}
