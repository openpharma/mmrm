#' Personal greeting
#'
#' @description Greet a person and appropriately capitalize their name.
#'
#' @param name Your name (character string; e.g. "john doe").
#'
#' @return A character string, capitalized to title case.
#' @export
#'
#' @examples
#' hello("james bond")
hello <- function(name = "your name") {
  name <- stringr::str_to_title(name)
  print(paste("Hello,", name))
}

#' Personal greeting as a Shiny app
#'
#' @description Greet a person and appropriately capitalize their name
#'              as a Shiny app.
#'
#' @return Shiny app showcasing the personal greeting feature.
#' @export
#'
shiny_app <- function() {
  ui <- shiny::fluidPage(
    shiny::textInput("name", "What is your name?"),
    shiny::actionButton("greet", "Greet"),
    shiny::textOutput("greeting")
  )

  server <- function(input, output, session) {
    output$greeting <- shiny::renderText({
      shiny::req(input$greet)
      hello(shiny::isolate(input$name))
    })
  }

  shiny::shinyApp(ui, server)
}

#' Personal greeting as a Plumber API
#'
#' @importFrom utils packageName
#'
#' @description Greet a person and appropriately capitalize their name
#'              as a Plumber API.
#'
#' @param ... Additional arguments to plumber::pr_run()
#'
#' @return Plumber API showcasing the personal greeting feature.
#' @export
#'
plumber_api <- function(...) {
  plumber::pr_run(
    plumber::plumb_api(
      package = packageName(), name = "hello"
    ),
    ...
  )
}
