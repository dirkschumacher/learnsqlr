#' Get a SQL leaner shiny app
#'
#' @export
#' @import shiny
#' @import miniUI
#' @include exercises.R
learnsql_shiny <- function() {
  database_connection <- DBI::dbConnect(duckdb::duckdb())
  exercise_ids <- purrr::map(exercise_database, "id")
  names(exercise_ids) <- purrr::map(exercise_database, "title")
  execution_environment <- new.env(parent = globalenv())
  shinyApp(
    ui = fluidPage(
      shinyjs::useShinyjs(),
      h1("Improve your SQL with R and duckdb"),
      selectInput("exercise_id", "Exercise", choices = exercise_ids),
      hr(),
      verbatimTextOutput("instructions"),
      h2("Duckdb SQL playground"),
      fluidRow(
        column(
          6,
          h3("Your SQL solution"),
          shinyAce::aceEditor(
            outputId = "sql_input",
            theme = "xcode",
            mode = "sql",
            readOnly = FALSE,
            value = ""
          )
        ),
        column(6, tableOutput("preview_result"))
      ),
      actionButton("show_sql_solution", "Show SQL solution"),
      verbatimTextOutput("sql_solution"),
      h2("Expected result"),
      tableOutput("expected_result"),
      h2("R solution"),
      fluidRow(
        column(6, shinyAce::aceEditor(
          outputId = "dplyr_code",
          theme = "xcode",
          mode = "r",
          readOnly = TRUE,
          value = ""
        )),
        column(6, shinyAce::aceEditor(
          outputId = "datatable_code",
          theme = "xcode",
          mode = "r",
          readOnly = TRUE,
          value = ""
        )),
      ),
      hr(),
      selectInput("datasets", "Preview datasets", choices = NULL),
      verbatimTextOutput("preview_dataset")
    ),
    server = function(input, output, session) {
      current_exercise <- reactive({
        req(length(input$exercise_id) == 1, input$exercise_id > 0)
        exercise <- purrr::keep(exercise_database, ~ .x$id == input$exercise_id)
        req(length(exercise) == 1)
        exercise[[1]]
      })

      observeEvent(input$show_sql_solution, {
        shinyjs::toggle("sql_solution")
      })

      observeEvent(current_exercise(), {
        shinyjs::hide("sql_solution")
      })

      observeEvent(current_exercise(), {
        # add dataset to the database
        for (required_data in current_exercise()$datasets) {
          utils::data(
            list = required_data$name,
            package = required_data$package,
            envir = execution_environment
          )
          DBI::dbWriteTable(database_connection,
            name = required_data$name,
            get0(required_data$name, execution_environment),
            overwrite = TRUE
          )
        }
      })

      observeEvent(current_exercise(), {
        names <- purrr::map_chr(current_exercise()$datasets, "name")
        updateSelectInput(session, "datasets", choices = names)
      })

      format_code <- function(code) {
        paste0(styler::style_text(paste0(deparse(code), collapse = "\n")), collapse = "\n")
      }
      observeEvent(current_exercise(), {
        shinyAce::updateAceEditor(session, "dplyr_code", value = format_code(current_exercise()$dplyr))
        shinyAce::updateAceEditor(session, "datatable_code", value = format_code(current_exercise()$datatable))
      })

      preview_result <- reactive({
        user_sql <- input$sql_input
        tibble::as_tibble(DBI::dbGetQuery(database_connection, user_sql))
      })

      output$instructions <- renderText({
        current_exercise()$text_instructions
      })

      output$sql_solution <- renderText({
        current_exercise()$sql_solution
      })

      output$preview_dataset <- renderPrint({
        req(is.character(input$datasets), length(input$datasets) == 1)
        value <- get0(input$datasets, envir = execution_environment)
        req(is.data.frame(value))
        utils::str(value)
      })

      output$preview_result <- renderTable({
        df <- preview_result()
        req(is.data.frame(df))
        df
      })

      expected_result <- reactive({
        exercise <- current_exercise()
        df <- eval(exercise$dplyr, envir = new.env(parent = globalenv()))
        colnames(df) <- tolower(colnames(df))
        df
      })

      is_equal <- reactive({
        all.equal(preview_result(), expected_result())
      })

      output$expected_result <- renderTable({
        expected_result()
      })
    },
    onStart = function() {
      onStop(function() {
        if (DBI::dbIsValid(database_connection)) {
          DBI::dbDisconnect(database_connection)
        }
      })
    }
  )
}


#' Start the SQL Learner Gadget
#'
#' @export
#' @import shiny
learnsqlr_rstudio <- function() {
  runGadget(learnsql_shiny())
}
