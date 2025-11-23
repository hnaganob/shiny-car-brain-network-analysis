source("global.R")


# ui ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(
  # theme = bs_theme(version = 5, bootswatch = "darkly"),
  tags$style(
    HTML("
      .sidebar {
        background-color: #f0f0f0 !important;
      }
      .sidebar hr { 
        border: none;
        border-top: 1px solid #ccc;
        margin: 1em 0;
      }
    ")
  ),
  # theme = bs_theme(
  #   bootswatch = "flatly",
  #   bg = "#ffffff",
  #   fg = "#333333",
  #   primary = "#007bff",
  #   # Sidebar customization:
  #   "sidebar-bg" = "#e9ecef"
  # ),

  titlePanel("CAR Brain Network Analysis"),
  layout_sidebar(
    sidebar = sidebar(
      open = "always",
      sliderInput(
        inputId = "time",
        label = "Time (sec)",
        min = 2, max = 480, value = 2, step = 2,
        ticks = FALSE,
        animate = animationOptions(interval = 1000)
      ),
      hr(),
      plotOutput("plot_colorbar", height = 75, width = "100%"),
      br(),
      checkboxInput("custom_color", "Customize Color 👇"),
      div(
        style = "display: flex; align-items: center; justify-content: space-between; margin-top: -1.5em;",
        tags$label("High", `for` = "col_high", style = "margin-right: 0.5em; margin-bottom: 1.0em;"),
        textInput("col_high", label = NULL, value = "orange", width = "150px")
      ),
      div(
        style = "display: flex; align-items: center; justify-content: space-between; margin-top: -1.5em;",
        tags$label("Mid", `for` = "col_mid", style = "margin-right: 0.5em; margin-bottom: 1.0em;"),
        textInput("col_mid", label = NULL, value = "white", width = "150px")
      ),
      div(
        style = "display: flex; align-items: center; justify-content: space-between; margin-top: -1.5em;",
        tags$label("Low", `for` = "col_low", style = "margin-right: 0.5em; margin-bottom: 1.0em;"),
        textInput("col_low", label = NULL, value = "blue", width = "150px")
      ),
      hr(),
      tags$footer(
        class = "footer",
        style = "padding: 10px; text-align: center; color: gray;",
        HTML("Built by <a href='https://hnaganob.github.io/' target='_blank'>Hiroki Naganobori</a><br/>Indiana University")
      )
    ),
    div(
      class = "grid-container",
      card(class = "card1", plotOutput("plot_network_signal", width = "100%")),
      card(class = "card2", plotOutput("plot_lambda_wth_se", height = 250, width = "100%")),
      card(class = "card3", verbatimTextOutput("print_model_summary"))
    )
  ),
  tags$head(
    tags$style(HTML("
      .grid-container {
        display: grid;
        grid-template-columns: 1fr 1fr;
        grid-template-rows: auto auto;
        gap: 0.5rem;
        grid-template-areas:
          'card1 card3'
          'card2 card2';
      }
      .card1 { grid-area: card1; }
      .card2 { grid-area: card2; }
      .card3 { grid-area: card3; }
    "))
  )
)


# server ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
server <- function(input, output) {
  
  # color pallette
  color_palette <- reactive({
    if (isTRUE(input$custom_color)) {
      c(input$col_low, input$col_mid, input$col_high)
    } else {
      color_ramp
    }
  })

  output$plot_network_signal <- renderPlot({
    par(oma = rep(0.2, 4), mar = rep(0, 4))
    plot_network_signal(
      net = net,
      coord = coord,
      signal = scale(signal),
      y_range = c(-5, 5),
      frame = which(time == input$time),
      signal_color_ramp = color_palette(),
      model_parameter_table = model_parameter_table,
    )
  })

  output$plot_lambda_wth_se <- renderPlot({
    par(oma = rep(0, 4), mar = c(5.1, 4.1, 0, 2.1))
    plot_lambda_with_se(
      model_parameter_table = model_parameter_table,
      frame = which(time == input$time),
      alpha = 0.2,
      frame.plot = FALSE,
      xlim = c(0, 500)
    )
  })

  output$plot_colorbar <- renderPlot({
    par(oma = rep(0, 4), mar = c(5.1, 4.1, 0, 2.1))
    add_colorbar(
      y_range = c(-5, 5),
      color_ramp = color_palette(),
      horizontal = TRUE,
      main = "z-score"
    )
  })

  output$print_model_summary <- renderPrint({
    frame <- which(time == input$time)
    y <- scale(signal)[frame, ]
    model <- spautolm(y ~ 1, listw = listw, family = "CAR")
    summary(model)
  })
}


# Run the application ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shinyApp(ui = ui, server = server)
