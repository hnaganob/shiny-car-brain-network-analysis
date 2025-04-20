source("global.R")

set.seed(50)
net <- network(adj_dti, directed = FALSE)
edgelist <- as.matrix(net, matrix.type = "edgelist")
coord <- network.layout.fruchtermanreingold(net, NULL)

# vertex colors
color_ramp <- c("darkblue", "blue", "gray90", "orange", "darkorange")
ramp <- colorRamp(color_ramp)

y_range <- c(-5, 5)

# colorbar
colorbar_seq <- seq(y_range[1], y_range[2], length = 101)
colorbar_range <- y_range

lambda <- model_parameter_table$lambda
lambda_p_value <- model_parameter_table$lambda_p_value

library(shiny)
library(bslib)

ui <- fluidPage(
    # theme = bs_theme(version = 5,bootswatch = "darkly"),
    
    titlePanel("Brain Network Analysis: Markov Random Fields"),
    
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
            br(),
            "👇  Customize Color Gradient",
            br(),
            br(),
            
            div(
                style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 0.5em;",
                tags$label("Low", `for` = "col_low", style = "margin-right: 0.5em; margin-bottom: 1.0em;"),
                textInput("col_low", label = NULL, value = "blue", width = "150px")
            ),
            div(
                style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 0.5em;",
                tags$label("Mid", `for` = "col_mid", style = "margin-right: 0.5em; margin-bottom: 1.0em;"),
                textInput("col_mid", label = NULL, value = "white", width = "150px")
            ),
            div(
                style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 0.5em;",
                tags$label("High", `for` = "col_high", style = "margin-right: 0.5em; margin-bottom: 1.0em;"),
                textInput("col_high", label = NULL, value = "orange", width = "150px")
            ),
            
            hr(),
            
            tags$footer(
                class = "footer",
                style = "padding: 10px; text-align: center; color: gray;",
                HTML("Built by <a href='https://www.linkedin.com/in/hnaganob/' target='_blank'>Hiroki Naganobori</a><br/>
Ph.D. Candidate @ Indiana University")
            )
        ),
        
        div(
            class = "grid-container",
            card(class = "card1", plotOutput("plot_network_signal", width = "100%")),
            card(class = "card2", plotOutput("plot_lambda_wth_se", height = 200, width = "100%")),
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

# ui ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ui <- fluidPage(
#   # theme = bs_theme(),
# 
#   # Application title
#   titlePanel("Old Faithful Geyser Data"),
#   card(
#     layout_sidebar(
#       sidebar = sidebar(
#         open = "always",
# 
#         # slider input for time with animation
#         sliderInput(
#           inputId = "time",
#           label = "Time (sec)",
#           min = 2, max = 480, value = 2, step = 2,
#           ticks = FALSE,
#           animate = animationOptions(
#             interval = 1000
#           )
#         ),
#         hr(),
# 
#         # colorbar in sidebar
#         plotOutput("plot_colorbar", height = 65, width = "100%"),
# 
#         # custom colorbar
#         div(
#           style = "display: flex; align-items: center;",
#           tags$label("col low",
#             `for` = "col1",
#             style = "margin-right: 0.5em; margin-bottom: 1.0em;"
#           ),
#           textInput("col1", label = NULL, value = "darkblue", width = "150px")
#         ),
#         div(
#           style = "display: flex; align-items: center;",
#           tags$label("col mid",
#             `for` = "col2",
#             style = "margin-right: 0.5em; margin-bottom: 1.0em;"
#           ),
#           textInput("col2", label = NULL, value = "white", width = "150px")
#         ),
#         div(
#           style = "display: flex; align-items: center;",
#           tags$label("col high",
#             `for` = "col3",
#             style = "margin-right: 0.5em; margin-bottom: 1.0em;"
#           ),
#           textInput("col3", label = NULL, value = "darkorange", width = "150px")
#         ),
#       ),
# 
#       # plot network
#       plotOutput("plot_network_signal", height = 500, width = 500),
# 
#       # plot lambda with standard error
#       plotOutput("plot_lambda_wth_se", height = 200, width = "100%")
#     ),
#     card(
#       verbatimTextOutput("print_model_summary")
#     ),
#   ),
# )

# server ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output) {
    
    output$plot_network_signal <- renderPlot({
        par(oma = rep(0.2, 4), mar = rep(0, 4))
        plot_network_signal(
            net = net,
            coord = coord,
            signal = scale(signal),
            y_range = c(-5, 5),
            frame = which(time == input$time),
            # signal_color_ramp = as.vector(unlist(strsplit(input$signal_color_ramp, ","))),
            signal_color_ramp = c(input$col_low, input$col_mid, input$col_high),
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
            # color_ramp = as.vector(unlist(strsplit(input$signal_color_ramp, ","))),
            color_ramp = c(input$col_low, input$col_mid, input$col_high),
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

# Run the application 
shinyApp(ui = ui, server = server)
