# ---------
# FUNCTIONS
# ---------
library(dplyr)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)

# the fun transformation
fun_t <- function(x, values)
{
  num_sampling <- length(values)
  integral <- 0
  fx <- values[max(1, round(x*num_sampling))]
  
  for (i in 1:num_sampling)
  {
    fy <- values[i]
    y <- i/num_sampling
    dy <- 1/num_sampling
    integral <- integral + abs(fx-fy)/(1+(x-y)^2) * dy
  }
  
  x^3/(1+exp(x)) + 1/17 * integral
}

# plug a function into the fun transformation
iterate <- function(values)
{
  num_sampling <- length(values)
  new_values <- vector(mode = "numeric", length = num_sampling)
  
  for (i in 1:num_sampling)
    new_values[i] <- fun_t(i/num_sampling, values)
  
  new_values
}

# iterate n times
iterate_n <- function(values, n)
{
  result <- vector(mode="list", length = n+1)
  
  for (i in 1:n)
  {
    # print(sprintf("mean: %s, sd: %s", mean(values), sd(values)))
    result[[i]] <- values 
    values <- iterate(values)
  }
  
  result
}

# plots the graph
plotly_2d <- function(x, y, color, c_seq, title) 
{
  plot_ly(
    x = x, y = y, text = color, color = color, colors = c_seq, type="scatter", 
    mode = "lines+markers", marker = list(size = 6, symbol = 'circle'),
    hovertemplate = "<b>%{text}</b><br>(%{x:.8f}, %{y:.8f})<extra></extra>", 
  ) %>% layout(title = title, showlegend = TRUE)
}

# Displays a default graph, which is accepted by ggplot2 and plotly.
ggplot2_null <- function()
{
  df <- data.frame()
  ggplot(df) + geom_point() + 
    ggtitle("This plot cannot be displayed.") +
    xlim(0, 1) + ylim(0, 1)
}

# -----------
# RUN THE APP
# -----------

my_css_styling <- HTML("
/* Everyone's favorite color - Yale Blue! */
.skin-blue .main-header .logo {
  background-color: #00356B !important;
}

/* Place sidebar toggle on right! */
.sidebar-toggle {
  float: right !important;
}

/* Prevents weird sidebar glitch */
.wrapper {
  height: auto !important; 
  position:relative; 
  overflow-x:hidden; 
  overflow-y:hidden
}
")

complete_ui <- dashboardPage(
  skin="blue",
  dashboardHeader(title="Banach", titleWidth="100%"),
  dashboardSidebar(
    width=300,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem(
        startExpanded=TRUE,
        "Parameters",
        numericInput("init_val", "Initial Value", value=0.5, min=0, max=1),
        numericInput("num_iter", "Number of Iterations", value=5, min=1, max=10),
        numericInput("sampling", "Number of Samples", value=500, min=1, max=1000)
      )
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(my_css_styling)),
    uiOutput("plotly2UI"),
    hr(),
    "Developed by Justin Chang."
  )
)

server <- function(input, output, session) {
  # pushes the subtitle to the right
  shinyjs::addClass(id = "central_nav", class = "navbar-right")
  
  # renders plotly2 output
  output$plotly2UI <- renderUI({
    plotlyOutput("plotly2_out", width="100%", height=600) %>% withSpinner(type = 6)
  })
  
  # renders the plotly2 data reactively
  plotly2_current <- reactiveVal()
  output$plotly2_out <- renderPlotly({
    if (input$sampling > 2000 || input$sampling < 1 || 
        input$num_iter > 10 || input$num_iter < 1 ||
        input$init_val > 1 || input$init_val < 0)
      return(ggplot2_null())
    
    # resolution of integral
    num_sampling <- input$sampling
    # number of iterations
    num_iter <- input$num_iter+1
    # initial function
    values <- rep(input$init_val, num_sampling)
    # sequence of colors
    seq <- hcl(1:num_iter * (360/(num_iter+1))-15, 160, 60)
    # result of iteration
    res <- iterate_n(values, num_iter)
    # used in plotting
    x_val <- NULL; y_val <- NULL; color <- NULL
    for (i in 1:num_iter)
    {
      x_val <- c(x_val, 1:num_sampling/num_sampling)
      y_val <- c(y_val, res[[i]])
      color <- c(color, rep(as.character(i-1), num_sampling))
    }
    # return the plot
    plotly_2d(x_val, y_val, color, seq, "Banach")
  })
}

shinyApp(ui = complete_ui, server = server)