#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("libraries.R")
# Read in the data
refugees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-22/population.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("Overview", fluid = TRUE,
    # Application title
    titlePanel("Refugees dataset WHO"),
    selectInput("country", "Select country of origin",
                choices = sort(unique(refugees$coo_name)),
                selected = "Afghanistan"),
    selectInput("year", "Select the year",
                choices = distinct(refugees, year),
                multiple = TRUE),
    plotOutput("refugeesyearly")
    # Sidebar with a slider input for number of bins 
      ),
    tabPanel("Map", fluid = TRUE
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # create year and country of origin filter
    refugees.overview <- reactive({refugees %>% 
        filter(coo_name == input$country) %>%
        filter(year %in% input$year) %>%
        group_by(year) %>%
        summarise(refugees = sum(refugees))})
 # calculate lollipop plot per year
    gg.refugees.yearly <- reactive ({ggplot(data = refugees.overview(),
                                           aes(x = as.factor(year), y = refugees)) +
        geom_point(size = 4) +
        geom_segment(aes(x = as.factor(year), 
                         xend = as.factor(year), y = 0, yend = refugees))})
    
    output$refugeesyearly <- renderPlot({plot(gg.refugees.yearly())})
}

# Run the application 
shinyApp(ui = ui, server = server)
