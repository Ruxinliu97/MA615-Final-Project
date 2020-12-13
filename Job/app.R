# In order to make the shiny app runs faster, some data has been pre-modified and
# stored separately. The complete data handling process is included in the Data 
# Processing.Rmd in the GitHub Repo.

library(shiny)
library(tidyverse)
library(plotly)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)

# Load in the data sets
muse_data <- read.csv("muse_data.csv")
Adzuna_data <- read.csv("Adzuna_data.csv")
Adzuna_us <- read.csv("Adzuna_us.csv")
cite <- read.csv("jobsite.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Add web page title
    titlePanel(h1("Job Descriptions ", align = "center",
                  style = {'background-color:hsl(90, 85%,90%);color:green; 
                    border:4px double black'})),
    mainPanel(
        tabsetPanel(
            tabPanel("Job Board Websites", 
                     # Generate a row with a sidebar
                     sidebarLayout(      
                         
                         # Define the sidebar with one input
                         sidebarPanel(
                             selectInput("platform", "Platform:", 
                                         choices = c("Select", cite$Name))
                             
                         ),
                         mainPanel(
                             tableOutput("view") 
                         ))) 

            
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$view <- renderTable({
        viewdata <- cite
        if (input$platform != "Select") {
            view(viewdata[viewdata$Name == input$platform, c(2, 3)])
        }
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
