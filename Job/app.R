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
library(shinythemes)

# Load in the data sets
muse_data <- read.csv("muse_data.csv")
Adzuna_data <- read.csv("Adzuna_data.csv")
Adzuna_us <- read.csv("Adzuna_us.csv")
cite <- read.csv("jobsite.csv")

muse_data$sql <- grepl("SQL",muse_data$contents)
muse_data$Python <- grepl("Python",muse_data$contents)
muse_data$SAS <- grepl("SAS",muse_data$contents)
muse_data$R <- grepl("\\bR\\b",muse_data$contents)


# Define UI for application that draws a histogram

ui <- fluidPage(
    theme = shinytheme('lumen'),
    # Add web page title
    titlePanel(h1("Job Descriptions ", align = "center",
                  style = {'background-color:hsl(90, 85%,90%);color:green; 
                    border:4px double black'})),
    h4(tags$a(href="https://github.com/Ruxinliu97/MA615-Final-Project", "Ruxin Liu")),
    mainPanel(
        tabsetPanel(
            
            tabPanel("Word Cloud",
                     sidebarLayout(
                         sidebarPanel(
                             radioButtons("source",
                                          "Word Source", 
                                          c("Adzuna_uk" = "uk", "Adzuna_us" ="us", "The Muse"="muse")),
                             numericInput("num", "Maximum Number Of Words", value = 180, min = 10)
                         ),
                      
                         
                         mainPanel(
                             wordcloud2Output('cloud') 
                         )
                     )
            ) ,
            tabPanel("Programing Skills In Job Descriptions", 
                     # Generate a row with a sidebar
                     sidebarLayout(      
                         
                         # Define the sidebar with one input
                         sidebarPanel(
                             selectInput("skill", "Skill:", 
                                         choices = c("R", "SAS", "SQL","Python"))
                             
                         ),
                         mainPanel(
                             plotOutput("plot")
                         ))) ,
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
    
    data_source <- reactive({
        if(input$source == "uk"){
            data <- read.csv("Adzuna_data.csv")
            text <- data$description
        } else if (input$source == "us"){
            data <- read.csv("Adzuna_us.csv")
            text <- data$description
        }else if (input$source == "muse"){
            data <- read.csv("muse_data.csv")
            text <- data$contents
        }
        print(text)
    })
    

    
    output$plot <- reactivePlot(function(){
        if(input$skill == "R"){
            p <- ggplot(muse_data, aes(level, fill = R)) +
                geom_bar() +
                scale_fill_discrete(labels = c("Not Containing R", "JContaining R"))
        }
        if(input$skill == "SAS"){
            p <- ggplot(muse_data, aes(level, fill = SAS)) +
                geom_bar() +
                scale_fill_discrete(labels = c("Not Containing SAS", "Containing SAS"))
        }
        if(input$skill == "Python"){
            p <- ggplot(muse_data, aes(level, fill = Python)) +
                geom_bar() +
                scale_fill_discrete(labels = c("Not Containing Python", "Containing Python"))
        }
        if(input$skill == "SQL"){
            p <- ggplot(muse_data, aes(level, fill = sql)) +
                geom_bar() +
                ggtitle("Occurrence Frequency of SQL in Job Description") +
                scale_fill_discrete(labels = c("Not Containing SQL", "Containing SQL"))
        }
        print(p)
        
    })
    plotOutput(
        plot,
        width = "100%",
        height = "400px"
    )
    
    
    create_wordcloud <- function(text, num_words = 100){
        
        docs <- Corpus(VectorSource(text))
        
        docs <- tm_map(docs, removeNumbers)
        docs <- tm_map(docs, removePunctuation)
        docs <- tm_map(docs, stripWhitespace)
        docs <- tm_map(docs, removeWords, stopwords(tolower("english")))
        docs <- tm_map(docs, removeWords, c("the", "will", "are", "you", "this"))
        dtm <- TermDocumentMatrix(docs) 
        matrix <- as.matrix(dtm) 
        words <- sort(rowSums(matrix),decreasing=TRUE) 
        df <- data.frame(word = names(words),freq=words)
        df <- head(df, n = num_words)
        wordcloud2(df)

    }
    
    
    output$cloud <-renderWordcloud2({
        create_wordcloud(data_source(),
                         num_words = input$num)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

















