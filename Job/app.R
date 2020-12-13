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

website <- c("Adzuna_uk", "Adzuna_us", "The Muse")

# Define UI for application that draws a histogram

ui <- fluidPage(
    # Add web page title
    titlePanel(h1("Job Descriptions ", align = "center",
                  style = {'background-color:hsl(90, 85%,90%);color:green; 
                    border:4px double black'})),
    h4(tags$a(href="https://github.com/Ruxinliu97/MA615-Final-Project", "Ruxin Liu")),
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
    output$cloud <- renderWordcloud2({
        text_us <- Adzuna_us$description
        docs_us <- Corpus(VectorSource(text_us))
        
        docs_us <- docs_us %>%
            tm_map(removeNumbers) %>%
            tm_map(removePunctuation) %>%
            tm_map(stripWhitespace)
        docs_us <- tm_map(docs_us, content_transformer(tolower))
        docs_us <- tm_map(docs_us, removeWords, stopwords("english"))
        
        dtm_us <- TermDocumentMatrix(docs_us) 
        matrix_us <- as.matrix(dtm_us) 
        words_us <- sort(rowSums(matrix_us),decreasing=TRUE) 
        df_us <- data.frame(word = names(words_us),freq=words_us)
        
        set.seed(1997) 
        wordcloud(words = df_us$word, freq = df_us$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,            colors=brewer.pal(8, "Dark2"))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
