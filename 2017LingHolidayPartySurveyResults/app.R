library(shiny)
library(gsheet)
library(tidyverse)

ui <- fluidPage(

   titlePanel("2017 Linguistics Holiday Party Survey Results"),
   mainPanel(
       hr(),
       DT::dataTableOutput("resultsTable"),
       br(),
       DT::dataTableOutput("considerationsTable")
       )
)

server <- function(input, output) {
    poll <- "https://docs.google.com/spreadsheets/d/1-GSYxj-uJnBntvw2dc0ekzPiNqhgIe4gKRyrUJYqFcY/edit?usp=sharing"
    data <- read_csv(construct_download_url(poll))

    data <- data %>%
        mutate(`Thursday, December 7` = ifelse(grepl("December 7", data[[2]]), 1, 0),
               `Friday, December 8` = ifelse(grepl("December 8", data[[2]]), 1, 0),
               `Saturday, December 9` = ifelse(grepl("December 9", data[[2]]), 1, 0),
               `Monday, December 11` = ifelse(grepl("December 11", data[[2]]), 1, 0))

    comments <- data.frame(Considerations = data[[3]][!is.na(data[[3]])],
                           stringsAsFactors = FALSE)

    data_sum <- data %>%
        select(`Thursday, December 7`,
               `Friday, December 8`,
               `Saturday, December 9`,
               `Monday, December 11`) %>%
        colSums() %>%
        data.frame(date = attr(., "names"),
                   count = .,
                   row.names = NULL)

   output$resultsTable <- DT::renderDataTable({data_sum},
                                              server = FALSE,
                                              rownames = FALSE,
                                              extensions = c("ColReorder", "Responsive"),
                                              options = list(dom = "t"),
                                              caption = paste0("Number of people who would attend on each date (n=",
                                                               nrow(data),
                                                               ")"))

   output$considerationsTable <- DT::renderDataTable({comments},
                                                     server = FALSE,
                                                     rownames = FALSE,
                                                     extensions = c("Responsive"),
                                                     options = list(dom = "t"),
                                                     caption = "Comments about other scheduling considerations")
}

shinyApp(ui = ui, server = server)