library(shiny)
library(gsheet)
library(tidyverse)

ui <- fluidPage(

   titlePanel("2017 Linguistics Holiday Party Survey Results"),
   mainPanel(
       hr(),
       DT::dataTableOutput("resultsTable"),
       br(),
       DT::dataTableOutput("oneOnlyTable"),
       br(),
       DT::dataTableOutput("alternativesTable"),
       br(),
       DT::dataTableOutput("considerationsTable"),
       br()
       )
)

server <- function(input, output) {
    poll <- "https://docs.google.com/spreadsheets/d/1-GSYxj-uJnBntvw2dc0ekzPiNqhgIe4gKRyrUJYqFcY/edit?usp=sharing"
    data <- read_csv(construct_download_url(poll))

    data <- data %>%
        mutate(`Thursday, December 7` = ifelse(grepl("December 7", data[[2]]), 1, 0),
               `Friday, December 8` = ifelse(grepl("December 8", data[[2]]), 1, 0),
               `Saturday, December 9` = ifelse(grepl("December 9", data[[2]]), 1, 0),
               `Monday, December 11` = ifelse(grepl("December 11", data[[2]]), 1, 0)) %>%
        mutate(other = str_replace_all(data[[2]],
                                       "(Thursday, December 7 \\(non-optimal because of modality and acquisition meetings Friday morning\\)|Friday, December 8 \\(non-optimal because there is a colloquium, but the speaker could come\\?!\\?\\)|Saturday, December 9|Monday, December 11 \\(last day of classes, but Sergio and Aaron \\(and possibly others\\?\\) will already be gone\\))",
                                       "")) %>%
        mutate(other = str_replace_all(other,
                                       "(, )*",
                                       "")) %>%
        mutate(other = ifelse(other == "", NA, other))

    other_dates <- data.frame(alternatives = data[["other"]][!is.na(data[["other"]])],
                              stringsAsFactors = FALSE)

    comments <- data.frame(considerations = data[[3]][!is.na(data[[3]])],
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

    data_one_only <- data %>%
        select(`Thursday, December 7`,
               `Friday, December 8`,
               `Saturday, December 9`,
               `Monday, December 11`) %>%
        filter(rowSums(.) == 1) %>%
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

   output$oneOnlyTable <- DT::renderDataTable({data_one_only},
                                              server = FALSE,
                                              rownames = FALSE,
                                              extensions = c("ColReorder", "Responsive"),
                                              options = list(dom = "t"),
                                              caption = "Number of people who would only attend on the indicated day")

   output$alternativesTable <- DT::renderDataTable({other_dates},
                                                   server = FALSE,
                                                   rownames = FALSE,
                                                   extensions = c("Responsive"),
                                                   options = list(dom = "t"),
                                                   caption = "Alternative suggestions for dates")

   output$considerationsTable <- DT::renderDataTable({comments},
                                                     server = FALSE,
                                                     rownames = FALSE,
                                                     extensions = c("Responsive"),
                                                     options = list(dom = "t"),
                                                     caption = "Comments about other scheduling considerations")
}

shinyApp(ui = ui, server = server)