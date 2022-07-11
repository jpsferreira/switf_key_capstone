library(shiny)
library(ggpubr)

library(dplyr)
library(ggplot2)
library(stringi)
library(tm)
library(RWeka)
library(wordcloud)
library(cowplot)


#load data

# dataset_url<-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
# zipfile <- "Coursera-SwiftKey.zip"
# if (!file.exists(zipfile))
#         download.file(dataset_url, zipfile, method = "auto")
# # Define file paths and names
# blogs_file <- "final/en_US/en_US.blogs.txt"
# twitter_file <- "final/en_US/en_US.twitter.txt"
# news_file <- "final/en_US/en_US.news.txt"

# # Unzip the files
# if (!file.exists(blogs_file) || !file.exists(twitter_file) || !file.exists(news_file) )
#     unzip(zipfile)

# # Load the data into memory
# data_blogs   <- readLines(blogs_file, encoding="UTF-8")
# data_twitter <- readLines(twitter_file, encoding="UTF-8")
# data_news    <- readLines(news_file, encoding="UTF-8")

# # Save an object to a file
# saveRDS(data_blogs, file = "blogs.rds")
# saveRDS(data_twitter, file = "twitter.rds")
# saveRDS(data_news, file = "news.rds")


# data_news <- readRDS(url("https://drive.google.com/file/d/1RkeRG1pKENWWhnFJhY9U9dVgHBTOfC5R",open="rb"))
# data_twitter <- readRDS(url("https://drive.google.com/file/d/1Rh5RBSa0PFFgjf5fWNroqoFfUwISAc6g",open="rb"))
# data_blogs <- readRDS(url("https://drive.google.com/file/d/1RfsQNdnYhNmXOQeCVHo04RpWbq9HinEH",open="rb"))

mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

shinyServer(function(input, output) {

  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })

  formulaTextPoint <- reactive({
    paste("mpg ~", "as.integer(", input$variable, ")")
  })

  fit <- reactive({
    lm(as.formula(formulaTextPoint()), data=mpgData)
  })

  output$caption <- renderText({
    formulaText()
  })

  output$mpgBoxPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = mpgData,
            outline = input$outliers)
  })

  output$fit <- renderPrint({
    summary(fit())
  })

  output$mpgPlot <- renderPlot({
    with(mpgData, {
      plot(as.formula(formulaTextPoint()))
      abline(fit(), col=2)
    })
  })

})


