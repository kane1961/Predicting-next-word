#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)





ui <- fluidPage(
  
  
  
  pageWithSidebar(
    
    headerPanel("Predicting the next word"),
    
    # Select inputs
    
    sidebarPanel(
      
      textInput("userInput", "word or sentence:", value="Welcome!"),
      
      
      submitButton("Submit"),
      
      br(),
      
      h1("Instructions", align = "center", style = "color:blue"),
      h4("Please be patient when loading first time , it could take 5-10 seconds", align = "left"),
      br(),
      h4("1. Type the word or the sentence in the box and submit", align = "left"),
      h4("2. The next word suggested where obtained analyzing the previous 1 to 3 words, through the back model.", align = "left"),
      h4("2. The next word suggested considered the less common, recommended, or in the second case the -stop words-, auxiliary.", align = "left"),
      h6("NA: word non available", align = "left")
    ),
    
    
    
    
    
    mainPanel(
      
      # use tab format to display outputs
      
      tabsetPanel(
        
        
        
        
        # tab 1       
        
        tabPanel("Word Prediction",
                 
                 mainPanel(
                   
                   p("The following are the predicted words from your word or sentence input.  This first list is for more significant words and the second list considered the stop words or more common words."),
                   
                   h1("Recomended Words", align = "center", style = "color:blue"),
                   h1(textOutput("w1"), align = "center"),
                   br(),
                   br(),
                   br(),
                   h1("Auxiliary Words", align = "center", style = "color:blue"),
                   h1(textOutput("w2"), align = "center")))
        
        
        
        
      )
      
    )
    
  )
  
)

library(tidytext)

library(tidyr) 

library(dplyr) 

library(stringr)

library(tm)


Gram2<- readRDS("Gram2s_r2")
Gram3<- readRDS("Gram3s_r2")
Gram4<- readRDS("Gram4s_r2")

server<-(function(input, output) {
  
  text1 <- reactive ({
    
    
    input <- input$userInput
    
    
    input <- VCorpus(VectorSource(input))
    
    input<-tm_map(input, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
    
    ## Extra whitespace is eliminated by:
    
    input <- tm_map(input, stripWhitespace)
    
    ## Remove punctuation marks from a text document.
    input <- tm_map(input, removePunctuation)
    
    ## Remove numbers
    input <- tm_map(input, removeNumbers) 
    
    ## Transform capital letters
    input <- tm_map(input, content_transformer(tolower))
    
    
    l<-strsplit(input[[1]]$content, " ")
    
    # get length of user input
    
    
    i<-length(l[[1]])
    
    x4<-0
    x3<-0
    
    if (i > 2){
      
      
      
      input4 <- word(input[[1]]$content, -3,-1)
      
      
      # input for search
      
      input4 <- paste(input4, sep=" ", collapse= " ")
      
      sugg4<-Gram4 %>%  filter(input == input4, n>1)
      x4<-length(sugg4$word)
      
      #     if (x<2)
      #    { sugg$word[1]}
    }
    
    
    
    if (i>1) {  input3 <- word(input[[1]]$content, -2,-1)
    
    input3 <- paste(input3, sep=" ", collapse= " ")
    
    sugg3<-Gram3 %>%  filter(input == input3, n>1)
    
    x3<-length(sugg3$word)}
    
    #  if (x==0) 
    
    # { sugg$word[1:6]}
    
    { input2 <- word(input[[1]]$content, -1)
      
      sugg2<-Gram2 %>%  filter(input == input2)
      
      x2<-length(sugg2$word)}
    
    if (x4>1) { sugg<-sugg4}
    if (x3>1 & x4<1) { sugg<-sugg3} 
    if (x2>1 & x3<1 ) { sugg<-sugg2}
    
    #    if (x3>1) { sugg<-sugg3}
    #   if (x2>1) { sugg<-sugg2}
    
    w1<-( sugg%>% filter(!word %in% stop_words$word))
    #    w2<-na.omit( sugg%>% filter(word %in% stop_words$word))
    
    x<-length(w1$word)
    if (x>6) {x=6}
    # else{ if(x==0) {w1$word[1]<-"" }}
    
    
    #    w1<-sugg
    w1<-w1$word[1:x]
    
    #    x<-length(w2$word)
    #    if (x==0) { w2<-"No suggestion"}
    #    w2<-w2$word[1:6] 
    
    
    
    
    
    #    source.df$predict[1:10]
    w1
    
  })
  
  
  
  output$w1 <- renderText({
    
    text1()
    
  })
  #=================================================
  text2 <- reactive ({
    
    
    input <- input$userInput
    
    
    input <- VCorpus(VectorSource(input))
    
    input<-tm_map(input, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
    
    ## Extra whitespace is eliminated by:
    
    input <- tm_map(input, stripWhitespace)
    
    ## Remove punctuation marks from a text document.
    input <- tm_map(input, removePunctuation)
    
    ## Remove numbers
    input <- tm_map(input, removeNumbers) 
    
    ## Transform capital letters
    input <- tm_map(input, content_transformer(tolower))
    
    
    l<-strsplit(input[[1]]$content, " ")
    
    # get length of user input
    
    
    i<-length(l[[1]])
    
    x4<-0
    x3<-0
    
    if (i > 2){
      
      
      
      input4 <- word(input[[1]]$content, -3,-1)
      
      
      # input for search
      
      input4 <- paste(input4, sep=" ", collapse= " ")
      
      sugg4<-Gram4 %>%  filter(input == input4, n>1)
      x4<-length(sugg4$word)
      
      #     if (x<2)
      #    { sugg$word[1]}
    }
    
    
    
    if (i>1) {  input3 <- word(input[[1]]$content, -2,-1)
    
    input3 <- paste(input3, sep=" ", collapse= " ")
    
    sugg3<-Gram3 %>%  filter(input == input3, n>1)
    
    x3<-length(sugg3$word)}
    
    #  if (x==0) 
    
    # { sugg$word[1:6]}
    
    { input2 <- word(input[[1]]$content, -1)
      
      sugg2<-Gram2 %>%  filter(input == input2)
      
      x2<-length(sugg2$word)}
    
    if (x4>1) { sugg<-sugg4}
    if (x3>1) { sugg<-sugg3} 
    if (x2>1) { sugg<-sugg2}
    
    #    if (x3>1) { sugg<-sugg3}
    #   if (x2>1) { sugg<-sugg2}
    
    # w1<-na.omit( sugg%>% filter(!word %in% stop_words$word))
    w2<-( sugg%>% filter(word %in% stop_words$word))
    
    x<-length(w2$word)
    if (x>6) {x<-6}
    
    #    w1<-sugg
    #w1<-w1$word[1:10]
    
    #    x<-length(w2$word)
    #    if (x==0) { w2<-"No suggestion"}
    w2<-w2$word[1:x] 
    
    
    
    
    
    #    source.df$predict[1:10]
    w2
    
  })
  #+============================================== 
  output$w2 <- renderText({
    
    text2()
    
  })
})

# Run the application 
shinyApp(ui = ui, server = server)