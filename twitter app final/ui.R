
library(shiny)
library(sqldf)
library(leaflet)

numChoices <- c(100,250,500,1000, 2000, 3000, 5000)
numChoices2 <- c(500,1000, 2000, 3000, 5000)
numChoices3 <- c(3,5, 10, 30, 60, 300, 600)


 

Locs = availableTrendLocations()
country_count=sqldf('select  count(*) as NumberOfCities, country from Locs group by country order by NumberOfCities desc limit 10 ')
trendsChoices = sqldf('select  country from country_count ')

languages = c('en','de','es','tr')

radiuss =  c(1,3,5,10,20,30)

resulttype = c('recent','mixed','popular')

shinyUI(
  navbarPage("BIPM Twitter Analysis",
             
             tabPanel("Get Trends",
                      fluidPage(
                        
                        titlePanel("Trends Around The World"),
                        
                     
                        sidebarLayout(
                          sidebarPanel(
                            
              
                            uiOutput("choose_dataset"),
                            
                            uiOutput("choose_columns"),
                            
                            helpText("The top 10 countries which have the most number of trends listed dynamically")
                            
                           
                          ),
                          
                          mainPanel(
                            dataTableOutput("data_table"),
                            verbatimTextOutput("summary")
      
                          )
                        )
                        
                        
                        
                        
                        
                      )),
             
             
             
             
             tabPanel("Load Tweets",
                      fluidPage(
                        sidebarLayout(

                          sidebarPanel(
                            
                            textInput("searchString",
                                      "Search Twitter for",
                                      "Business Intelligence"),
                            
                            selectInput("numTweets", "# of Tweets",
                                        choices = numChoices),
                            
                            dateInput('date1',
                                      label = "From",
                                      value = as.character(Sys.Date()- 14),
                                      format = "yy/mm/dd",
                                      startview = 'year', language = 'en', weekstart = 1
                            ),
                            
                            dateInput('date2',
                                      label = "To",
                                      value = as.character(Sys.Date()),
                                      format = "yy/mm/dd",
                                      startview = 'year', language = 'en', weekstart = 1
                            ),
                            
                            textInput("adress",
                                      "Address",
                                      "Berlin"),
                            
                            
                            sliderInput("radius", "Radius km",
                                        min = 1, max = 10, value = 4
                            ),
                            
                            
                            selectInput("language", "Language",
                                        choices = languages),
                            
                            
                            selectInput("resultType", "Result Type",
                                        choices = resulttype),
                            
                            
                        
                            actionButton("update", "Search")
                          ),
                          
                          mainPanel( leafletOutput("myMap"),
                                    
                                    verbatimTextOutput("tweetCount"),
                                    dataTableOutput("data_table2")
                          )
                        )
                      )),
             
             
             tabPanel("Tweets Sentiments",
                      fluidPage(
                      
                        
                        
                        splitLayout(cellWidths = c("40%", "60%"),
                          plotOutput("sentiment"),
                          plotOutput("plot")
                        ),
                        HTML("<br><br>"),
                        
                        splitLayout(cellWidths = c("40%", "60%"),
                          plotOutput("polarity"),
                          dataTableOutput("polarityTable")
                        )
                        
                       
                      )),
             
             
             
             tabPanel("Get User Timeline",
                      fluidPage(
                        sidebarLayout(

                          sidebarPanel(

                            textInput("searchString2",
                                      "Username:",
                                      "rmmueller"),
                            
                            selectInput("numTweets2", "# of Tweets",
                                        choices = numChoices2),
                            helpText("You may like to search for: billgates, realDonaldTrump, barackobama  "),
                            
                            actionButton("update2", "Search"),
                            HTML("<br><br>"),
                            downloadLink("downloadData", "Download as csv")
                          ),
                          
                          mainPanel( 
                            
                            tableOutput("data_table_user_info"),
                                      HTML("<br><br>"),
                                      plotOutput("hashtag"),
                                      HTML("<br><br>"),
                                      dataTableOutput("data_table_user1")
                          )
                        )
                      )),
             
             
             
             tabPanel("User Sentiments",
                      fluidPage(
                        
                        
                        
                        splitLayout(cellWidths = c("40%", "60%"),
                                    plotOutput("sentiment_user"),
                                    plotOutput("plot_user")
                        ),
                        HTML("<br><br>"),
                        
                        splitLayout(cellWidths = c("40%", "60%"),
                                    plotOutput("polarity_user"),
                                    dataTableOutput("polarityTable_user")
                        )
                        
                        
                      )),
             
             
             
             
             tabPanel("Streaming",
                      fluidPage(
                        sidebarLayout(

                          sidebarPanel(

                            textInput("searchString4",
                                      "Get Stream For:",
                                      "trump"),
                            
                            textInput("searchString5",
                                      "Filter The Result For",
                                      "USA"),
                            
                            sliderInput("seconds", "How many seconds",
                                        min = 1, max = 60, value = 20
                            ),
                            
                            helpText("Streaming will stop automatically ..."),
                            
                            actionButton("update3", "Start Stream")
                           
                            
                          ),
                          
                          mainPanel( 
                            plotOutput("plotstreamfiltered"),
                            HTML("<br><br>"),
                            dataTableOutput("data_table_streaming")
                          )
                        )
                      ))
             
             
             
             
             
             
  )
)