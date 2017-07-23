

library(shiny)
library(sentiment)

function(input, output, session) {

  

    statuses <- reactive({
        
        input$update
        
        isolate({
            withProgress({
                setProgress(message = "Gathering tweets...")
                getTweets(input$searchString, input$numTweets, input$date1,input$date2,input$adress,input$radius,input$language,input$resultType)
            })
        })
    })
    
    textdata <- reactive({
        
        input$update
        
        getTextData(statuses())
    })
    
    sentiments <- reactive({
        
        input$update
        
        #isolate({
            withProgress({
                setProgress(message = "Gathering sentiments...")
                sentiments <- getSentiments(textdata())
            })
        #})
    })
    
    
    ########################################## user timeline ############
    statuses2 <- reactive({

      input$update2

      isolate({
        withProgress({
          getTimeline(input$searchString2, input$numTweets2)
        })
      })
    })
    
    
    statuses2_1 <- reactive({

      input$update2

      isolate({
        withProgress({
          get_user(input$searchString2)
        })
      })
    })
    
    textdata2 <- reactive({
      input$update2

      getTextData2(statuses2())
    })
    
    
    sentiments2 <- reactive({
      input$update2
      withProgress({

        sentiments2 <- getSentiments2(textdata2())
      })
      
    })
  
    
    statuses3 <- reactive({
      input$update3

      isolate({
        withProgress({
          getStream(input$searchString4,input$searchString5, input$seconds)
          
        })
      })
    })  
    
    
    
    ################ first tab ################
    library(DT)
    output$data_table <- renderDataTable({
      
      

      if(is.null(input$dataset))
        return()
      
      dat = input$dataset
      
    
      if (is.null(input$columns) )return()
      
      
      city=input$columns
      Locs_country = subset(Locs, country == dat)
      woeidcity = subset(Locs_country, name == city)$woeid
      
      
      trends = getTrends(woeid=woeidcity)
      trends$url <- paste0("<a href='",trends$url,"' target='_blank'>",trends$url,"</a>")
      
      
      return (head(trends[1:2],50))
      
    }, escape = FALSE)
    
    
    
    
    
    
    
      
    output$data_table_streaming = renderDataTable({
        df_text3 <- statuses3()
        
        df_text3
        
        
      })
    
    
    output$plotstreamfiltered <- renderPlot({
      df_text3 <- statuses3()
      ts_plot(df_text3, by = "secs", filter = c(input$searchString4,input$searchString5),theme = 2, cols = c("#6699ee", "#dd7a7a"),
              main = "Stream Filtred")
      
      
      
    })
    
    
    ##################### for user timeline ###############################
    
    output$data_table_user_info  <- renderTable({
      df_text3 <- statuses2_1()
      df_text3
      
      
      
    })
    
    
    ############ hastags ############
    output$hashtag <- renderPlot({
      
      df_text3 <- statuses2()
      
      
      extract.hashes = function(vec){
        
        hash.pattern = "#[[:alpha:]]+"
        have.hash = grep(x = vec, pattern = hash.pattern)
        
        hash.matches = gregexpr(pattern = hash.pattern,
                                text = vec[have.hash])
        extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
        
        df = data.frame(table(tolower(unlist(extracted.hash))))
        colnames(df) = c("tag","freq")
        df = df[order(df$freq,decreasing = TRUE),]
        return(df)
      }
      
      
      dat = extract.hashes(df_text3$Text)
      dat2 = transform(dat,tag = reorder(tag,freq))
      dat3=(head(dat2,6))
      
      ggplot(data = dat3, aes(x = tag, y = freq)) +
        geom_bar(aes(fill = tag), stat = "identity") +
        xlab("Tag") + ylab("Total Count") + 
        scale_fill_brewer(palette='RdBu') + 
        theme_bw() + theme(legend.position='none')+
        ggtitle('Hash Tag Analysis of User' ) +
        theme(plot.title = element_text(size=15, face='bold'))
    })
    
    
    
    
    output$data_table_user1  <- renderDataTable({
      df_text2 <- statuses2()
      
      df_text2
      
      
    })
    
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('stats', '.csv', sep='')
      },
      
      content = function(file) {
        data <- statuses2()
        write.csv(data, file)
      }
    )
    
    #### Make the wordcloud for user timeline
    wordcloud_rep2 <- repeatable(wordcloud)
    
    output$plot_user <- renderPlot({
      wordcloud_rep2(textdata2(), scale=c(10,0.5),
                     min.freq=3, max.words=50,
                     colors=brewer.pal(8, "RdBu"), random.order=F, 
                     rot.per=0.1, use.r.layout=F)
    })
    
    
    
    ###### user timeline ######
    output$sentiment_user <- renderPlot({
      v <- sentiments2()
      emotions <- data.frame("count"=colSums(v[,c(1:8)]))
      emotions <- cbind("sentiment" = rownames(emotions), emotions)
      ggplot(data = emotions, aes(x = sentiment, y = count)) +
        geom_bar(aes(fill = sentiment), stat = "identity") +
        xlab("Sentiment") + ylab("Total Count") + 
        scale_fill_brewer(palette='RdBu') + 
        theme_bw() + theme(legend.position='none')+
        ggtitle('Sentiment Analysis of Tweets') +
        theme(plot.title = element_text(size=15, face='bold'))
    })
    

  
    output$polarityTable_user <- renderDataTable({
      some_txt <- textdata2()
      
      
      ##### classify polarity ####
      class_pol = classify_polarity(some_txt, algorithm='bayes')
      # get polarity best fit
      polarity = class_pol[,4]
      
      
      sent_df = data.frame(text=some_txt,polarity=polarity, stringsAsFactors=FALSE)
      sent_df
      
      
    })
    
    
    output$polarity_user <- renderPlot({
      
      some_txt <- textdata2()
      
      # classify polarity
      class_pol = classify_polarity(some_txt, algorithm='bayes')
      # get polarity best fit
      polarity = class_pol[,4]
      sent_df = data.frame(text=some_txt,polarity=polarity, stringsAsFactors=FALSE)
      sent_df
      ggplot(sent_df, aes(x=polarity)) +
        geom_bar(aes(y=..count.., fill=polarity)) +
        scale_fill_brewer(palette='RdGy') +
        labs(x='polarity categories', y='number of tweets') +
        ggtitle('Polarity Analysis of Tweets') +
        theme(plot.title = element_text(size=15, face='bold'))
    })
   
    
    
    
    ###################
    output$data_table2  <- renderDataTable({
      
      df_text <- statuses()
      
      df_text
    
      
    })
    
    #######################################
    # Make the wordcloud for tweets
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
        wordcloud_rep(textdata(), scale=c(10,0.5),
                      min.freq=3, max.words=50,
                      colors=brewer.pal(8, "RdBu"), random.order=F, 
                      rot.per=0.1, use.r.layout=F)
    })
    
    output$tweetCount  <- renderText({
        df <- statuses() 
      

        paste("Number of Tweets Found: ", as.character(nrow(df)))
        
        
    })
    
  
    output$sentiment <- renderPlot({
        v <- sentiments()
        emotions <- data.frame("count"=colSums(v[,c(1:8)]))
        emotions <- cbind("sentiment" = rownames(emotions), emotions)
        ggplot(data = emotions, aes(x = sentiment, y = count)) +
            geom_bar(aes(fill = sentiment), stat = "identity") +
            xlab("Sentiment") + ylab("Total Count") + 
            scale_fill_brewer(palette='RdBu') + 
            theme_bw() + theme(legend.position='none')+
          ggtitle('Sentiment Analysis of Tweets') +
          theme(plot.title = element_text(size=15, face='bold'))
    })
    
   
    
    
    
    output$polarityTable <- renderDataTable({

      
      some_txt <- textdata()
      
      
      # classify polarity
      class_pol = classify_polarity(some_txt, algorithm='bayes')
      # get polarity best fit
      polarity = class_pol[,4]
      
      
      sent_df = data.frame(text=some_txt,polarity=polarity, stringsAsFactors=FALSE)
      sent_df

      
      
      
      
    })
    
    
    output$polarity <- renderPlot({
      
      some_txt <- textdata()
      
      # classify polarity
      class_pol = classify_polarity(some_txt, algorithm='bayes')
      # get polarity best fit
      polarity = class_pol[,4]
      sent_df = data.frame(text=some_txt,polarity=polarity, stringsAsFactors=FALSE)
      sent_df
      ggplot(sent_df, aes(x=polarity)) +
        geom_bar(aes(y=..count.., fill=polarity)) +
        scale_fill_brewer(palette='RdGy') +
        labs(x='polarity categories', y='number of tweets') +
        ggtitle('Polarity Analysis of Tweets') +
        theme(plot.title = element_text(size=15, face='bold'))
    })
    
    
    
    
    
    
    
    
    
    
    output$TrendsWv = renderTable({
      
      Locs = availableTrendLocations()
      country_count=sqldf('select  count(*) as NumberOfCities, country from Locs group by country order by NumberOfCities desc limit 10 ')
      country_count})

    
    
    

    ######################## get Trends
    
    data_sets = sqldf('select  country from country_count ')
    
    # Drop-down selection box forcountry
    output$choose_dataset <- renderUI({
      selectInput("dataset", "Country", as.list(data_sets))
    })
    
    # city
    output$choose_columns <- renderUI({
      # If missing input, return to avoid error later in function
      if(is.null(input$dataset))
        return()
      
      counttry = input$dataset

      colnames =sqldf(sprintf("select name from Locs where country = '%s'", counttry))
      
      # Create the cities
      selectInput("columns", "City", choices = colnames)
    })
    
    
  
    
    
    
    
    
    
    #######################
    # Create a reactive leaflet map for Radius
    library(ggmap)  
    mapTweets <- reactive({
      
      a=geocode(input$adress, source="google")
      ad=as.String(input$adress)
      lo=a$lon
      la=a$lat
      r= (input$radius) *1000
      
      map = leaflet(a) %>% addTiles() %>%
        setView(lng = lo, lat = la , zoom = 12)%>%
        addCircles(lng = ~a$lon, lat = ~a$lat, weight = 2,radius = r)%>%
       addMarkers(as.numeric(lo), as.numeric(la), popup = ad) #%>%
     
       
    })
    output$myMap = renderLeaflet(mapTweets())
    
    
   
      
    
    
    
    
       
}