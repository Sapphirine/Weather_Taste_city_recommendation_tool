server <- function(input, output, clientData, session) {
  
  ############ homepage tab ###########
  output$hp <- renderUI({
    tags$iframe(
      srcdoc = paste(readLines('try.html'), collapse = '\n'),
      width = "100%",
      height = "1000px"
    )
  })
  
  ############## table tab ##############
  
  output$ziptable <- DT::renderDataTable({
      weather_state <- weather %>%
        filter(state %in% input$states)
      date_begin <- month(strptime(as.character(input$data1),format = "%Y-%m-%d")) * 100 + day(strptime(as.character(input$data1),format = "%Y-%m-%d"))
      date_end <- month(strptime(as.character(input$data2),format = "%Y-%m-%d")) * 100 + day(strptime(as.character(input$data2),format = "%Y-%m-%d"))
      if (date_begin <= date_end){
        weather_date <- weather_state[which(weather_state$month_day>=date_begin & weather_state$month_day<=date_end),]
      }else{
        weather_date <- weather_state[which((weather_state$month_day>=date_begin & weather_state$month_day<=1231)|(weather_state$month_day>=101 & weather_state$month_day<=date_end)),]
      }
     this.user <- user_data %>%
       filter(userID==as.character(input$text))
     weather_date <- merge(weather_date, this.user[,c("weather_type","rate")], by = "weather_type")
     my_count <- aggregate(weather_date$rate, list(weather_date$station), mean)
     names(my_count) <- c("station", "score")
     my_count <- my_count[order(my_count$score, decreasing = TRUE),]
     rownames(my_count) <- NULL
     action <- DT::dataTableAjax(session, my_count)
     DT::datatable(my_count, options = list(ajax = list(url = action)),escape = FALSE)
    
  },rownames=FALSE)
  
  ############# graph plots1 ##########
  output$user_graph <- renderPlot({
    this.user <- user_data[which(user_data$userID==as.character(input$text)),]
    this.user$is_self <- as.factor(this.user$is_self)
    ggplot(data=this.user, aes(x=as.factor(weather_type), y=as.numeric(rate),fill=is_self)) +
      ggtitle(sprintf("%s's Weather Taste", this.user$userID)) +
      #geom_bar(stat="identity", fill=c("#999999", "#56B4E9")[this.user$is_self+1]) +
      geom_bar(stat="identity") +
      scale_fill_manual(values = c("#999999", "#56B4E9"),name="Data Scource",breaks =c(0,1),labels = c("Predicted","User Rated"))+
      geom_text(aes(label=round(rate,1)), vjust=1.6, color="white", size=3.5) +
      #scale_fill_manual(values = c("#999999", "#56B4E9")[this.user$is_self+1])+
      ylim(0,5) + theme(plot.title = element_text(size = rel(2.5),hjust = 0.5))
  })
 
  ############# graph plots2 ##########
  output$user_station<- renderPlot({
    weather_state <- weather %>%
      filter(state %in% input$states)
    date_begin <- month(strptime(as.character(input$data1),format = "%Y-%m-%d")) * 100 + day(strptime(as.character(input$data1),format = "%Y-%m-%d"))
    date_end <- month(strptime(as.character(input$data2),format = "%Y-%m-%d")) * 100 + day(strptime(as.character(input$data2),format = "%Y-%m-%d"))
    if (date_begin <= date_end){
      weather_date <- weather_state[which(weather_state$month_day>=date_begin & weather_state$month_day<=date_end),]
    }else{
      weather_date <- weather_state[which((weather_state$month_day>=date_begin & weather_state$month_day<=1231)|(weather_state$month_day>=101 & weather_state$month_day<=date_end)),]
    }
    this.user <- user_data %>%
      filter(userID==as.character(input$text))
    weather_date <- merge(weather_date, this.user[,c("weather_type","rate")], by = "weather_type")
    my_count <- aggregate(weather_date$rate, list(weather_date$station), mean)
    names(my_count) <- c("station", "score")
    my_count <- my_count[order(my_count$score, decreasing = TRUE),]
    chosen_station <- as.character(my_count$station[as.integer(input$rank)])
    this.city <- weather_date[which(weather_date$station==chosen_station),]
    this.city.count <- ddply(this.city, .(weather_type),nrow)
    create_table <- data.frame(weather_type = c(1:10))
    create_table <- merge(create_table, this.city.count, by = "weather_type", all.x = TRUE)
    names(create_table)[2] <- "Occurence"
    create_table$Occurence <- create_table$Occurence / sum(create_table$Occurence, na.rm = TRUE)
    barplot(create_table$Occurence,ylim = c(0,1))
    #ggplot(create_table)
    ggplot(data=create_table, aes(x=as.factor(weather_type), y=as.numeric(Occurence))) +
      ggtitle(sprintf("Weather type occurence in %s", chosen_station)) +
      geom_bar(stat="identity", fill=c("#56B4E9")) +
      geom_text(aes(label=round(Occurence,3)), vjust=1.6, color="white", size=3.5) +
      ylim(0,1) + theme(plot.title = element_text(size = rel(2.5),hjust = 0.5))
  
  })
  
  
  ############## map tab ############## 
  
  output$analysis1 <- renderPlot({
    this.user <- user_data[which(user_data$userID==as.character(input$text1)),]
    this.user$is_self <- as.factor(this.user$is_self)
       ggplot(data=this.user, aes(x=as.factor(weather_type), y=as.numeric(rate),fill=is_self)) +
           ggtitle(sprintf("%s's Weather Taste", this.user$userID)) +
          #geom_bar(stat="identity", fill=c("#999999", "#56B4E9")[this.user$is_self+1]) +
          geom_bar(stat = "identity") +
         scale_fill_manual(values = c("#999999", "#56B4E9"),name="Data Scource",breaks =c(0,1),labels = c("Predicted","User Rated"))+
          geom_text(aes(label=round(rate,1)), vjust=1.6, color="white", size=3.5) +
         ylim(0,5) + theme(plot.title = element_text(size = rel(2.5),hjust = 0.5))
         
  })
  
  output$table <-DT::renderDataTable({
    this.record <- pseudo_record[which(pseudo_record$userID==as.character(input$text1)),]
    this.record <- arrange(this.record, Year, Month, Day)
    action <- DT::dataTableAjax(session, this.record)
    DT::datatable(this.record, options = list(ajax = list(url = action)),escape = FALSE)
    
  })
  
  output$table1 <- DT::renderDataTable({
    weather_state <- weather %>%
      filter(state %in% input$states)
    date_begin <- month(strptime(as.character(input$data1),format = "%Y-%m-%d")) * 100 + day(strptime(as.character(input$data1),format = "%Y-%m-%d"))
    date_end <- month(strptime(as.character(input$data2),format = "%Y-%m-%d")) * 100 + day(strptime(as.character(input$data2),format = "%Y-%m-%d"))
    if (date_begin <= date_end){
      weather_date <- weather_state[which(weather_state$month_day>=date_begin & weather_state$month_day<=date_end),]
    }else{
      weather_date <- weather_state[which((weather_state$month_day>=date_begin & weather_state$month_day<=1231)|(weather_state$month_day>=101 & weather_state$month_day<=date_end)),]
    }
    this.user <- user_data %>%
      filter(userID==as.character(input$text))
    weather_date <- merge(weather_date, this.user[,c("weather_type","rate")], by = "weather_type")
    my_count <- aggregate(weather_date$rate, list(weather_date$station), mean)
    names(my_count) <- c("station", "score")
    my_count <- my_count[order(my_count$score, decreasing = TRUE),]
    chosen_place <- station_list[which(station_list$airportCode %in% my_count$station),]
    action <- DT::dataTableAjax(session, chosen_place)
    DT::datatable(chosen_place, options = list(ajax = list(url = action)),escape = FALSE)
  })
  
  
  output$map <- renderLeaflet({
    weather_state <- weather %>%
      filter(state %in% input$states)
    date_begin <- month(strptime(as.character(input$data1),format = "%Y-%m-%d")) * 100 + day(strptime(as.character(input$data1),format = "%Y-%m-%d"))
    date_end <- month(strptime(as.character(input$data2),format = "%Y-%m-%d")) * 100 + day(strptime(as.character(input$data2),format = "%Y-%m-%d"))
    if (date_begin <= date_end){
      weather_date <- weather_state[which(weather_state$month_day>=date_begin & weather_state$month_day<=date_end),]
    }else{
      weather_date <- weather_state[which((weather_state$month_day>=date_begin & weather_state$month_day<=1231)|(weather_state$month_day>=101 & weather_state$month_day<=date_end)),]
    }
    this.user <- user_data %>%
      filter(userID==as.character(input$text))
    weather_date <- merge(weather_date, this.user[,c("weather_type","rate")], by = "weather_type")
    my_count <- aggregate(weather_date$rate, list(weather_date$station), mean)
    names(my_count) <- c("station", "score")
    my_count <- my_count[order(my_count$score, decreasing = TRUE),]
    chosen_number <- my_count[1:input$num,]
    chosen_place <- station_url[which(station_url$airportCode %in% chosen_number$station),]
    #icon.fa <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', prefix='fa', iconColor = 'black')
    leaflet(data = chosen_place) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      addMarkers(~Lon, ~Lat, popup=paste("Station:",chosen_place$Station, ",State:",chosen_place$State,
                                         ",Elevation:",chosen_place$Elevation,",Url:",chosen_place[,8]))
  })
  
}
