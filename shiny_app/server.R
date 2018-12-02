library(shiny)

server <- function(input, output, session) {
  output$projectDescription <- renderText({
    chr.description <- "In light of recent data breaches to major organizations such as Facebook and Equifax, 
                        companies are beginning to take extra precautions to protect their data. 
                        Data breaches are not only costly to recuperate from, but also hurt businesses and 
                        consumers in the long-run. This movement sparks great discussion about the types of 
                        companies that are typically targeted, when breaches occur, how the breaches are 
                        broadcasted to the public, and just how much of our data is at stake. 
                        Using United States data breaches information from 2005 to 2018, our group hopes to craft
                        a comprehensive story centered on these breaches and help the public better understand 
                        why they are happening through meaningful visualizations and interactivity."
    return (chr.description)
  })
  
  output$rawData <- renderDataTable({
    return (dt.data)
  })
  
  output$industryBreach <- renderPlotly({
    # Filtered by
    # Have actual records breach data
    # Number of instances for each category must be at least 5
    # Remove outlier
    dt.data_temp <- dt.data[!is.na(records_breached)]
    cat_records_breached <- dt.data_temp[,list(records_breached, cat_name)]
    
    # Count numbers of categories
    dt.cat_count <- cat_records_breached[,.N, by = list(cat_name)]
    dt.cat_count <- dt.cat_count[N >= 5]
    
    cat_records_breached <- cat_records_breached[cat_name %in% dt.cat_count$cat_name]
    vec_cat <- cat_records_breached[,list(med = median(records_breached)), by = list(cat_name)][order(med)]$cat_name
    
    cat_records_breached$cat_name <- factor(x = cat_records_breached$cat_name, levels = vec_cat)
      
    dt.click_event_industry_breach <- data.table(event_data("plotly_click"))
    
    if (nrow(dt.click_event_industry_breach)){
      chr.industry <- rev(vec_cat)[unique(dt.click_event_industry_breach$y)]
      dt.plot.this <- dt.data_temp[cat_name == chr.industry][,list(breach_type, records_breached)]
      dt.plot.this <- dt.plot.this[,list(records_breached = sum(records_breached)), by = list(breach_type)]
      dt.plot.this$breach_type <- factor(x = dt.plot.this$breach_type,
                                         levels = dt.plot.this[order(records_breached, decreasing = TRUE)]$breach_type)
      plt <- ggplot(dt.plot.this, aes(x = breach_type, y = records_breached)) + 
        geom_bar(stat = "identity", fill = "lightblue") + theme_bw(base_size = 15) + 
        ggtitle(paste0("Total Records Breached for ", chr.industry, " by Breach Type")) + 
        xlab("Breach Type") + ylab("Total Records Breached") + scale_y_log10()
        
      
    } else{
      plt <- ggplot(data = cat_records_breached[records_breached < 100000],aes(x = cat_name, y =records_breached)) + 
        geom_boxplot(fill = "lightblue") + 
        ggtitle("Distribution of Records Breached Across Category/Industry") + 
        ylab("# Records Breached") + xlab("") + scale_y_continuous(labels = scales::comma) + 
        theme_bw(base_size = 15) + coord_flip()
    }
    ggplotly(plt)
  })
  
  output$annualBreaches <- renderPlotly({
    dt.time_series <- dt.data[,list(dt, breach_id)]
    dt.time_series <- dt.time_series[order(dt)]
    
    if (input$quarterly){
      dt.quarterly <- dt.time_series
      dt.quarterly[,quarter := ifelse(month(dt) %in% c(1:3), "q1",
                                      ifelse(month(dt) %in% c(4:6), "q2",
                                             ifelse(month(dt) %in% c(7:9), "q3", "q4")))]
      dt.quarterly[,year := year(dt)]
      dt.plot.this <- dt.quarterly[,.N, by = list(quarter, year)]
      
      dt.plot <- ggplot(dt.plot.this, aes(x = year, y = N)) + 
        geom_bar(stat = "identity", fill = "lightblue") + 
        theme_bw(base_size = 15) + 
        xlab("Year") + ylab("Number of breach instances") + 
        ggtitle("Number of data breaches from 2005 until 2018") +
        facet_wrap(~quarter)
      
      
    } else{
      dt.year <- dt.time_series[,.N, by = year(dt)]
      dt.year[,year := as.Date(paste0(year, "-01-01"))]
      
      dt.plot <- ggplot(dt.year, aes(x = year, y = N)) + 
                  geom_bar(stat = "identity", fill = "lightblue") + 
                  theme_bw(base_size = 15) + 
                  xlab("Date") + ylab("Number of breach instances") + 
                  ggtitle("Number of data breaches from 2005 until 2018") +
                  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
    }
    ggplotly(dt.plot)
    
  })
  
  output$breachMap <- renderPlot({
    if (input$normalizePopulation){
      # State population data
      dt.state_population <- data.table(state.x77)[,list(State = V1, Population)]
      dt.state_population$State <- tolower(dt.state_population$State)
      dt.state_population$Population <- dt.state_population$Population * 1000
      dt.states <- dt.data[nchar(state) > 2][,list(dt, state, breach_id)]
      dt.map <- dt.states[,.N, by = state]
      dt.map <- dt.map[state %in% state.name]
      dt.map$state <- tolower(dt.map$state)
      dt.merged_data <- merge(dt.states, dt.map, by = c("state"))
      ggplot(dt.merged_data, aes(x = Population, y = N)) + geom_point() + 
        xlab("State Population") + ylab("Number of Breach Instance") + theme_bw(base_size = 12)
      
    } else{
      dt.states <- dt.data[nchar(state) > 2][,list(dt, state, breach_id)]
      dt.map <- dt.states[,.N, by = state]
      dt.map <- dt.map[state %in% state.name]
      dt.map$state <- tolower(dt.map$state)
      # Add DC
      dt.map <- rbind(dt.map,
                      data.table(state = "district of columbia",
                                 N = 0))
      colnames(dt.map) <- c("region", "value")
      state_choropleth(data.frame(dt.map),
                       "Number of data breach instances from 2005 to 2018")
    }
  })
  
  
  
}
  