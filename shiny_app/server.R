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
  
  output$industryBreach <- renderPlot({
    cat_records_breached <- dt.data %>% filter(records_breached <1000000& !is.na(records_breached)) %>% select(records_breached, cat_name)
    ggplot(data = cat_records_breached,aes(x = reorder(cat_name,records_breached,FUN =median), y =records_breached)) + 
      geom_boxplot() + coord_flip() + theme_bw() +
      ggtitle("Distribution of Records Breached Across Category/Industry") + 
      ylab("# Records Breached") + xlab("") + scale_y_continuous(labels = scales::comma)
    
  })
  
  output$annualBreaches <- renderPlotly({
    dt.time_series <- dt.data[,list(dt, breach_id)]
    dt.time_series <- dt.time_series[order(dt)]
    
    dt.year <- dt.time_series[,.N, by = year(dt)]
    dt.year[,year := as.Date(paste0(year, "-01-01"))]
    
    dt.plot <- ggplot(dt.year, aes(x = year, y = N)) + geom_bar(stat = "identity", fill = "lightblue") + theme_bw(base_size = 15) + xlab("Date") + ylab("Number of breach instances") + ggtitle("Number of data breaches from 2005 until 2018") + scale_x_date(date_breaks = "2 years")
    ggplotly(dt.plot)
  })
  
  output$breachMap <- renderPlot({
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
  })
  
  
  
}
  