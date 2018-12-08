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
  
  output$dataDescription <- renderText({
    chr.description <- "We obtained our data from theDataMapT, a non-profit organization focused on documenting all 
                        the places and entities that our personal data gets transferred to and from. theDataMapT operates 
                        as a research project in the Data Privacy Lab, which is a program in the Institute for Quantitative 
                        Social Science at Harvard University. Interested in seeing what types of analysis and insight students 
                        and researchers could garner about data breaches, theDataMapT is hosting a competition where they have 
                        released company breach data and is asking researchers to come up with novel insights from the data. 
                        Our team chose to utilize these datasets for our EDAV final project, which can be accessed here: 
                        https://thedatamap.org/contests/materials.php. We will also be submitting our final analysis to 
                        theDataMapT's competition. We hope that our exploratory data analysis can help promote greater awareness 
                        to the academic community and our fellow classmates at Columbia University regarding where and why 
                        data breaches occur."
    
    return (chr.description)
  })
  
  output$dataPreProcessing <- renderText({
    chr.description <- "We decided to merge the four original datasets so that we could create a final, comprehensive data sets with 
                        all of the important attributes that we wanted to analyze. First, we merged prc_breaches with catorgs using 
                        the a composite key made up of cat_id and org_id, which both datasets had. Next, we merged this intermediary 
                        data set with the categories dataset on cat_id. Lastly, we merged this output with the orgsindex dataset on org_id. 
                        Our final dataset contained 4126 rows and 19 columns."
    
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
      
    dt.click_event_industry_breach <- data.table(event_data("plotly_click", source = "industryBreach"))

    if (nrow(dt.click_event_industry_breach) > 1){
      chr.industry <- rev(vec_cat)[unique(dt.click_event_industry_breach$y)]
      dt.plot.this <- dt.data_temp[cat_name == chr.industry][,list(breach_type, breach_type_name, records_breached)]
      dt.plot.this <- dt.plot.this[,list(records_breached = sum(records_breached)), by = list(breach_type, breach_type_name)]
      dt.plot.this$breach_type <- factor(x = dt.plot.this$breach_type,
                                         levels = dt.plot.this[order(records_breached, decreasing = TRUE)]$breach_type)
      dt.plot.this$breach_type_name <- factor(x = dt.plot.this$breach_type_name,
                                         levels = dt.plot.this[order(records_breached, decreasing = TRUE)]$breach_type_name)
      
      # If the proportion of largest to smallest breach type instances is greater than 100, then use log scales
      plt <- ggplot(dt.plot.this, aes(x = breach_type, 
                                      y = records_breached, 
                                      fill = breach_type_name,
                                      text = paste0("Breach type name: ", breach_type_name,
                                                    "\nRecords Breached: ", prettyNum(records_breached, scientific=FALSE, big.mark=",")))) + 
        geom_bar(stat = "identity") + theme_bw(base_size = 15) + 
        ggtitle(paste0("Total Records Breached for ", chr.industry, " by Breach Type")) + 
        xlab("Breach Type") + ylab("Total Records Breached") + scale_fill_colorblind() + scale_y_log10()
      
      ggplotly(plt, source = "industryBreach", tooltip = c("text"))
      
      
    } else{
      plt <- ggplot(data = cat_records_breached[records_breached < 100000],aes(x = cat_name, y =records_breached)) + 
        geom_boxplot(fill = "lightblue") + 
        ggtitle("Distribution of Records Breached Across Category/Industry") + 
        ylab("Number of records breached") + xlab("") + scale_y_continuous(labels = scales::comma) + 
        theme_bw(base_size = 15) + coord_flip()
      ggplotly(plt, source = "industryBreach")
    }
  })
  
  output$sourceBreach <- renderPlot({
    dt.breach_source_count <- dt.data[,.N, by = list(breach_source)]
    dt.breach_source_count$breach_source <- factor(dt.breach_source_count$breach_source,
                                                   levels = dt.breach_source_count[order(N, decreasing = FALSE)]$breach_source)
    ggplot(dt.breach_source_count, aes(x = breach_source, y = N)) + geom_bar(stat = "identity") + 
      xlab("Breach Source") + ylab("Frequency of Breach Source") + theme_bw(base_size = 15) + 
      ggtitle("Frequency of Source of Breaches") + coord_flip()
  })
  
  output$annualBreaches <- renderPlot({
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
    dt.plot
    
  })
  
  output$breachState <- renderPlot({
      dt.states <- dt.data[nchar(state) > 2][,list(dt, state, breach_id, breach_type_name)]
      
      dt.map <- dt.states[,.N, by = list(state, breach_type_name)]
      dt.map <- dt.map[state %in% state.name]
      colnames(dt.map) <- c("state", "breach_type_name", "breach_instance")
      
      dt.map_total <- dt.states[,.N, by = list(state)]
      dt.map_total <- dt.map_total[state %in% state.name]
      colnames(dt.map_total) <- c("state", "breach_instance")
      
      dt.map$state <- factor(dt.map$state,levels = dt.map_total[order(breach_instance, decreasing = TRUE)]$state)
      
      dt.map <- dt.map[state %in% head(dt.map_total[order(breach_instance, decreasing = TRUE)]$state, 5)]
      
      ggplot(dt.map, aes(x = state, y = breach_instance, fill = breach_type_name)) + 
        geom_bar(stat = "identity", position = "dodge") + 
        theme_bw(base_size = 15) +
        xlab("State") + ylab("Breach Instance") + scale_fill_colorblind()
      
  })
  
  output$breachScatterPlot <- renderPlotly({
    # State population data
    dt.states <- dt.data[nchar(state) > 2][,list(dt, state, breach_id)]
    dt.map <- dt.states[,.N, by = state]
    dt.map <- dt.map[state %in% state.name]
    dt.merged_data <- merge(dt.state_population, dt.map, by = c("state"))
    dt.merged_data[,population := population / 1000000]
    
    dt.click_event_breach_scatter <- data.table(event_data("plotly_click", source = "breachScatterPlot"))
    
    if (nrow(dt.click_event_breach_scatter)){
      if (dt.click_event_breach_scatter$y%%1==0){
        chr.state <- dt.merged_data$state[dt.click_event_breach_scatter$pointNumber + 1]
        dt.data_temp <- dt.data[state == chr.state & !is.na(records_breached)][,list(category,
                                                                                     records_breached)]
        
        dt.data_temp <- dt.data_temp[,list(records_breached = sum(records_breached)), by = list(category)]
        
        dt.data_temp$category <- factor(dt.data_temp$category, levels = dt.data_temp[order(records_breached, decreasing = TRUE)]$category)
        
        plt <- ggplot(dt.data_temp, aes(x = category, y = records_breached,
                                        text = paste0("Category: ", category,
                                                      "\nRecords Breached: ", prettyNum(records_breached, scientific=FALSE, big.mark=",")))) +
          geom_bar(stat = "identity", position = "dodge", fill = "lightblue") + 
          xlab("Category") + ylab("Number of records breached") +
          theme_bw(base_size = 15) + scale_y_log10() + 
          ggtitle(glue("Number of records breached by category for {chr.state}")) + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        ggplotly(plt, tooltip = c("text"), source = "breachScatterPlot")
      } else{
        plt <- ggplot(dt.merged_data, aes(x = population, y = N, 
                                          text = paste0("State: ", state,
                                                        "\nState population: ", population,
                                                        "\nNumber of Breach Instance: ", N))) + geom_point() + 
          xlab("State Population (in millions)") + ylab("Number of Breach Instance") + theme_bw(base_size = 15) +
          ggtitle("Number of breach instances vs State Population")
        
        ggplotly(plt, tooltip = c("text"), source = "breachScatterPlot")
      }
      
    } else{
      plt <- ggplot(dt.merged_data, aes(x = population, y = N, 
                                        text = paste0("State: ", state,
                                                      "\nState population: ", population,
                                                      "\nNumber of Breach Instance: ", N))) + geom_point() + 
        xlab("State Population (in millions)") + ylab("Number of Breach Instance") + theme_bw(base_size = 15) +
        ggtitle("Number of breach instances vs State Population")
      
      ggplotly(plt, tooltip = c("text"), source = "breachScatterPlot")
    }
  })
  
  output$breachBoxplotState <- renderPlot({
    dt.states <- dt.data[nchar(state) > 2][,list(dt, state, records_breached)]
    dt.states <- dt.states[!is.na(records_breached)]
    ggplot(data = dt.states[records_breached < 100000],
           aes(x = reorder(state,records_breached,FUN =median), y =records_breached)) + geom_boxplot() + 
      coord_flip() + ggtitle("Distribution of Records Breached Across States") + 
      ylab("Number of Records Breached") + xlab("") + theme_bw(base_size = 15)
    

  })
  
  output$missingData <- renderPlot({
    visna(dt.data, sort = "r")
  })
  
  output$missingDataFrequency <- renderPlot({
    # Yimin frequency bar plot
    dt.frequency_bar_plot <- dt.data[,list(records_breached, breach_type_name)]
    dt.frequency_bar_plot[,is_records_breached_na := is.na(records_breached)]
    
    dt.frequency_bar_plot_total <- dt.frequency_bar_plot[,.N, by = list(breach_type_name)]
    colnames(dt.frequency_bar_plot_total) <- c("breach_type_name", "total")
    
    dt.frequency_bar_plot_count <- dt.frequency_bar_plot[,.N, by = list(breach_type_name, is_records_breached_na)]

    dt.frequency_bar_plot_count <- merge(dt.frequency_bar_plot_count, dt.frequency_bar_plot_total, by = c("breach_type_name"))
    dt.frequency_bar_plot_count[,percentage := N / total]
    
    dt.order.this <- dt.frequency_bar_plot_count[is_records_breached_na == TRUE][order(percentage, decreasing = TRUE)]
    
    dt.frequency_bar_plot$breach_type_name <- factor(dt.frequency_bar_plot$breach_type_name, 
                                                     levels = dt.order.this$breach_type_name)
    
    
    
    ggplot(data = dt.frequency_bar_plot) + 
      geom_mosaic(aes(x = product(is_records_breached_na, breach_type_name), fill = is_records_breached_na)) +
      scale_fill_brewer(palette = "Set1") + theme_bw(base_size = 15) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })

}
  