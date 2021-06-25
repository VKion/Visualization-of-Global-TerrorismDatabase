library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(tidyverse)
library(forcats)
library(gridExtra)
library(ggmap)
library(ggthemes)
library(shiny)
library(plotly)
library(httr)
library(maps)
library(egg)
library(grid)
library(stringr)
library(dygraphs)
library(xts)  
library(lubridate)
library(shiny)
library(wordcloud2)
library(tidytext)
library(shinydashboard)
library(shinythemes)
library(mwshiny)
library(randomcoloR)
library(cowplot)
library(reshape2)
library(dashboardthemes)

terrorism <- read_csv(file = 'globalterrorismdb_0718dist.csv',
                      col_types = cols(
                        iyear = col_integer(),
                        imonth = col_integer(),
                        iday = col_integer(),
                        nkill = col_double(),
                        nwound = col_double()), locale = locale(encoding = "Latin1")
)



terrorism <- rename(terrorism, c('Year'='iyear','Month'='imonth','Day'='iday','Country'='country_txt','Region'='region_txt','AttackType'='attacktype1_txt','Target'='target1','Killed'='nkill','Wounded'='nwound','Summary'='summary','Group'='gname','Target_Type'='targtype1_txt','Weapon_Type'='weaptype1_txt','Motive'='motive'))

dataset <- terrorism

terrorism <- terrorism %>%
  unite(Date, Year, Month, Day, sep = "-", remove = FALSE)


country <- c(None='.', unique(dataset[c("Country")]))

country <- country[country != "None"]



shinyApp(
  
  ui = dashboardPage(
    dashboardHeader(title = "Terrorist Situation"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Introduction", tabName = "in", icon = icon("network-wired")),
        menuItem("Country Map", tabName = "cm", icon = icon("map")),
        menuItem("Time Series", tabName = "ts", icon = icon("clock")),
        menuItem("Target Type", tabName = "m", icon = icon("bullseye")),
        menuItem("Type of Attacks", tabName = "h", icon = icon("khanda"))
      )
    ),
    
    dashboardBody(
      shinyDashboardThemes(
        theme = "grey_dark"
      ),
      tabItems(
        tabItem(tabName = "in",
                fluidPage(
                  h2("The Terrorist Situation in the Scope of Country", align = "center", font = "Times New Roman"),
                  h4("  This interactive app is using the Global Terror Database from Kaggle: https://www.kaggle.com/START-UMD/gtd.
                   which includes all of the terrorist incidents of the globe from 1970 to 2017, except for 1993. This dataset
                   contains 181614 incidents, and more than 100 variables ranges from location (longitude and latitude) to the 
                   motive of each attack (in text). In this app, we visit the analysis of terrorist situation at a country 
                     level. The intended users of this app would be anyone who wishes to know more about the terrorist
                     situation in a specific country. Given the huge size of the Global Terrorism Database, it might take a while to load. Wish the 
                     users will learn something insightful from using this app!")
                )),
        # First tab content
        tabItem(tabName = "cm",
                fluidRow(
                  box(
                    title = "Terrorist Situation",
                    "There are two interactive plots in this page. The plots' attributes control the plot in
                    the bottom. The users can choose a country and observe the terrorist situation in that country. The user
                    can see the summary of the incident when hovering the mouse on one of the red points in the map. The top right plot presents the terrorist situations in the top three cities of the selected country,
                    Additionally, the user can also change the bottom plot into a contour plot with the number of people been killed
                    as the level. Moreover, the user can also choose to filter the terrorist incidents by the number of people died in 
                    that incident.",
                    width = 100
                  )
                ),
                br(), 
                fluidRow(
                  box(
                    title = "Plots' Attributes",
                    tags$head(tags$script('
                                  var dimension = [0, 0];
                                  $(document).on("shiny:connected", function(e) {
                                      dimension[0] = window.innerWidth;
                                      dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension);
                                  });
                                  $(window).resize(function(e) {
                                      dimension[0] = window.innerWidth;
                                      dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension);
                                  });
                              ')),
                    selectInput('country_name', 'Country', unique(dataset[c("Country")]), selected = 'Iraq'),
                    checkboxInput("contour", "Change to a Contour Plot with number of people killed as level", value = FALSE),
                    sliderInput("nmk", label = "Number of People Killed (More Than):",
                                min = 0, max = 50, value = 0, step = 10)
                  ),
                  
                  box(
                    title = "Cities",
                    tags$head(tags$script('
                                  var dimension = [0, 0];
                                  $(document).on("shiny:connected", function(e) {
                                      dimension[0] = window.innerWidth;
                                      dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension);
                                  });
                                  $(window).resize(function(e) {
                                      dimension[0] = window.innerWidth;
                                      dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension);
                                  });
                              ')),
                    textOutput("is_conf_shown"), 
                    plotlyOutput("plot_1_2"),
                    height = 300,
                    align = "center"
                  )
                ),
                
                br(), 
                fluidRow(
                  title = "Cities",
                  tags$head(tags$script('
                                  var dimension = [0, 0];
                                  $(document).on("shiny:connected", function(e) {
                                      dimension[0] = window.innerWidth;
                                      dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension);
                                  });
                                  $(window).resize(function(e) {
                                      dimension[0] = window.innerWidth;
                                      dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension);
                                  });
                              ')),
                  plotlyOutput ('plot_1')
                )
        ),
        # Second tab content
        tabItem(tabName = "ts",
                fluidRow(
                  box(
                    title = "Trend of Terrorist Activities for Different Countries",
                    "This item displays the trend of terrorist incidents for different countries over time.
                  Users could zoom in the time axis to take a closer look at the relationship between the dates and
                  the number of terrorist incidents.",
                    width = 100
                  )
                ),
                br(),
                fluidRow(
                  box(
                    selectInput('country_name_2', 'Country', unique(dataset[c("Country")]), selected = 'Iraq'),
                    width  = 100
                  )
                ),
                fluidRow(
                  box(
                    title = "Time Series",
                    dygraphOutput("plot_2"),
                    width = 100
                  )
                  
                )
        ),
        
        tabItem(tabName = "m",
                fluidRow(
                  box(
                    title = 'Type of Targets',
                    "This tab item displays the result of the text mining as the Word Cloud. The size
                    of the word would be closely related to the number of times it appears in the targets of
                    terrorist incidents. Users can also hover on the word to see the number of times that this
                    word appears.",
                    width = 100
                    
                  )
                ),
                br(),
                
                fluidRow(
                  box(
                    selectInput('country_name_3', 'Country', unique(dataset[c("Country")]), selected = 'Iraq'),
                    width  = 100
                  )
                ),
                
                fluidRow(
                  box(
                    title = "Word Cloud for Terrorists' Targets",
                    wordcloud2Output("plot_3", width = "50%", height = "400px"),
                    width = 300,
                    height = 500,
                    align = "center"
                    
                  )
                )
        ),
        
        tabItem(tabName = "h",
                fluidRow(
                  box(
                    title = 'Type of Terrorist Attack vs. Number of People Got Killed',
                    "This tab item displays a bar plot of the types of terrorist attacks with the number of
                    incidents as the heights of the bars. By all means, the users can also choose to switch the
                    height of bar representing the number of people got killed from this type of terrorist attack.
                    Similarly, there is always a choice for the country.",
                    width = 100
                    
                  )
                ),
                br(),
                
                fluidRow(
                  box(
                    selectInput('country_name_4', 'Country', unique(dataset[c("Country")]), selected = 'Iraq'),
                    checkboxInput("kill", "Change to a Bar Plot with number of people killed as height of the Bar", value = FALSE),
                    width  = 100
                  )
                ),
                
                fluidRow(
                  title = "Trend of Terrorist Activities for Different Countries"
                ),
                br(),
                fluidRow(
                  title = "Which Type of Attack caused the most number people of death",
                  plotlyOutput("plot_4")
                  
                )
        )
      )
    )
    # ),
    # tags$head(tags$style(HTML('* {font-family: "Times New Roman"};')))
  ),
  
  server = function(input, output) {
    output$plot_1 <- renderPlotly({
      
      GET("http://httpbin.org/delay/1", timeout(20))
      
      country_cond <- terrorism %>%
        filter(Country == input$country_name)
      
      country_cond$longitude[is.na(country_cond$longitude)] <- 0
      country_cond$latitude[is.na(country_cond$latitude)] <- 0
      
      
      
      if(input$country_name == "United States"){
        map_bbox <- c(left = -125, bottom = 25.75, right = -67, top = 49)
        
      } else {
        
        bounds <- maps::map("world", input$country_name, plot=FALSE)$range
        map_bbox <- c(left = bounds[1], bottom = bounds[3], right = bounds[2], top = bounds[4])
        
      }
      
      map_base <- get_stamenmap(bbox = map_bbox,
                                maptype = "toner-lite",
                                zoom = 6)
      
      name <- paste("Terrorist Situation in",input$country_name, sep=" ")
      
      
      country_cond_mid <- country_cond %>%
        filter(Killed >= input$nmk)
      
      # if (dim(country_cond_mid)[1] == 0) {
      #   country_cond <- country_cond
      # } else {
      #   country_cond <- country_cond_mid
      # }
      
      map_object <- ggmap(ggmap = map_base,
                          extent = "panel") 
      
      
      map_object <- map_object +
        {if (dim(country_cond_mid)[1] != 0) 
          geom_point(data = country_cond_mid, aes(x = longitude, y = latitude, text = paste("Summary: ", gsub('(.{1,20})(\\s|$)', '\\1\n', Summary),
                                                                                            "<br>City: ", city,
                                                                                            "<br>Number of People Killed: ", Killed,
                                                                                            "<br>Motive: ", Motive)), colour = "red", size = 0.2) 
        }
      
      
      map_object <- map_object +
        labs(title = name,
             caption = "Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.") +
        theme_bw()  %+replace%
        theme(text = element_text(family="serif", face="bold", size=12)) +
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(), axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        theme(axis.text=element_text(size=13),
              title = element_text(size = 15))
      
      
      country_cond$Killed[is.na(country_cond$Killed)] <- 0
      
      if(input$contour == TRUE){
        contour <- ggmap(ggmap = map_base,
                         extent = "panel") + 
          stat_density2d(data = country_cond, aes(x = longitude, y = latitude,  level = Killed)) +
          scale_fill_distiller(palette = "Spectral", direction = -1) +
          labs(title = name,
               caption = "Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.") +
          theme_bw()  %+replace%
          theme(text = element_text(family="serif", face="bold", size=12)) +
          theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(), axis.text.y=element_blank(),
                axis.ticks.y=element_blank()) +
          theme(axis.text=element_text(size=13),
                title = element_text(size = 15))
        
        ggplotly(contour)
      } else {
        
        ggplotly(map_object, tooltip = "text")
      }
      
    })
    
    output$plot_2 <- renderDygraph({
      
      country_cond <- terrorism %>%
        filter(Country == input$country_name_2)
      
      
      name_1 <- paste("Trend of Terrorist Situation in",input$country_name_2, sep=" ")
      
      
      country_cond <- country_cond %>%
        group_by(Date) %>%
        summarize(count = n())
      
      country_cond$Date <- as.Date(country_cond$Date, format = "%F")
      
      country_cond <- na.omit(country_cond)
      
      don <- xts(x = country_cond$count, order.by = country_cond$Date)
      
      p <- dygraph(don, main = name_1, 
                   ylab = "Number of Terrorist Incidents") %>%
        dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
        dyRangeSelector() %>%
        dyCrosshair(direction = "vertical") %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
        dyRoller(rollPeriod = 1)
      
      p
    })
    
    output$plot_3 <- renderWordcloud2({
      country_cond <- terrorism %>%
        filter(Country == input$country_name_3)
      
      df <- data.frame(column = country_cond$	
                         targsubtype1_txt)
      
      all_words <- unlist( # flattten word list from individual strings into one vector
        regmatches(df$column,  gregexpr('\\w+', df$column))) # extract all words
      # count frequencies
      freq_count <- table(all_words)
      
      w <- wordcloud2(data = freq_count, size = 0.5, color = "red", shape = "circle")
      
      w
    })
    
    
    output$plot_4 <- renderPlotly({
      
      terrorism$Killed[is.na(terrorism$Killed)] <- 0
      
      atc_type_subset <- terrorism %>%
        filter(Country == input$country_name_4)
      
      name_2 <- paste("Types of Terrorist Activity",input$country_name_4, sep=" ")
      
      if(input$kill == TRUE){
        
        atc_type_subset <- atc_type_subset  %>%
          group_by(AttackType) %>%
          summarise(
            Kill_Total = sum(Killed)
          )
        
        
        atc_type_subset$AttackType <- reorder(atc_type_subset$AttackType, +atc_type_subset$Kill_Total)
        
        atc_type_subset <- atc_type_subset  %>%
          mutate(max_total = ifelse(Kill_Total==max(Kill_Total), "1", "0"))
        
        atc_type <-
          ggplot(data = atc_type_subset, aes(x= AttackType, y = Kill_Total, fill = max_total)) +
          geom_bar(stat = "identity") +
          labs(
            title = name_2,
            caption = "Source: Global Terrorism Database",
            y = "Number of People Being Killed"
          )+
          scale_fill_manual( values = c( "1"="red", "0"="black" ), guide = FALSE )+
          theme_bw()  %+replace%
          theme(text = element_text(family="serif", face="bold", size=12)) +
          theme(axis.title.y = element_blank()) +
          coord_flip() +
          theme(axis.text=element_text(face = "bold", size=13),
                title = element_text(size = 15),
                legend.position = "none")
        
        ggplotly(atc_type, tooltip = c("AttackType", "Kill_Total"))
        
        
        
        
      } else {
        
        atc_type_subset <- atc_type_subset  %>%
          group_by(AttackType) %>%
          summarize(count = n())
        
        atc_type_subset$AttackType <- reorder(atc_type_subset$AttackType, +atc_type_subset$count)
        
        atc_type_subset <- atc_type_subset  %>%
          mutate(max_total = ifelse(count==max(count), "1", "0"))
        
        
        atc_type <-
          ggplot(data = atc_type_subset, aes(x = AttackType, y = count, fill = max_total)) +
          geom_bar(stat = "identity") +
          labs(
            title = name_2,
            subtitle = "Bombing is still the 'most popular' choice",
            caption = "Source: Global Terrorism Database",
            y = "Number of Terrorist Incidents"
          ) +
          scale_fill_manual( values = c( "1"="red", "0"="black" ), guide = FALSE )+
          theme_bw()  %+replace%
          theme(text = element_text(family="serif", face="bold", size=12)) +
          theme(axis.title.y = element_blank()) +
          coord_flip() +
          theme(axis.text=element_text(face = "bold", size=13),
                title = element_text(size = 15),
                legend.position = "none")
        
        
        ggplotly(atc_type, tooltip = c("AttackType", "count"))
        
      }
      
      
      
    })
    
    
    
    output$plot_1_2 <- renderPlotly({
      
      country_cond <- terrorism %>%
        filter(Country == input$country_name)
      
      country_cond <- country_cond %>%
        group_by(city) %>%
        summarize(Total = n())
      
      country_cond <- country_cond %>% arrange(desc(Total))
      
      country_cond <- head(country_cond, 3)
      
      name <- paste("Cities in",input$country_name, sep=" ")
      
      country_cond$city <- reorder(country_cond$city, +country_cond$Total)
      
      country_cond <- country_cond  %>%
        mutate(max_total = ifelse(Total==max(Total), "1", "0"))
      
      
      cities_type <-
        ggplot(data = country_cond, aes(x= city, y = Total, fill = max_total)) +
        geom_bar(stat = "identity") +
        # geom_bar(data=subset(country_cond, Total ==max(Total)), aes(city, Total),
        #          fill="red", stat="identity") +
        labs(
          title = name,
          caption = "Source: Global Terrorism Database",
          y = "Number of Terrorist Incidents"
        ) +
        scale_fill_manual( values = c( "1"="red", "0"="black" ), guide = FALSE )+
        theme_bw()  %+replace%
        theme(text = element_text(family="serif", face="bold", size=12)) +
        theme(axis.title.y = element_blank()) +
        coord_flip() +
        theme(axis.text=element_text(face = "bold", size=13),
              title = element_text(size = 15),
              legend.position = "none")
      
      
      ggplotly(cities_type, height = 230, width = 400, tooltip = c("city", "Total"))
      
      
    })
    
  },
  
  options = list(height = 800)
)