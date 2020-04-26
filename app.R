library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(lubridate)
library(dplyr)

library(geojsonio)
library(mapview)
library(leaflet)
library(leaflet.extras)
ui <- dashboardPage(
  dashboardHeader(title = "DC Crime Card"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Over View",
      tabName = "overview",
      selected = TRUE,
      icon = icon("dashboard")
    ),
    menuItem("Map", tabName = "map", icon = icon("map-marked-alt")),
    menuItem(
      "Data analysis",
      tabName = "analysis",
      icon = icon("chart-area")
    ),
    
    menuItem("About", tabName = "about", icon = icon("info-circle"))
  )),
  dashboardBody(tabItems(
    #First tab
    tabItem(tabName = "overview",
            fluidRow(
              #line
              box(
                title = "Liner View",
                status = "success",
                solidHeader = TRUE,
                collapsible = F,
                plotOutput("liner_view"),
                selectInput(
                  "by_what",
                  label = h3("Group by"),
                  choices = list("Month" = "Month", "Hour" = "Hour"),
                  selected = "Month"
                ),
                
                checkboxInput("Show_Point", "Show Point", value = T),
                checkboxInput("Show_all_Line", "Show All Smooth Line", value = F),
                checkboxInput("Show_each_Line", "Show Each Smooth Line", value = F),
                width = 6,
                height = 670
                
              ),
              
              
              
              
              
              
              #pie
              box(
                title = "Pie chart",
                status = "info",
                solidHeader = TRUE,
                collapsible = F,
                plotlyOutput("pie_by_month"),
                h3('Select a month'),
                sliderInput("pie_month", "Month", 1, 12, 1),
                width = 6,
                height = 670
                
              )
            ),
            fluidRow(
              #3D view
              box(
                title = "3D View",
                status = "primary",
                solidHeader = TRUE,
                collapsible = F,
                plotlyOutput("3d_plot"),
                width = 6,
                height = 460
              ),
              
              #table
              box(
                title = "Table",
                status = "danger",
                solidHeader = TRUE,
                collapsible = F,
                plotOutput('bar_year'),
                width = 6,
                height = 460
              ),
              
              
            )),
    
    #second tab
    tabItem(tabName = "map",
            fluidRow(
              box(
                title = "Crime by sector",
                status = "primary",
                solidHeader = TRUE,
                collapsible = F,
                leafletOutput("crime_sector"),
                width = 6,
                height = 550,
                h4(
                  "On Jan 10, 2019, the MPD launched police boundary changes based on this evaluation.
                      There are seven police districts in Washington, DC, and each police district is divided into three sectors
                      with a sector being an informal grouping of Police Service Areas (PSAs).  "
                )
              ),
              box(
                title = "Crime ",
                status = "danger",
                solidHeader = TRUE,
                collapsible = F,
                leafletOutput("crime_des"),
                width = 6,
                height = 550,
                fixedRow(
                  column(3,
                         selectInput(
                           "month_map",
                           h6("Month"),
                           c(
                             'Jan' = 1,
                             'Feb' = 2,
                             'Mar' = 3,
                             'April' = 4,
                             'May' = 5,
                             'Jun' = 6,
                             'Jul' = 7,
                             'Aug' = 8,
                             'Sep' = 9,
                             'Oct' = 10,
                             'Nov' = 11,
                             'Dec' = 12
                           ),
                           selected = "Jan"
                         )),
                  column(3,
                         selectInput(
                           "shift_map",
                           h6("Shift"),
                           c(
                             "day" = "day",
                             "evening" = "evening",
                             "midnight" = "midnight"
                           ),
                           selected = "day"
                         )),
                  column(3,
                         selectInput(
                           "week_day_map",
                           h6("Week day"),
                           c(
                             'Monday' = 1,
                             'Tuesday' = 2,
                             'Wednesday' = 3,
                             'Thursday' = 4,
                             'Friday' = 5,
                             'Saturday' = 6,
                             'Sunday' = 7
                           ),
                           selected = "Monday"
                         )),
                  column(
                    3,
                    selectInput(
                      "crime_type_map",
                      h6("Crime type"),
                      c(
                        'theft/other' = 'theft/other',
                        'theft f/auto' = 'theft f/auto',
                        'assault' = "assault w/dangerous weapon",
                        'burglary' = 'burglary',
                        'robbery' = 'robbery',
                        'vehicle theft' = 'motor vehicle theft',
                        #'homicide'="homicide",
                        'sex abuse' = 'sex abuse',
                        'arson' = 'burglary'
                      ),
                      selected = "theft/othe"
                    )
                  )
                )
                
                
                
              )
              
              
              
            )),
    #analyise
    tabItem(tabName = "analysis",
            fluidRow(
              box(
                title = "Crime VS Month",
                status = "primary",
                solidHeader = TRUE,
                collapsible = T,
                width = 6,
                height = 890,
                plotOutput("mn_plot"),
                verbatimTextOutput("mn_lm")
                
              ),
              box(
                title = "Crime VS Time",
                status = "warning",
                solidHeader = TRUE,
                collapsible = T,
                width = 6,
                height = 890,
                plotOutput("tn_plot"),
                verbatimTextOutput("tn_lm")
              ),
              
            ),
            fluidRow(
              box(
                title = "Crime around Holiday Season",
                status = "primary",
                solidHeader = TRUE,
                collapsible = T,
                width = 6,
                plotOutput("hsn_plot"),
                
              ),
              box(
                title = "Crime around Holiday Season",
                status = "warning",
                solidHeader = TRUE,
                collapsible = T,
                width = 6,
                plotOutput("ho_lm"),
                
              ),
              box(
                title = "Crime VS Dictrict",
                status = "success",
                solidHeader = TRUE,
                collapsible = T,
                width = 6,
                plotOutput("dn_plot"),
              )
            )),
    
    #third
    tabItem(tabName = "about",
            fluidRow(
              box(
                title = "About the project",
                status = "primary",
                solidHeader = TRUE,
                collapsible = F,
                width = 12,
                h3("Introduction"),
                h4(
                  "The crime statistic not only help criminal justice professionals anticipate increased risk of crime,
                but also can reduce the fear of crime in the society for people and enhance self-protection.
                In this report, our group will focus on time of crime, the type of crime and the area where the crime occurred.
                we will begin to see how the criminal activity has changed over time, begin to understand how it is evolving.
                Using all the data available from Washington, D.C.,
                we can begin to understand how crime is evolving and understand if local law enforcement is prioritizing the effective resources to address it.
                Besides, the primary audience is for is local law enforcement and security professionals in the Washington, D.C. area."
                ),
                h3("About this data"),
                h4(
                  "This data set include the information for the crimes from 2018 to 2019, with time, shift, location.
                   There are 29 variables in the data set."
                ),
                h3("Initial Hypotheses"),
                h4("1. The crime is associated with month."),
                h4("2. The crime is associated with time.\n"),
                h4("3. The crime is associated with district.\n"),
                h4("4. The crime is associated with month, time and district.\n"),
                h3("Conclusion"),
                h4(
                  "Through this project, we have shown the tend of crime rate.
                The crime frequents at the end of the year and at night.
                Compared with the citywide of DC, city center crime is more serious.
                Besides, through this project, we want to show people that Building on DC’s public safety improvements will take more than policing alone,
                we need a holistic approach that involves social services, the police, nonprofits, and residents working together,
                only through collaboration can the city build safer communities—creating the social supports, stability,
                and opportunities that will bring down even the most persistent pockets of violent crime.
                We have only shown the data for 2018 and 2019.
                Perhaps the scope of the data set should be expanded, which is more convincing.
                  In addition, as a non-professional, there may be some analytical bias."
                ),
                h3("Team Member"),
                h4("Bo"),
                h4("Jingyi"),
                h4("Alex"),
                
                
                #img(src='https://i.loli.net/2020/03/05/xHVmZtARacnWeJK.png', align = "right", height = '30px', width = '150px')
                
                
              ),
              h6("Shiny App designed by: Alex Liu", align = "center"),
              
              h6("Polaris Studio & Alex_NKG © 2020", align = "center")
              
            ))
  ))
)








server <- function(input, output) {
  output$pie_by_month <- renderPlotly({
    dc_crime %>%
      mutate(REPORT_DAT = parse_datetime(as.character(dc_crime$REPORT_DAT), format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
      mutate(Month = month(REPORT_DAT)) %>%
      group_by(offense.text, Month) %>%
      summarise(n = n()) %>%
      filter(Month == input$pie_month) %>%
      plot_ly(
        labels = ~ `offense.text`,
        values = ~ n,
        type = 'pie'
        #showlegend = FALSE
      )
  })
  output$`3d_plot` <- renderPlotly({
    dc_crime %>%
      mutate(REPORT_DAT = parse_datetime(as.character(dc_crime$REPORT_DAT), format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
      mutate(Month = month(REPORT_DAT)) %>%
      mutate(Hour = hour(REPORT_DAT)) %>%
      mutate(Wday = wday(REPORT_DAT, label = T)) %>%
      
      group_by(offense.text, Month, Wday) %>%
      
      summarise(Total = n()) %>%
      plot_ly(
        x = ~ Month,
        y = ~ Wday,
        z = ~ log(Total),
        color = ~ offense.text
      ) %>%
      add_markers() %>%
      layout(scene = list(
        xaxis = list(title = 'Month'),
        yaxis = list(title = 'Week day'),
        zaxis = list(title = 'Number by log')
      ))
    
  })
  output$liner_view <- renderPlot({
    if (input$by_what == "Month")
      dc_crime %>%
      mutate(REPORT_DAT = parse_datetime(as.character(dc_crime$REPORT_DAT), format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
      mutate(Month = month(REPORT_DAT)) %>%
      mutate(Year = year(REPORT_DAT)) %>%
      mutate(Day = mday(REPORT_DAT)) %>%
      group_by(offense.text, Year, Month) %>%
      summarise(Total = n()) %>%
      mutate(Time = make_date(year = Year, month = Month)) %>%
      ggplot(mapping = aes(x = Time, y = Total)) +
      xlab("Time") +
      ylab("Total Number") +
      ggtitle("Crime number VS Time") +
      theme_bw() -> linerdf
    
    if (input$by_what == "Hour")
      dc_crime %>%
      mutate(REPORT_DAT = parse_datetime(as.character(dc_crime$REPORT_DAT), format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
      mutate(Month = month(REPORT_DAT)) %>%
      mutate(Year = year(REPORT_DAT)) %>%
      mutate(Day = mday(REPORT_DAT)) %>%
      mutate(Hour = hour(REPORT_DAT)) %>%
      group_by(offense.text, Hour) %>%
      summarise(Total = n()) %>%
      ggplot(mapping = aes(x = Hour, y = Total)) +
      xlab("Time") +
      ylab("Total Number") +
      ggtitle("Crime number VS Time") +
      theme_bw() -> linerdf
    
    if (input$Show_Point)
      linerdf <-
        linerdf + geom_point(mapping = aes(color = offense.text),
                             size = 2.0) +
        scale_color_discrete(name = "Offense Type")
    
    if (input$Show_each_Line)
      linerdf <-
        linerdf + geom_smooth(
          mapping = aes(color = offense.text),
          method = lm,
          se = FALSE
        ) +
        scale_color_discrete(name = "Offense Type")
    
    if (input$Show_all_Line)
      linerdf <-
        linerdf + geom_smooth(
          method = lm,
          se = FALSE,
          color = "black",
          linetype = "dashed"
        ) +
        scale_color_discrete(name = "Offense Type")
    print(linerdf)
    
  })
  
  output$bar_year <- renderPlot({
    dc_crime %>%
      mutate(REPORT_DAT = parse_datetime(as.character(dc_crime$REPORT_DAT), format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
      select(offensegroup, offense.text, REPORT_DAT) %>%
      mutate(Month = month(REPORT_DAT)) %>%
      mutate(Week_day = wday(REPORT_DAT)) %>%
      mutate(Year = year(REPORT_DAT)) %>%
      filter(Year != 2020) %>%
      group_by(offensegroup, offense.text, Year) %>%
      
      rename(Group = "offensegroup") %>%
      rename(Title = 'offense.text') %>%
      
      ggplot(mapping = aes(x = fct_rev(fct_infreq(Title)), fill =
                             as.factor(Year))) +
      geom_bar(stat = "count",
               width = 0.7,
               position = 'dodge') +
      labs(x = "", y = "Total Number", fill = "Year") +
      theme_bw() +
      geom_text(
        stat = 'count',
        aes(label = ..count..),
        color = "black",
        size = 3.5,
        position = position_dodge(width = 1),
        vjust = 0.7
      ) +
      
      
      coord_flip()
    
  })
  
  
  output$crime_sector <- renderLeaflet({
    #by sector inclued different
    dc_crime %>%
      group_by(sector) %>%
      summarise(n = n()) %>%
      filter(sector != '') -> total_n
    
    dc_crime %>%
      group_by(sector, offense.text) %>%
      summarise(n = n()) %>%
      filter(sector != '') %>%
      spread(key = "offense.text", value = "n") %>%
      mutate(arson = replace_na(arson, 0))  -> split_n
    crime_by_sector <- left_join(total_n, split_n) %>%
      separate(sector,
               into = c("distrct", "sector"),
               sep = "D")
    
    
    crime_by_sector_labels <- sprintf(
      "
<strong>District %s Sector %s </strong>
<br/>
<table>
  <tr>
    <th>Offense</th>
    <th>Num</th>
  </tr>
  <tr>
    <td>arson:</td>
    <td> %s </td>
  </tr>
  <tr>
    <td>assault:</td>
    <td> %s </td>
  </tr>
  <tr>
    <td>burglary:</td>
    <td> %s </td>
  </tr>
  <tr>
    <td>homicide:</td>
    <td> %s </td>
  </tr>
  <tr>
    <td>vehicle theft:</td>
    <td> %s </td>
  </tr>
  <tr>
    <td>robbery:</td>
    <td> %s </td>
  </tr>
  <tr>
    <td>sex abuse:</td>
    <td> %s </td>
  </tr>
  <tr>
    <td>theft f/auto:</td>
    <td> %s </td>
  </tr>
  <tr>
    <td>theft/other:</td>
    <td> %s </td>
  </tr>
  <tr>
    <td>Total:</td>
    <td> %s </td>
  </tr>
</table>",
      
      crime_by_sector$distrct,
      crime_by_sector$sector,
      crime_by_sector$arson,
      crime_by_sector$`assault w/dangerous weapon`,
      crime_by_sector$burglary,
      crime_by_sector$homicide,
      crime_by_sector$`motor vehicle theft`,
      crime_by_sector$robbery,
      crime_by_sector$`sex abuse`,
      crime_by_sector$`theft f/auto`,
      crime_by_sector$`theft/other`,
      crime_by_sector$n
    ) %>% lapply(htmltools::HTML)
    
    
    
    
    police_sector_map <-
      geojson_read("Police_Sectors.geojson",  what = "sp")
    
    
    
    crime_by_secto_pal <-
      colorNumeric(c("green", "red"), 1000:7500)
    
    leaflet(police_sector_map , options = leafletOptions(minZoom = 10, maxZoom = 12)) %>%
      addTiles(attribution = 'Polaris Studio & Alex © 2020') %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 10, maxZoom =
                                                       12),
      ) %>%
      #addFullscreenControl() %>%
      addPolygons(
        #stroke = T,
        color = "black",
        weight = 1,
        fillColor = ~ crime_by_secto_pal(crime_by_sector$n),
        smoothFactor = 0.7,
        fillOpacity = 0.6,
        highlight = highlightOptions(
          weight = 5,
          color = "gray",
          fillOpacity = 0.4,
          bringToFront = TRUE
        ),
        label = crime_by_sector_labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal",
                       padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
        
      ) %>%
      addLegend(
        pal = crime_by_secto_pal,
        values = ~ crime_by_sector$n,
        opacity = 0.7,
        title = NULL,
        position = "bottomright"
      )
    
    
    
  })
  
  
  
  output$crime_des <- renderLeaflet({
    dc_b <- geojson_read("dcb.geojson",  what = "sp")
    
    tidy_cirme <- dc_crime %>%
      mutate(REPORT_DAT = parse_datetime(as.character(dc_crime$REPORT_DAT), format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
      #select(offensegroup,offense.text,REPORT_DAT,SHIFT) %>%
      mutate(Month = month(REPORT_DAT)) %>%
      mutate(Week_day = wday(REPORT_DAT)) %>%
      mutate(Year = year(REPORT_DAT)) %>%
      mutate(Hour = hour(REPORT_DAT))
    #select(LONGITUDE,LATITUDE,`offense.text`,SHIFT,Month,Week_day,Hour)
    
    tidycrime1 <- tidy_cirme %>%
      filter(
        Month == input$month_map,
        SHIFT == input$shift_map,
        Week_day == input$week_day_map,
        `offense.text` == input$crime_type_map
      )
    leaflet(dc_b, options = leafletOptions(minZoom = 11, maxZoom = 18)) %>%
      addTiles(attribution = 'Polaris Studio & Alex © 2020') %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(maxZoom = 13)) %>%
      #addTiles() %>%
      addPolygons(
        color = "#444444",
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 0.0
      ) %>%
      addMarkers(~ tidycrime1$LONGITUDE,
                 ~ tidycrime1$LATITUDE,
                 label = ~ tidycrime1$OCTO_RECORD_ID)
    
  })
  
  output$ana_cvm <- renderPlot({
    dc_crime %>%
      mutate(REPORT_DAT = parse_datetime(as.character(dc_crime$REPORT_DAT), format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
      select(offensegroup, offense.text, REPORT_DAT) %>%
      mutate(Month = month(REPORT_DAT)) %>%
      mutate(Week_day = wday(REPORT_DAT)) %>%
      mutate(Year = year(REPORT_DAT)) %>%
      filter(Year != 2020) %>%
      group_by(offensegroup, offense.text, Year) %>%
      
      rename(Group = "offensegroup") %>%
      rename(Title = 'offense.text') %>%
      
      ggplot(mapping = aes(x = fct_rev(fct_infreq(Title)), fill =
                             as.factor(Year))) +
      geom_bar(stat = "count",
               width = 0.7,
               position = 'dodge') +
      labs(x = "", y = "Total Number", fill = "Year") +
      theme_bw() +
      geom_text(
        stat = 'count',
        aes(label = ..count..),
        color = "black",
        size = 3.5,
        position = position_dodge(width = 1),
        vjust = 0.7
      ) +
      
      
      coord_flip()
    
  })
  dc_crime %>%
    mutate(REPORT_DAT = parse_datetime(as.character(dc_crime$REPORT_DAT), format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
    mutate(Month = month(REPORT_DAT)) %>%
    mutate(Year = year(REPORT_DAT)) %>%
    mutate(Day = mday(REPORT_DAT)) %>%
    group_by(Month,Year) %>%
    summarise(Total = n()) %>%
    mutate(Time = make_date(month = Month)) -> ana_m
  
  output$mn_plot <- renderPlot({
    ana_m %>%
      filter(Year!=2020) %>% 
      ggplot(mapping = aes(x = Month, y = Total,)) +
      geom_point() +
      theme_bw() +
      geom_smooth(mapping = aes(), method = lm, se = FALSE)+
      facet_wrap(~ Year)
  })
  output$mn_lm <- renderPrint({
    m_m_n <- lm(Total ~ Month+as.factor(Year), data = ana_m)
    summary(m_m_n)
  })
  
  
  output$dn_plot <- renderPlot({
    dc_crime %>%
      mutate(REPORT_DAT = parse_datetime(as.character(dc_crime$REPORT_DAT), format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
      mutate(Month = month(REPORT_DAT)) %>%
      mutate(Year = year(REPORT_DAT)) %>%
      mutate(Day = mday(REPORT_DAT)) %>%
      filter(Year != 2020) %>%
      ggplot(mapping = aes(x = DISTRICT, fill =
                             as.factor(Year))) +
      geom_bar(stat = "count",
               width = 0.7,
               position = 'dodge') +
      labs(x = "DISTRICT", y = "Total Number", fill = "Year") +
      theme_bw()
  })
  dc_crime %>%
    mutate(REPORT_DAT = parse_datetime(as.character(dc_crime$REPORT_DAT), format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
    mutate(Month = month(REPORT_DAT)) %>%
    mutate(Year = year(REPORT_DAT)) %>%
    mutate(Week_day = wday(REPORT_DAT)) %>%
    group_by(Month, SHIFT, Week_day) %>%
    summarise(Total = n()) %>%
    mutate(Time = make_date(month = Month)) -> ana_t
  
  output$tn_plot <- renderPlot({
    ana_t %>%
      ggplot(mapping = aes(x = Week_day, y = Total, colour = SHIFT)) +
      geom_point() +
      theme_bw()
  })
  
  output$tn_lm <- renderPrint({
    m_t_n <- lm(Total ~ SHIFT * Week_day, data = ana_t)
    summary(m_t_n)
  })
  dc_crime %>%
    mutate(REPORT_DAT = parse_datetime(as.character(dc_crime$REPORT_DAT), format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
    mutate(Month = month(REPORT_DAT)) %>%
    mutate(Year = year(REPORT_DAT)) %>%
    mutate(Day = mday(REPORT_DAT)) %>%
    group_by(offense.text, Year, Month, Day) %>%
    summarise(Total = n()) %>%
    mutate(Time = make_date(
      year = Year,
      month = Month,
      day = Day
    )) -> d
  
  output$ho_lm <- renderPlot({
    d %>%
      filter(
        Time == "2018-12-06" |
          Time == "2018-12-07" |
          Time == "2018-12-08" |
          Time == "2018-12-09" |
          Time == "2018-12-10" |
          Time == "2018-12-11" |
          Time == "2018-12-12" |
          Time == "2018-12-13" |
          Time == "2018-12-14" |
          Time == "2018-12-15" |
          Time == "2018-12-16" |
          Time == "2018-12-17" |
          Time == "2018-12-18" |
          Time == "2018-12-19" |
          Time == "2018-12-20" |
          Time == "2018-12-21" |
          Time == "2018-12-22" |
          Time == "2018-12-23" |
          Time == "2018-12-24" |
          Time == "2018-12-25" |
          Time == "2018-12-26" |
          Time == "2018-12-27" |
          Time == "2018-12-28" |
          Time == "2018-12-29" |
          Time == "2018-12-30" |
          Time == "2018-12-31" |
          Time == "2019-01-01" |
          Time == "2019-01-02" |
          Time == "2019-01-03" |
          Time == "2019-01-04" |
          Time == "2019-01-05"
      ) %>%
      group_by(offense.text, Time) %>%
      summarise(n = sum(Total)) %>%
      ungroup() %>%
      ggplot(aes(x = Time, y = n, color = offense.text)) +
      geom_line() +
      theme_bw() +
      ylab("Total Number") +
      xlab("Holiday Season") +
      #ggtitle("Crime around Holiday Season")+
      theme(legend.position = "bottom") +
      scale_color_discrete(name = "")
  })
  
  output$hsn_plot <- renderPlot({
    holiday <- d %>%
      filter(
        Time == "2018-12-24" |
          Time == "2018-12-25" |
          Time == "2018-12-26" |
          Time == "2018-12-30" |
          Time == "2018-12-31" |
          Time == "2019-01-01" |
          Time == "2019-01-02"
      ) %>%
      group_by(offense.text, Time) %>%
      summarise(n = sum(Total)) %>%
      ungroup()
    holiday %>%
      ggplot(aes(x = Time, y = n, color = offense.text)) +
      geom_col() +
      theme_bw() +
      ylab("Number") +
      xlab("Holiday") +
      labs(title = "Crime around Holiday Season",
           subtitle = "The day before the holiday,The day of the holiday,The day after the holiday") +
      scale_color_discrete(name = "Offense Type")
  })
  
  
  
  
  
  
  
  
  
  
  
}







dc_crime <- read.csv("dc_crimes_search_results.csv")




shinyApp(ui = ui, server = server)
