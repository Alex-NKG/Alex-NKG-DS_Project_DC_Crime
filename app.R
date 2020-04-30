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
                title = "Crime number and Time relation",
                status = "success",
                solidHeader = TRUE,
                collapsible = F,
                plotOutput("liner_view"),
                column(3,selectInput(
                  "by_what",
                  label = ("Group by"),
                  choices = list("Month" = "Month", "Hour" = "Hour"),
                  selected = "Month"
                )),
                
                column(3,checkboxInput("Show_Point", "Show Point", value = T)),
                column(3,checkboxInput("Show_all_Line", "Show All Smooth Line", value = F),),
                column(3,checkboxInput("Show_each_Line", "Show Each Smooth Line", value = F)),

                width = 6,
                height = 550
                
              ),
              
              
              
              
              
              
              #pie
              box(
                title = "Crime precentage in per month",
                status = "info",
                solidHeader = TRUE,
                collapsible = F,
                plotlyOutput("pie_by_month",width = "auto"),
                #h3('Select month'),
                column(
                  3,
                  selectInput(
                    "crime_year_pi",
                    ("Crime Year"),
                    c(
                    2018,2019
                    ),
                    selected = 2019
                  )
                ),
                column(5,
                sliderInput("pie_month", "Month", 1, 12, 6)),
                
                width = 6,
                height = 550
                
              )
            ),
            fluidRow(
              #3D view
              box(
                title = "Relation between number and weekdays or month",
                status = "primary",
                solidHeader = TRUE,
                collapsible = F,
                plotlyOutput("3d_plot",width = "auto"),
                width = 6,
                height = 460
              ),
              
              #table
              box(
                title = "Crime number changes in past 2 year",
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
                title = "Crime Numbers in each sector",
                status = "primary",
                solidHeader = TRUE,
                collapsible = F,
                leafletOutput("crime_sector"),
                width = 6,
                height = 550,
                column(
                3,
                  selectInput(
                  "crime_year_map1",
                  h4("Crime Year"),
                  c(
                    2018,2019
                  ),
                  selected = 2019
                ))
              ),
              box(
                title = "Crime case location",
                status = "danger",
                solidHeader = TRUE,
                collapsible = F,
                leafletOutput("crime_des"),
                width = 6,
                height = 550,
                fixedRow(
                  column(
                    2,
                    selectInput(
                      "crime_year_map",
                      h5("Year"),
                      c(
                        2018,2019
                      ),
                      selected = 2019
                    )
                  ),
                  column(2,
                         selectInput(
                           "month_map",
                           h5("Month"),
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
                  column(2,
                         selectInput(
                           "shift_map",
                           h5("Shift"),
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
                           h5("Week day"),
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
                      h5("Crime type"),
                      c(
                        'theft/other' = 'theft/other',
                        'theft f/auto' = 'theft f/auto',
                        'assault' = "assault",
                        'burglary' = 'burglary',
                        'robbery' = 'robbery',
                        'vehicle theft' = 'motor vehicle theft',
                        #'homicide'="homicide",
                        'sex abuse' = 'sex abuse',
                        'arson' = 'burglary'
                      ),
                      selected = "theft/othe"
                    )
                  ),
                  
                )
                
                
                
              )
              
              
              
            )),
    #analyise
    tabItem(tabName = "analysis",
            fluidRow(
              box(
                title = "Relation between Crime number and Month",
                status = "primary",
                solidHeader = T,
                collapsible = F,
                width = 6,
                height = 600,
                plotOutput("lmmonth"),
                column(1,h4("R^2:")),
                column(11,verbatimTextOutput("mlm")),
                
                h4("In the piecewise function model, the R-squared value is 0.7139, and the p-value is small,"),
                h4("We can accept that there is a piecewise linear relationship between the month and the crime number.  ")
                
              ),
              box(
                title = "Analyze",
                status = "warning",
                solidHeader = TRUE,
                collapsible = F,
                width = 6,
                height = 600,
                plotOutput("night"),
                h4("According to the plot we find there are more crimes in the evening than at midnight."),
                h4("Thus, we will reject our hypothesis 4."),
                h4("Residents can be reminded that there are more thieves during that time, and law enforcement agencies need to maintain sufficient workforces at night to cope with the criminal acts.  ")
              ),
              
            ),
          ),
    
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
                  "Our project is criminal cases analyze in the DC area, in the project we hope we can explore the relationship between criminal case numbers and other possible predictor variables. By this topic, we hope to raise people’s awareness of public safety. Using all the data available from the Metropolitan Police Department, we can begin to understand how crime is evolving and understand whether local law enforcement is prioritizing the effective resources to address it."
                ),
                h3("About this data"),
                h4(
                  "This data set include the information for the crimes from 2018 to 2019, with time, shift, location.
                   There are 29 variables in the data set."
                ),
                h3("Initial Hypotheses"),
                h4("1. Some types of crime’s number increased in 2019 than in 2018."),
                h4("2. Total number of crimes may occur the most at the end of the year until the beginning of the next year."),
                h4("3. TThe month can affect the number of crimes."),
                h4("4. Compared with the evening and day, there are more crimes at midnight."),
                h3("Conclusion"),
                h4(
                  "After further study, we can draw a conclusion of the what tendency of the crime change from 2018 to 2019 in the DC area is.  
(1)The first hypothesis is some types of criminal cases increased in 2019 than in 2018, and we have proven it is true. The theft and robbery increased slightly from 2018 to 2019.  
(2) When mentioning the month and the crimes. Our second hypothesis is due to Christmas Day and New Year, we think the total number of crimes may occur the most at the end of the year until the beginning of the next year(from November to January of the following year). However, we have to reject the hypothesis. The period of the crimes occurs the most is from August to October. After analysis, we think the month can affect the number of crimes. We take January as outliers, then use a piecewise function to fit the linear model. Finally, we accept the hypothesis that there is a piecewise linear relationship between the month and the crime number.  
(3) In common idea, criminal cases likely happen during the period from midnight time to dawn, when people are usually sleepy and tired.  Our hypothesis is compared with the evening, and there are more crimes at midnight. However, according to the plot, we reject our last hypothesis. The crimes are more active in the evening rather than midnight.  

In summary, through this project, we want to show people that Building on DC’s public safety improvements will take more than policing alone, we need a holistic approach that involves social services, the police, nonprofits, and residents working together, only through collaboration can the city build safer communities—creating the social supports, stability, and opportunities that will bring down even the most persistent pockets of violent crime.  

."
                ),
               
                
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
      mutate(Year = year(REPORT_DAT)) %>%
      mutate(Month = month(REPORT_DAT)) %>%
      group_by(offense.text,Year, Month) %>%
      summarise(n = n()) %>%
      filter(Year == input$crime_year_pi) %>%
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
      filter(Year != 2020) %>%
      summarise(Total = n()) %>%
      mutate(Time = make_date(year = Year, month = Month)) %>%
      ggplot(mapping = aes(x = Time, y = Total)) +
      xlab("Month") +
      ylab("Crime Number") +
      ggtitle("Each month's crime number") +
      theme_bw() -> linerdf
    
    if (input$by_what == "Hour")
      dc_crime %>%
      mutate(REPORT_DAT = parse_datetime(as.character(dc_crime$REPORT_DAT), format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
      mutate(Month = month(REPORT_DAT)) %>%
      mutate(Year = year(REPORT_DAT)) %>%
      mutate(Day = mday(REPORT_DAT)) %>%
      mutate(Hour = hour(REPORT_DAT)) %>%
      filter(Year != 2020) %>%
      group_by(offense.text,Hour) %>%
      summarise(Total = n()) %>%
      ggplot(mapping = aes(x = Hour, y = Total)) +
      xlab("Hour") +
      ylab("Crime Number") +
      ggtitle("Each hour's crime number") +
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
          se = FALSE
        ) +
        scale_color_discrete(name = "Offense Type")
    
    if (input$Show_all_Line)
      linerdf <-
        linerdf + geom_smooth(
          #method = lm,
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
      mutate(REPORT_DAT = parse_datetime(as.character(dc_crime$REPORT_DAT), format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
      mutate(Year = year(REPORT_DAT)) %>%
      group_by(sector,Year) %>%
      summarise(n = n()) %>%
      filter(sector != '') %>% 
      filter(Year==input$crime_year_map1)-> total_n
    
    dc_crime %>%
      mutate(REPORT_DAT = parse_datetime(as.character(dc_crime$REPORT_DAT), format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
      mutate(Year = year(REPORT_DAT)) %>%
      group_by(sector, offense.text,Year) %>%
      
      summarise(n = n()) %>%
      filter(sector != '') %>%
      filter(Year==input$crime_year_map1) %>% 
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
      crime_by_sector$`assault`,
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
      colorNumeric(c("green", "red"), 10:4500)
    
    leaflet(police_sector_map , options = leafletOptions(minZoom = 11, maxZoom = 12)) %>%
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
        Year==input$crime_year_map,
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

output$lmmonth<-renderPlot(
  {

    
    stage_fit<-lm (mvt$Total~  I(mvt$Month<=7) + 
                     I((mvt$Month<=7)*mvt$Month)+
                     I(mvt$Month>7) + 
                     I((mvt$Month>7)*mvt$Month))
    qplot(mvt$Month,mvt$Total)+
      geom_line(aes(mvt$Month, fitted.values(stage_fit)),colour=3)+
      theme_bw()+
      xlab("Month") +
      ylab("Crime Number") 
      #ggtitle("Each month's crime number")
    
  }
)
output$mlm <- renderPrint({
  stage_fit<-lm (mvt$Total~  I(mvt$Month<=7) + 
                   I((mvt$Month<=7)*mvt$Month)+
                   I(mvt$Month>7) + 
                   I((mvt$Month>7)*mvt$Month))
  summary(stage_fit)$r.squared
})
  
output$night<-renderPlot(
  
  {
    tidy_dc %>% 
      mutate(Week_day=wday(REPORT_DAT)) %>%
      group_by(Year, Month,SHIFT,Week_day, Year) %>%
      na.omit() %>% 
      summarise(n=n()) -> tidy_week
    tidy_week %>% 
      filter(SHIFT!='day')%>%
      ggplot(mapping=aes(x=as.factor(Week_day),y=n,colour=SHIFT))+
      geom_boxplot()+
      facet_wrap(~ Year)+
      theme_bw()+
      xlab("Week_day")+
      ylab("Total Number") 
  }
)  
  
output$nlm <- renderPrint({
tm<-lm(n~as.factor(Hour),data = time_n)
#summary(tm)
})









}







dc_crime <- read.csv("dc_crimes_search_results.csv")
dc_crime %>% 
  mutate(offense.text=recode(offense.text, `assault w/dangerous weapon` = "assault")) ->dc_crime
dc_crime %>% 
  mutate(REPORT_DAT=parse_datetime(as.character(dc_crime$REPORT_DAT),format = "%Y-%m-%dT%H:%M:%OSZ")) %>% 
  mutate(Month=month(REPORT_DAT)) %>% 
  mutate(Year=year(REPORT_DAT)) %>%
  mutate(Day=mday(REPORT_DAT)) %>%
  mutate(Hour=hour(REPORT_DAT)) %>% 
  #group_by(offense.text,Year,Month) %>% 
  mutate(Time=make_date(year = Year,month = Month)) %>% 
  filter(Year!=2020) %>% 
  dplyr::select(-NEIGHBORHOOD_CLUSTER,-CENSUS_TRACT,-offensegroup,-END_DATE,-YBLOCK,-WARD,-offensekey,-BID,-PSA,-`ucr.rank`,-BLOCK_GROUP,-VOTING_PRECINCT,-XBLOCK,BLOCK,-location)->tidy_dc
tidy_dc %>% 
  group_by(Year,Month) %>% 
  summarise(Total=n())->mvt
tidy_dc %>% 
  filter(SHIFT=="midnight") %>% 
  group_by(Time,Hour) %>%
  na.omit() %>% 
  summarise(n=n())->time_n


shinyApp(ui = ui, server = server)
