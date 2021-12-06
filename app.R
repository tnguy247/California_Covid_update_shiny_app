#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(scales)
library(ggspatial) 
library(dplyr)
library(ggplot2)
library(leaflet)
library(RCurl)
library(plotly)
library(shinydashboard)
library(hrbrthemes)
library(zoo)

df <- read_csv("Data/Counties_Geo copy123456.csv")
df_age <-read_csv("Data/Age.csv")
df_race <-read_csv("Data/Race_fixed.csv")
df_hospital <- read_csv("Data/hospitalizity.csv")
spreading_df <- df %>%
  mutate_at(vars(Lat, Long), funs(as.numeric)) %>%
  filter(!is.na(Lat))

hrbrthemes::import_roboto_condensed()
# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    #titlePanel("COVID-19 Situation in California 2020-2021"),
    dashboardHeader(title = "COVID-19 Update"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("home")),
        menuItem("Infected Area", tabName = "infected", icon = icon("map")),
        menuItem("How hard Covid hit Cali?", tabName = "hard", icon = icon("bar-chart-o", verify_fa = FALSE)),
        menuItem("Average of new cases every 7 days", tabName = "average", icon = icon("bar-chart-o", verify_fa = FALSE)),
        menuItem("How Cali handles Covid?", tabName = "handle", icon = icon("dashboard", verify_fa=FALSE))
        
      )
    ),
    # Sidebar with a slider input for number of bins 
    dashboardBody(
      h2("COVID SITUATION IN CALIFORNIA 03/2020-03/2021"),
      
      tabItems(
        tabItem(
          tabName = "overview",
          
          h3("This data set is provided by the Government of California."),
          h3("This data set is about the Covid situation in California in the period of time from 03/2020 to 03/2021."),
          h3("This data set contains multiple tables about the total cases, age groups and races get hit by Covid, testing availability, ICU bed availability,..."),
         
          h3("As California reopens, Californians must follow best practices and make informed decisions to save lives in the state."),
          h3("By understanding trends and geographic spread of COVID-19 displayed through the graphs on our app, local goverment can take action to protect their citizens and each citizen can also know what they need to do to protect themselves and their loved ones!"),
          img(src = "covid image.png")
        ),
        
        tabItem(
          tabName = "infected",
          h4("Which county gets hit the most by COVID?"),
          leafletOutput("map1")
        ),
        tabItem(
          tabName = "hard",
          sidebarLayout(
            sidebarPanel(
              checkboxInput(inputId = "age", label = "Age", value = TRUE),
              checkboxInput(inputId = "race", label = "Race", value = TRUE),
              actionButton(inputId = "go", label = "Update"),
              #sliderInput(inputId= "death", label = "Number of death caused by COVID", 
                          #min = 0, max = 100000, value = c(0, 100000))
              hr(),
              selectInput(inputId = "countyID123", label = "Select a county", choices = df$county),
            ),
            mainPanel(
              tabsetPanel(
                tabPanel("Age", plotOutput("distPlot")),
                tabPanel("Race", plotOutput("distRace")),
                tabPanel("The notable increase in the number of hospitalized covid patients", plotlyOutput("spreading", height = 800, width = 3000)),
                tabPanel("The increase in the number of death caused by Covid for each county", plotOutput("newCases")),
                
              )
            )
            
          )
        ),
        
        tabItem(
          tabName = "handle",
        
    
    sidebarLayout(
        sidebarPanel(
            #h3("Select to see how hard COVID hit Cali?"),
            #checkboxInput(inputId = "age", label = "Age", value = TRUE),
            #checkboxInput(inputId = "race", label = "Race", value = TRUE),
            #actionButton(inputId = "go", label = "Update"),
            #hr(),
            #h3("How Cali handles COVID-19?"),
            selectInput(inputId = "countyID", label = "Which county has testing available?", choices = df$county),
            #dateRangeInput(inputId="date", label = "Select date", start = "2020-03-18", end = "2021-03-11"),
            #sliderInput(inputId="dateFix", label="Date", 
                        #min = as.Date("2020-03-18"), max = as.Date("2021-03-11"),
                        #value=as.Date("2021-03-11"), timeFormat = "%b %Y"),
            sliderInput(inputId="icu", label="ICU Beds Availability for each county", 
                        min = 0, max = 800,
                        value=c(0,800))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            #tabPanel("Age", plotOutput("distPlot")),
            #tabPanel("Race", plotOutput("distRace")),
            #tabPanel("Which county gets hit the most by COVID?", leafletOutput("map1")),
            #tabPanel("The notable increase in the number of death caused by COVID", plotlyOutput("spreading", height = 600, width = 4000)),
            tabPanel("How Cali handles the COVID?", leafletOutput("map")),
            hr(),
            tabPanel("ICU Beds Availability", plotOutput("icu"))
            
           
        )
    )
  )
),
 tabItem(
   tabName = "average",
   
   sidebarLayout(
     sidebarPanel(
       selectInput(inputId = "countyID456", label = "Which county has testing available?", choices = df$county)
     ),
     mainPanel(
       tabPanel("The average of new cases every 7 days for each county", plotOutput("seven_days"))
     )
   )
 )
)
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  spreading_df <- df %>%
    mutate_at(vars(Lat, Long), funs(as.numeric)) %>%
    filter(!is.na(Lat))
  
    observe({
        age_react <- eventReactive(input$go, {input$age})
        output$distPlot <-renderPlot({
          if(input$age){
            v <- age_react()
            v %>%
              ggplot(data = df_age, mapping=aes(x=AGE_GROUP,y = TOTALPOSITIVE))+
              geom_col() + scale_y_discrete(breaks=c("0.0e +00","5.0e+07","1.0e+08","1.5e+08","2.0e+08"),
                                            labels=c("0","500K","1000K","1500K","2000K"))
          }
          })
    })
  
    
  observe({
    race_react <- eventReactive(input$go, {input$race})
    output$distRace <- renderPlot({
      if(input$race){
        g<-race_react() 
        g %>%
          ggplot(data = df_race, mapping=aes(x=RACE_ETHNICITY,y = CASES))+
          geom_boxplot()
      }
      
    })
  })
    
    #radius_react <- reactive(input$date,{spreading_df$TOTALCOUNTCONFIRMED/10000})
    #p1 <- reactive(spreading_df[spreading_df$COUNTY == input$county])
  df_groups <- reactive({
    #water_data_df[water_data_df$station_name = input$station]
    df %>% filter(county %in% input$countyID)
  })
  
  df_groups123 <- reactive({
    #water_data_df[water_data_df$station_name = input$station]
    df %>% filter(county %in% input$countyID123)
  })
  observe({
    
  
  #dplyr::filter(county == input$) %>%
  output$newCases <- renderPlot({
    df_groups123() %>%
    ggplot2::ggplot(aes(x = Date,
                        y = NEWCOUNTDEATHS)) +
    geom_col(fill= 'red') +
    ggplot2::labs(title = "Daily count of death caused by COVID",
                  subtitle = "Rolling average between 2020-06-01 and 2020-06-20",
                  y = "New Death",
                  x = "Day") 
  })
  })
  new_df<-df %>%
    mutate(new_confirmed_cases = TOTALCOUNTCONFIRMED - dplyr::lag(x = TOTALCOUNTCONFIRMED, n = 1,
                                                            order_by = Date)) 
    
    
    
  
    output$map <- renderLeaflet({
    leaflet(df) %>%
      addTiles() %>%
        fitBounds(min(-df$Long), min(df$Lat), max(-df$Long),max(df$Lat))
    })
      
      observe({
        
        leafletProxy("map", data=df_groups()) %>% 
          addMarkers( lng=~-Long, lat = ~Lat,
                            popup=paste("County: ", input$countyID, "<br>", "<strong>","Testing is available!","</strong>"
                            )
                            #popup=paste("County: ", spreading_df$COUNTY,"<br>", "Total cases: ", spreading_df$TOTALCOUNTCONFIRMED),
                             #radius = ~TOTALCOUNTCONFIRMED/10000,
                             #color = "red",
                             #stroke=FALSE,
                             #fillOpacity = 0.5)
        
          )
      })
      
      output$map1 <-renderLeaflet({
        leaflet(df) %>%
          addTiles() %>%
          addCircleMarkers( lng=-df$Long, lat = df$Lat,
                            popup= paste("County: ", df$county, "<br>", "Total cases: ", df$TOTALCOUNTCONFIRMED,  "<br>", "Percentage of population got hit by Covid: ", df$NEWCOUNTCONFIRMED*100/df$population),
                            radius = (df$TOTALCOUNTCONFIRMED/df$population)*50,
                            color = "red",
                            stroke=FALSE,
                            fillOpacity = 0.5)
      })
      
      #p1 <- reactive({df[df$DATE >= input$dateFix[1] & df$DATE <= input$dateFix[2],]})
      #p2 <- eventReactive(input$go, {input$date})
      observe({
        #p12 <- reactive({df[df$TOTALCOUNTDEATHS >= input$death[1] & df$TOTALCOUNTDEATHS <= input$death[2],]})
        output$spreading <- renderPlotly({
          #countDeath <- df %>% 
            #filter(TOTALCOUNTDEATHS == input$death)
        #l12 <- p12()
        
        #l12 %>%
          #countDeath %>%
            ggplot(data = df_hospital, mapping =aes(x=TODAYS_DATE, y = HOSPITALIZED_COVID_CONFIRMED_PATIENTS)) + 
            geom_line() + 
            labs(x = "Month",
                 y = "Number of Hospitalized Covid Patients") + 
            scale_x_date(date_labels = "%Y") +
            facet_wrap(~COUNTY, scales = "free_y") 
           
           
        })
      })
      
      
      #p1 <- reactive({df_hospital[df_hospital$ICU_AVAILABLE_BEDS >= input$icu[1] & df_hospital$ICU_AVAILABLE_BEDS <= input$icu[2],]})
      #observe({
        #p1 <- reactive({df_hospital[df_hospital$ICU_AVAILABLE_BEDS >= input$icu[1] & df_hospital$ICU_AVAILABLE_BEDS <= input$icu[2],]})
        #output$icu <-renderPlot({
          #l <- p1()
          #l %>% 
              #ggplot(data = df_hospital, mapping = aes(x = reorder(COUNTY, n), y = n))+
              #geom_col()+
              #coord_flip()
        #})
        
      #})
      
      output$icu <- renderPlot({
        df_hospital$COUNTY <- as.factor(df_hospital$COUNTY)
        icuView <- seq(input$icu[1], input$icu[2])
        
        icuNew <- df_hospital[which(df_hospital$ICU_AVAILABLE_BEDS %in% icuView),]
        
        ggplot(data = icuNew, mapping = aes(x = reorder(COUNTY, ICU_AVAILABLE_BEDS), y = ICU_AVAILABLE_BEDS)) +
          geom_col() +
          coord_flip() + scale_y_discrete(breaks=c("0","50000","100000","150000","200000"),
                                          labels=c("0","150","300","450","600"))
      })
      #setView(lng=-100,lat=50, zoom =-2)

    #observe({
      #leafletProxy("map", data = spreading_df) %>%
        #clearMarkers() %>%
        #addProviderTiles("CartoDB.Positron") %>%
        #addMarkers(lng =-spreading_df$Long, lat = spreading_df$Lat,
                   #popup = spreading_df$COUNTY
                   #label = labels,
                   #labelOptions = labelOptions(noHide = F, offset=c(0,-12)))
    #)
#})
 
    
  #r_birthplace_map <- leaflet() %>%
   # addTiles() %>%  # use the default base map which is OpenStreetMap tiles
    #addMarkers(lng=-spreading_df$Long, lat=spreading_df$Lat,
               #popup=spreading_df$COUNTY)
  #r_birthplace_map
      df_7days <- df %>%
        dplyr::arrange(desc(county)) %>%
        dplyr::group_by(county) %>%
        dplyr::mutate(new_case_7 = zoo::rollmean(NEWCOUNTCONFIRMED, k = 7, fill = NA)) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_longer(names_to = "rolling_mean_key",
                          values_to = "rolling_mean_value",
                          cols = c(new_case_7
                          )) %>%
        # after may 15
        dplyr::filter(Date >= lubridate::as_date("2020-03-15") &
                        # before june 20
                        Date <= lubridate::as_date("2021-04-15")) 
      
      
      df_groups456 <- reactive({
        #water_data_df[water_data_df$station_name = input$station]
        df_7days %>% filter(county %in% input$countyID456)
      })
      
      observe({
        
      
      output$seven_days <- renderPlot({
        df_groups456() %>%
      
      
        #df_7days %>%
        #dplyr::arrange(Date) %>%
        #dplyr::filter(county == "Santa Clara") %>%
        #dplyr::select(county, Date, NEWCOUNTCONFIRMED,
                      #new_case_7) %>%
        #utils::head(7)
          
      
     
        #dplyr::filter(county == "San Luis Obispo") %>%
        #tidyr::pivot_longer(names_to = "rolling_mean_key",
                            #values_to = "rolling_mean_value",
                            #cols = c(new_case_7
                            #)) %>%
        # after may 15
        #dplyr::filter(Date >= lubridate::as_date("2020-03-15") &
                        # before june 20
                        #Date <= lubridate::as_date("2021-04-15")) %>%
        ggplot2::ggplot(aes(x = Date,
                            y = rolling_mean_value,
                            #color = rolling_mean_key, group=1
                            )) +
        ggplot2::geom_line() +  
        ggplot2::labs(title = "Rolling average total New Covid Cases",
                      subtitle = "Between 2020-05-15 and 2020-06-20",
                      y = "New Cases",
                      color = "Metric",
                      x = "Date") #+
        #hrbrthemes::theme_ipsum_rc()
      })
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
