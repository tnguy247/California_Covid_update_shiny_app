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
library(treemap)

df <- read_csv("Data/Counties_Geo copy123456.csv")
df_age <-read_csv("Data/Age.csv")
df_race <-read_csv("Data/Race_fixed.csv")
df_hospital <- read_csv("Data/hospitalizity.csv")
spreading_df <- df %>%
  mutate_at(vars(Lat, Long), funs(as.numeric)) %>%
  filter(!is.na(Lat))

df_race <- mutate(df_race, pop_etnicity= (PERCENT_CA_POPULATION * 39538223)/100)
df_race <- mutate(df_race, percent_each_race = CASES/pop_etnicity)

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
          h3("By understanding trends and geographic spread of COVID-19 displayed through the graphs on our app, local goverment can take action to protect their citizens and each citizen can also know what they need to do to protect themselves and their loved ones!")
          #img(src = 'covidImage.png')
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
              
              selectInput(inputId = "countyID123", label = "Select a county", choices = df$county),
            ),
            mainPanel(
              tabsetPanel(
                tabPanel("Age", plotOutput("distPlot")),
                tabPanel("Race", plotOutput("distRace")),
                tabPanel("The notable increase in the number of hospitalized covid patients", plotlyOutput("spreading", height = 800, width = 1000)),
                tabPanel("The increase in the number of death caused by Covid for each county", plotOutput("newCases")),
                
              )
            )
            
          )
        ),
        
        tabItem(
          tabName = "handle",
        
    
    sidebarLayout(
        sidebarPanel(
            
            selectInput(inputId = "countyID", label = "Which county has testing available?", choices = df$county),
            
            sliderInput(inputId="icu", label="ICU Beds Availability for each county", 
                        min = 0, max = 800,
                        value=c(0,800))
        ),

        mainPanel(
          tabsetPanel(
            
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
       selectInput(inputId = "countyID456", label = "Select a county", choices = df$county)
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
  
        output$distPlot <-renderPlot({
          
              ggplot(data = df_age, mapping=aes(x=AGE_GROUP,y = TOTALPOSITIVE))+
              geom_col() + scale_y_discrete(breaks=c("0.0e +00","5.0e+07","1.0e+08","1.5e+08","2.0e+08"),
                                            labels=c("0","500K","1000K","1500K","2000K"))+
              labs(x = "Age group", y = "Total positive")
         
          })
   
  
    
    output$distRace <- renderPlot({
      df_race %>% filter(DATE == "3/11/2021") %>%
        filter(RACE_ETHNICITY != "Other" ) %>%
        treemap(df_race, #Your data frame object
              index=c("RACE_ETHNICITY"),  #A list of your categorical variables
              vSize = "percent_each_race",  #This is your quantitative variable
              type="index", #Type sets the organization and color scheme of your treemap
              palette = "Reds",  #Select your color palette from the RColorBrewer presets or make your own.
              title="Distribution of Percentage of Covid Cases per Races/Ethnicity", #Customize your title
              fontsize.title = 14 #Change the font size of the title
      )
    })
  
    
    
  df_groups <- reactive({
    df %>% filter(county %in% input$countyID)
  })
  
  df_groups123 <- reactive({
    df %>% filter(county %in% input$countyID123)
  })
  observe({
    
  
  output$newCases <- renderPlot({
    df_groups123() %>%
    ggplot2::ggplot(aes(x = Date,
                        y = NEWCOUNTDEATHS)) +
    geom_col(fill= 'red') +
    ggplot2::labs(title = "Daily count of death caused by COVID",
                  subtitle = "Rolling average between 2020-03 and 2021-03",
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
        
          )
      })
      
      output$map1 <-renderLeaflet({
        leaflet(df) %>%
          addTiles() %>%
          addCircleMarkers( lng=-df$Long, lat = df$Lat,
                            popup= paste("County: ", df$county, "<br>", "Total cases: ", format(df$TOTALCOUNTCONFIRMED, big.mark =",", scientific = FALSE),  "<br>", "Percentage of population got hit by Covid: ", round(df$NEWCOUNTCONFIRMED*100/df$population,3)),
                            radius = (df$TOTALCOUNTCONFIRMED/df$population)*50,
                            color = "red",
                            stroke=FALSE,
                            fillOpacity = 0.5)
      })
      
      
      observe({
        
        output$spreading <- renderPlotly({
          
            ggplot(data = df_hospital, mapping =aes(x=TODAYS_DATE, y = HOSPITALIZED_COVID_CONFIRMED_PATIENTS)) + 
            geom_line() + 
            labs(x = "Month",
                 y = "Number of Hospitalized Covid Patients") + 
            scale_x_date(date_labels = "%Y") +
            facet_wrap(~COUNTY, scales = "free_y") 
           
           
        })
      })
      
      
      output$icu <- renderPlot({
        df_hospital$COUNTY <- as.factor(df_hospital$COUNTY)
        icuView <- seq(input$icu[1], input$icu[2])
        
        icuNew <- df_hospital[which(df_hospital$ICU_AVAILABLE_BEDS %in% icuView),]
        
        ggplot(data = icuNew, mapping = aes(x = reorder(COUNTY, ICU_AVAILABLE_BEDS), y = ICU_AVAILABLE_BEDS)) +
          geom_col() +
          coord_flip() + scale_y_discrete(breaks=c("0","50000","100000","150000","200000"),
                                          labels=c("0","150","300","450","600"))+
          labs(y = "ICU Bed Availability", x = "County")
      })
      
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
        ggplot2::ggplot(aes(x = Date,
                            y = rolling_mean_value
                            )) +
        ggplot2::geom_line() +  
        ggplot2::labs(title = "Rolling average total New Covid Cases",
                      subtitle = "Between 2020-03 and 2021-03",
                      y = "New Cases",
                      color = "Metric",
                      x = "Date") 
      })
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
