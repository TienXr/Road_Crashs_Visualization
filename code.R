# library
library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(treemap)
library(lubridate)
library(dplyr)


# Read dataset
road_crash <- read_csv("Road_Crashes_for_five_Years_Victoria.csv")
road_volume <- read_csv("Traffic_Volume.csv")

# fixing data for visualize better
road_crash$accident_time <- gsub("1899/12/30 ", "", road_crash$accident_time)
road_crash$accident_time <- gsub(":[0-9]+:00[+]00", ":00", road_crash$accident_time)

# Setting up choices
severity_choice <- road_crash$severity %>%
  unique()

week_choices <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
month_choices <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
hour_choices <- c("00:00", "01:00","02:00", "03:00","04:00", "05:00", "06:00", "07:00", "08:00", 
                  "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00",
                  "17:00" ,"18:00", "19:00", "20:00", "21:00", "22:00","23:00", "24:00")

# set up road volume data frame
road_volume_lga <- road_volume %>%
  group_by(lga) %>%
  summarise(
    total_volume = sum(volume),
    .groups = "drop" 
  ) %>%
  ungroup()


# UI
ui <- fluidPage(style = "background-color: #C5CBE3;",
  navbarPage(title = "Victoria Road Crash",
             id = "nav_bar",
             tabPanel(
               title = "Home",
               titlePanel(h1("Road Accidents in Victoria, Australia",
                             style = "text-align: center;
                                      font-size: 250%;
                                      font-weight: bold;")),
               br(), br(),br(), br(),
               hr(style = "height: 4px; 
                            width: 500px; 
                            background-color: white;
                            border-radius: 25px;
                            margin: auto;"),
               br(), br(),
               fluidRow(
                 column(
                   12,
                   h2("Motivation Of Project",
                      style = "text-align: center;
                               font-size: 200%;"),
                   p("This project will help the observer to see about the 
                     visualisation of the road crash in Victoria. Thus, this project
                     will help to warn people on the danger on the road if they
                     do not drive carefully. This will also help officer or 
                     analyst for their purpose of analysing on the road crash in
                     Victoria.",
                     style = "text-align: center;
                              font-size: 100%;
                              width: 50%;
                              margin: auto;")
                 )
               ),
               br(), br(),br(), br(),
               hr(style = "height: 4px; 
                            width: 500px; 
                            background-color: white;
                            border-radius: 25px;
                            margin: auto;"),
               br(),
               fluidRow(
                 column(12,
                        h2("The Dataset",
                           style = "text-align: center;
                                    font-size: 200%;"))
               ),
               br(),
               fluidRow(
                 column(3),
                 column(
                   3,
                   h2("Road Crash in Victoria",
                      style = "text-align: center;
                               font-size: 150%;"),
                   p("For the first dataset, this will help us to go into detail the road crash factors from
                     2015 to 2020 to see which are the main factors that affect
                     the road crash in Victoria. For this, we will only consider
                     the factors that are from this dataset. You can get free
                     dataset from below. The dataset that we use is already cleaned but below data is not cleaned yet."),
                   a("Go To Dataset...", href="https://discover.data.vic.gov.au/dataset/road-crashes-for-five-years-victoria")  # adding reference
                 ),
                 column(
                   3,
                   h2("Road Volume in Victoria",
                      style = "text-align: center;
                               font-size: 150%;"),
                   p("With this dataset, we can see if there is relationship between
                     traffic volume and the road accident in Victoria. You can get free
                     dataset from below. The dataset that we use is already cleaned but below data is not cleaned yet."),
                   a("Go To Dataset...", href="https://discover.data.vic.gov.au/dataset/traffic-volume") # adding reference
                 ),
                 column(3)
               ),
               br(), br(),
               hr(style = "height: 4px; 
                            width: 500px; 
                            background-color: white;
                            border-radius: 25px;
                            margin: auto;"),
               br(), br(),
               fluidRow(
                 column(3),
                 column(3,
                        img(src='img1.png', width="100%", height = "50%")
                 ),
                 column(
                   3,
                   h2("Factor Affecting Crashs",
                      style = "text-align: center;
                               font-size: 150%;"),
                   p("In this part, we will use the first dataset to investigate the
                     factors that affect the causes of the crash around Victoria. We
                     will go into detail to the relatioship between factors that
                     recorded of the accident cases."),
                 ),
                 column(3)
               ),
               br(), br(),
               hr(style = "height: 2px; 
                            width: 250px; 
                            background-color: white;
                            border-radius: 25px;
                            margin: auto;"),
               br(), br(),
               fluidRow(
                 column(3),
                 column(3,
                        img(src='img2.png', width="100%", height = "50%")
                 ),
                 column(
                   3,
                   h2("Road Accidents Change Over Time",
                      style = "text-align: center;
                               font-size: 150%;"),
                   p("In this part, the analysis will help us to answer question \"How
                     does crash change over times?\", we can see if there is any improvement
                     over time or it might be worse.")
                 ),

                 column(3)
               ),
               br(), br(),
               hr(style = "height: 2px; 
                            width: 250px; 
                            background-color: white;
                            border-radius: 25px;
                            margin: auto;"),
               br(), br(),
               fluidRow(
                 column(3),
                 column(3,
                        img(src='img3.png', width="100%", height = "50%")
                 ),
                 column(
                   3,
                   h2("Road Volume Relation With Road Crash",
                      style = "text-align: center;
                               font-size: 150%;"),
                   p("In this part, we will see if the road volume factor
                     affect the number of road accidents which is not recorded in
                     the first dataset. Thus, I use other dataset to visualize the
                     relation between road volume and number of crash in Victoria."),
                 ),
                 column(3)
               ),
               br(), br(),
               hr(style = "height: 4px; 
                            width: 500px; 
                            background-color: white;
                            border-radius: 25px;
                            margin: auto;"),
               br()
             ),
             tabPanel(title = "Factor Affecting Crashs",
                        sidebarLayout(
                          mainPanel(width = 8,
                            fluidRow(
                              column(style = "border-style: solid;
                                              border-radius: 25px;
                                              border: 1px solid black;
                                              padding: 10px;
                                              margin-bottom: 10px",
                                12,
                                plotOutput("age_vis")
                              ),
                              column(style = "border-style: solid;
                                              border-radius: 25px;
                                              border: 1px solid black;
                                              padding: 10px;
                                              margin-bottom: 10px",
                                12,
                                plotOutput("speed_light_vis")
                              ),
                              column(style = "border-style: solid;
                                              border-radius: 25px;
                                              border: 1px solid black;
                                              padding: 10px",
                                     12,
                                     plotOutput("road_alcohol_vis")
                              )
                            )
                          ),
                          sidebarPanel(width = 4,
                                       h2("Crashs Cause",
                                          style = "text-align: center;
                                    font-size: 200%;"),
                                       p("In this section, we can use the exploration
                              to answer about following questions: What are
                              the major causes of the crash in Victoria? And
                              which is the most frequent factor?"),
                                       p("We will explore about the following factors:"),
                                       tags$ul(
                                         tags$li("1. Seeing Relationship between Young Drivers, Old Driver and Unlicensed with Number of Crash."),
                                         tags$li("2. Light condition and speed zone with number of Crash."),
                                         tags$li("3. Metro, Alcohol and Divided Road with number of Crash.")
                                       ),
                                       p("Due to large amount of features, multiple features will be put
                              to each graph so we can see all the relationships with enough
                              graphs."),
                                       p("One interest part is that if you select only fatal accident (crash
                                          cause death) then 
                                          the speed zone with highest crash is 100 km/hr but not
                                          60 km/hr like other cases. Then we know that speed zone affect
                                          the danger of crash."),
                                       pickerInput(
                                         "display_number", "Display number?:",
                                         choices = c("Yes", "No"),
                                         selected = "No"
                                       ),
                                    
                                       fluidRow( 
                                         column(12, style ="background-color: lightblue;
                                                            border-radius: 25px;
                                                            padding: 10px;",
                                                checkboxGroupInput("severity_picker", 
                                                                   p("Severity"), 
                                                                   choices = severity_choice,
                                                                   selected = severity_choice[1])
                                         )
                                       ),
                                       p("All the graphs (except below graph) are changed depending
                              on the choices you select from the severity picker. To show number on bar chart then choose Yes for Display number."),
                                       p("Below graph is about severity and the count of crash on each severity.
                              It will help you better with the choosing the category you want. 
                              You can also choose data to appear on different severity on the picker box."),
                                       br(),
                                       plotOutput("severity_vis"),
                                       br(),

                          )
                        )
                      ),
              tabPanel(title = "Crash Change Over Time",
                sidebarLayout(
                  sidebarPanel(width = 3,
                    h2("Crash Change Over Time",
                    style = "text-align: center;
                             font-size: 200%;"),
                    p(style = "font-size: 18px","In this section, we can use the exploration
                       to answer about questions: How does the road
                       crash change over time?"),
                    br(),
                    pickerInput( # display crash number or not
                      "display", "Display crash number?:",
                      choices = c("Yes", "No"),
                      selected = "No"
                    ),
                    pickerInput( # This value will change the conditional panel below
                      "typeTime", "Choose time type to manipulate:",
                      choices = c("Year", "Weekday", "Month", "Hour"),
                      selected = "Year"
                    ),
                    conditionalPanel( # only one of the conditionalPanel will display depending on the above value
                      condition = "input.typeTime == 'Year'",
                      sliderInput("year", "Year:",
                                  min = 2016, max = 2020, value = c(2016,2020)),
                    ),
                    conditionalPanel(
                      condition = "input.typeTime == 'Weekday'",
                      sliderTextInput("weekday", "Weekday:", choices = week_choices,
                                      selected = c("Monday", "Sunday"), grid = TRUE),
                    ),
                    conditionalPanel(
                      condition = "input.typeTime == 'Month'",
                      sliderTextInput("month", "Month:", choices = month_choices,
                                  selected = c(month_choices[1], month_choices[5]), grid = TRUE),
                    ),
                    conditionalPanel(
                      condition = "input.typeTime == 'Hour'",
                      sliderTextInput("hour", 
                                      "Hour:",
                                      choices = hour_choices,
                                      selected = c(hour_choices[1], hour_choices[25]),
                                      grid = TRUE
                      )
                    ),
                    p(style = "font-size: 18px","By choosing type of time which are Year, Month, Weekday or Hour, you can
                      filter only the data that in the time range to see different change in different
                      timeline. By that, we can answer the above questions with different aspects. To view detail count of crash in line chart then choose Yes from first picker category"),
                    p(style = "font-size: 18px","Both of the graphs will be changed by the choice of the slider so you can also see the change over time
                      in the crash distribution on the map. "),
                    p(style = "font-size: 18px","You can zoom in and out of the map by using scroll on your
                      mouse. To view more informations of the data, you can choose display crash number as Yes. For the map, you can click on the area to see more 
                      informations of that locations.")
                  ),
                  mainPanel(width = 9,
                    fluidRow(
                      column(
                         12,
                         h3("Road Crash Change Overtime in Victoria"),
                         plotOutput("time_vis")
                      ),
                      column(
                        12,
                        h3("Road Crash Distribution in Victoria"),
                        leafletOutput("count_map")
                      ),
                    ),
                  ),
                ),
              ),
             tabPanel(title = "Crash vs Volume",
                      h1("Relationship between Road Crash and Road Volume in Victoria"),
                        fluidRow(
                          sidebarLayout(
                            sidebarPanel(
                              width = 8,
                              fluidRow(
                                column(6,
                                       p(style = "font-size: 16px",
                                            "In this section, we can use the exploration
                                            to answer about questions: How the traffic 
                                            volume affect the accidents? To answer this
                                            question, we need two dataset too see relationship
                                            between the to total crash in Victoria and total volume
                                            in Victoria. We will consider in different crash number
                                            volume in different LGA."),
                                       p(style = "font-size: 16px",
                                         "From the graph below, we can clearly see there is a relationship
                                         between road volume and road crash. You can choose different
                                         sorting method to see the pattern of total crash and total volume."),
                                       p(style = "font-size: 16px",
                                         "To see more clearly the values, you can select to display number to yes and
                                         choose which value to display (volume or crash)"),
                                       p(style = "font-size: 16px",
                                         "To focus on top LGA that has most crash then use slider.")
                                ),
                                column(6,
                                         pickerInput(
                                           "sort", "Choose displaying data method:", # pick value for sorting the graph
                                           choices = c("Sort By LGA Names", "Sort by Value"),
                                           selected = "Sort By LGA Names"
                                         ),
                                         conditionalPanel( # this will display depending on the option above
                                           condition = "input.sort == 'Sort by Value'",
                                           sliderTextInput("top", 
                                                           "How many top to display:",
                                                           choices = c("5", "10", "20", "30","40", "All"),
                                                           selected = "5",
                                                           grid = TRUE
                                           )
                                         ),
                                         pickerInput( # display count number or not
                                           "display_rel", "Display number?:",
                                           choices = c("Yes", "No"),
                                           selected = "No"
                                         ),
                                         conditionalPanel( # this will display depending on the option above
                                           condition = "input.display_rel == 'Yes'",
                                           radioButtons("display_which", "Display which one:",
                                              choices = c("Crash", "Volume"), selected = "Crash"),
                                         ),
                                       ),

                              )
                            ),
                            mainPanel(
                              width = 12,
                              plotOutput("relation_vis")
                              
                            )
                          )
                        )
                      ))
)

server <- function(input, output, session) {
  ## severity frequency 
  severity_count <- road_crash %>%
    group_by(severity) %>%
    summarise(
      total_crash = n(),
      .groups = "drop"
    ) %>%
    ungroup()
  
  output$severity_vis <- renderPlot({
    if (input$display_number == "Yes") {
      ggplot(data = severity_count, aes(x = severity, y = total_crash, label = total_crash)) +  # adding label if select Yes
        geom_bar(stat="identity", fill = "lightblue") + # add color to bar chart
        geom_text(size = 3, position = position_stack(vjust = 0.5))+ # add label
        labs(x = "severity", y = "Total Crash") +
        theme(axis.text.x = element_text(size = 10, face = "bold")) +
        ggtitle("Severity Frequency")
    } else {
      ggplot(data = severity_count, aes(x = severity, y = total_crash)) +
        geom_bar(stat="identity", fill = "lightblue") +
        labs(x = "severity", y = "Total Crash") +
        theme(axis.text.x = element_text(size = 10, face = "bold")) +
        ggtitle("Severity Frequency")
      
    }
  })
  
  ## Age and Unlicensed
  age_licensed_react <- reactive({
    age_licensed <- road_crash %>%
      filter(severity %in% input$severity_picker) # filter the data to be only in severity that picked
    
    age_licensed <- age_licensed %>%
      group_by(severity) %>%
      summarise(
        unlicensed = round(sum(unlicensed) / n(), 2), # calculate average and round it to 2
        old_driver = round(sum(old_driver) / n(), 2),
        young_driver = round(sum(young_driver) / n(), 2)
      )
    
    age_licensed %>%
      gather(key = "factors", value = "count", 2:4) # gather so columns unlicensed, old_driver, young_driver into one column as value
    
  })
  
  output$age_vis <- renderPlot({
    if (input$display_number == "Yes") {
      ggplot(data = age_licensed_react(), aes(x = factors, y = count, label = paste0(severity,": ", count))) + # format the label to display
        geom_bar(stat="identity", aes(fill = severity), color = "black") + # add color as black for border
        geom_text(size = 3, position = position_stack(vjust = 0.5)) + # add label
        labs(y = "Number of Crash") +
        theme(axis.text.x = element_text(size = 10, face = "bold"),
              plot.title = element_text(size=17)) +
        ggtitle("Average of Old Driver, Young Driver and Unlicensed")
      
    } else {
      ggplot(data = age_licensed_react(), aes(x = factors, y = count)) +
        geom_bar(stat="identity", aes(fill = severity), color = "black") +
        labs(y = "Number of Crash") +
        theme(axis.text.x = element_text(size = 10, face = "bold"),
              plot.title = element_text(size=17)) +
        ggtitle("Average of Old Driver, Young Driver and Unlicensed")
    }
  })
  
  # Light Condition and Speed Zone
  speed_light_react <- reactive({
    road_crash %>%
      filter(severity %in% input$severity_picker) %>%
      group_by(light_condition, speed_zone) %>% # group by light condition and speed zone
      summarise(
        count_crash = n(),
        .groups = "drop"
      ) %>%
      ungroup() %>%
      filter(!(light_condition %in% c("Dark Street lights unknown", "Unk."))) %>% # filter the data that do not give helpful information to reduce confusing to audience
      filter(!(speed_zone %in% c("Camping grounds or off road", "Not known", "Other speed limit"))) # filter the data that do not give helpful information to reduce confusing to audience
  })
  
  output$speed_light_vis <- renderPlot({
    treemap(speed_light_react(), index=c("light_condition","speed_zone"), vSize="count_crash", type="index", # tree map with light condition group and speed zone as subgroup
            fontsize.labels=c(24, 12),                
            fontcolor.labels=c("white","black"), # font color to label
            border.col=c("black","white"), # font color to border of group and subgroup
            border.lwds = c(5, 2), # width for border 
            bg.labels=c("transparent"),  # give transparent to label
            fontface.labels=c(2),  # give the style for label (bold)
            align.labels=list(
              c("center", "center"),  # position the label
              c("left", "top")
            ),
            overlap.labels=0.5,                      
            inflate.labels=F,
            title = "Light Condition and Speed Zone",
            fontsize.title= 18
            
    )
  })
  
  # road divided or not/ Metro/ alcohol related
  road_alcohol_react <- reactive({
    ra <- road_crash %>%
      filter(severity %in% input$severity_picker) %>%  # filter only the severity that chosen
      group_by(alcohol_related, divided_road) %>% 
      summarise(
        count_crash = n(),
        .groups = "drop"
      ) %>%
      ungroup() %>%
      drop_na()
    
    ra$alcohol_related <- gsub("Yes", "Related", ra$alcohol_related) # change the name for reducing confusing
    ra$alcohol_related <- gsub("No", "Not Related", ra$alcohol_related)
    ra
  })
  
  output$road_alcohol_vis <- renderPlot({
    treemap(road_alcohol_react(), index=c('divided_road', "alcohol_related"), vSize="count_crash", type="index",
            fontsize.labels=c(25, 14),
            fontface.labels=c(2, 3),
            border.col=c("black","white"),
            border.lwds = c(5, 3),
            fontcolor.labels=c("white","black"),  
            bg.labels=c("transparent"),       
            align.labels=list(
              c("center", "center"),
              c("left", "top"),
              c("left", "center")
              
            ),
            title.legend = "divided road or not",
            overlap.labels=0.5,                      
            inflate.labels=F,
            title = "Alcohol Related, Divided/Non-divided road",
            fontsize.title= 18
    )
  })
  
  # to force the check box to have selected values
  observe({
    if (length(input$severity_picker) < 1) {
      updateCheckboxGroupInput(session, "severity_picker", selected = severity_choice[1])
    }
  })
  
  ################# Second Tab #####################
  
  # Time series plot
  time_range <- reactive({
    if (input$typeTime == "Year") { # check the type to display
      tr <- road_crash
      tr$accident_date <- format(tr$accident_date, "%Y-%m") # format to yeah and month only for better visual
      tr <- tr %>%
        filter(as.character(input$year[1]) <= substring(accident_date,1,4) & substring(accident_date,1,4) <= as.character(input$year[2])) %>% # filter by year 
        group_by(accident_date) %>% # group by date column
        summarise(
          count = n(),
          .groups = "drop"
        ) %>%
        ungroup()

    } else if (input$typeTime == "Month") {
      tr <- road_crash
      tr$accident_date <- month_choices[month(road_crash$accident_date)]
      tr$accident_date <- factor(tr$accident_date, levels = month_choices, ordered = TRUE) # factor month for ordering and filtering
      
      tr <- tr %>%
        filter(input$month[1] <= accident_date & accident_date <= input$month[2]) %>% # filter by month 
        group_by(accident_date) %>% # group by date column
        summarise(
          count = n(),
          .groups = "drop"
        ) %>%
        ungroup()
      
    } else if (input$typeTime == "Weekday"){
      tr <- road_crash
      tr$accident_date <- weekdays(tr$accident_date) # get weekday from the date
      tr$accident_date <- factor(tr$accident_date, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE) # factor weekday for ordering and filter
      tr <- tr %>%
        filter(input$weekday[1] <= accident_date & accident_date <= input$weekday[2]) %>%
        group_by(accident_date) %>% # group by date column
        summarise(
          count = n(),
          .groups = "drop"
        ) %>%
        ungroup()
      
    } else {
      tr <- road_crash %>%
        filter(input$hour[1] <= accident_time & accident_time <= input$hour[2]) %>% # filter the hour
        group_by(accident_time) %>% # group by date column
        summarise(
          count = n(),
          .groups = "drop"
        ) %>%
        ungroup()
    }
    tr
  })
  
  output$time_vis <- renderPlot({
    if (input$typeTime == "Year") { # change x axis title and angle of x and label position depending on the input
      x_title = "Year" 
      angle_x <- 90
      label_location <- -90
      
    } else if (input$typeTime == "Month") {
      x_title = "Month"
      angle_x <- 0
      label_location <- -90
      
    } else if (input$typeTime == "Weekday"){
      x_title = "Weekday"
      angle_x <- 0
      label_location <- -90
      
    } else {
      x_title = "Hour"
      angle_x <- 0
      label_location <- -500
    }
    
    reactive_df <- time_range()
    colnames(reactive_df) <- c("x", "y")
    # plot
    p <- ggplot(reactive_df, aes(x = x)) +
      geom_path(aes(y = y, group = 1), color = "#3ACAFF", size = 1.5) + # add the line
      geom_point(aes(y = y), color = "#FF873A", size = 4) +  # add the point
      theme(axis.text.x = element_text(angle = angle_x)) +
      labs(x = x_title, y = "Total Crash")
    
    if (input$display == "Yes") {
      p <- p + geom_text(aes(y = y, label = y), nudge_y = label_location, check_overlap = T) # add text if the choice Yes is chosen
    }
    p
  })
  
  # Map 
  map_data <- reactive({
    react_data <- road_crash # same as above for filtering data for map
      if (input$typeTime == "Year") {
        react_data$accident_date <- format(react_data$accident_date, "%Y-%m")
        react_data <- react_data %>%
          filter(as.character(input$year[1]) <= substring(accident_date,1,4) & substring(accident_date,1,4) <= as.character(input$year[2]))
      } else if (input$typeTime == "Month") {
        react_data$accident_date <- month_choices[month(road_crash$accident_date)]
        react_data$accident_date <- factor(react_data$accident_date, levels = month_choices, ordered = TRUE)
        
        react_data <- react_data %>%
          filter(input$month[1] <= accident_date & accident_date <= input$month[2])

      } else if (input$typeTime == "Weekday"){
        react_data$accident_date <- weekdays(react_data$accident_date)
        react_data$accident_date <- factor(react_data$accident_date, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE)
        react_data <- react_data %>%
          filter(input$weekday[1] <= accident_date & accident_date <= input$weekday[2])

      } else {
        react_data <- react_data %>%
          filter(input$hour[1] <= accident_time & accident_time <= input$hour[2])
      }

    road_crash_lga <- react_data %>%
      group_by(lga) %>%
      summarise(
        total_crash = n(),
        lat = median(lat), # getting only one value of lat and long for each LGA
        lon = median(long),
        .groups = "drop"
      ) %>%
      ungroup()
    
    road_crash_lga <- road_crash_lga %>%
      drop_na() %>% # drop NA values
      unique()
    
    road_crash_lga
  })

  output$count_map <- renderLeaflet({
    leaflet(data = map_data()) %>% addTiles() %>%
      setView( lat=-37.81, lng=144.805540522615, zoom=10) %>% # set default zoom in one location
      addCircles(
        lng = ~lon,
        lat = ~lat,
        radius = ~total_crash * 2, 
        color = "#166EE1",
        fillColor = "orange",
        fillOpacity = 0.4,
        popup = paste("LGA: ", map_data()$lga, "<br>",     # adding tool tips
                      "Total Crash: ", map_data()$total_crash, "<br>")
      )
  })
  
    ################# 3rd tab ##################
    relation <- reactive({
      relation_data <- road_crash %>%
        group_by(lga) %>% # group data by LGA
        summarise(
          total_crash = n(),
          .groups = "drop" 
        ) %>%
        ungroup()
      
      # getting volume from volume data frame
      relation_data$total_volume <- road_volume_lga$total_volume[match(relation_data$lga, road_volume_lga$lga)]
      
      if (input$sort == "Sort By LGA Names") {
        relation_data <- relation_data %>% # if not sort then keep as default 
          drop_na() %>%
          unique() 
      } else {
        if (input$top == "All") { # if sort then use arrange to sort by total_crash
          relation_data <- relation_data %>% # if display all then take all and sort 
            drop_na() %>%
            unique() %>%
            arrange(total_crash)
        } else {
          relation_data <- relation_data %>%
            drop_na() %>%
            unique() %>%
            arrange(total_crash) %>%
            tail(strtoi(input$top))   # if not getting all then getting top by tail
        }
      }
      relation_data
    })
  
  output$relation_vis <- renderPlot({
    active_data <- relation()
    # scale value for second axis
    scale_range <- max(active_data$total_volume) / max(active_data$total_crash)
    
    if (input$sort == "Sort By LGA Names") { # if not sort then keep normal 
      gg1 <- ggplot(data = active_data, aes(lga)) +
        geom_bar(aes(y = total_crash, fill = "Total Crash"), stat="identity") +  # adding bar chart
        geom_path(aes(y = total_volume / scale_range, group = 1, color = "Volume"), size = 1.5) + # adding line chart with value normalized by scale value before
        scale_color_manual(" ", values = c("Volume" = "#FF873A")) + # adding line chart with value normalized by scale value before
        scale_fill_manual("", values = "#3ACAFF") + # add color for bar chart
        scale_y_continuous(sec.axis = sec_axis(~.*scale_range, name = "Volume")) + # add second y-axis
        theme(axis.text.x = element_text(angle = 90), # rotate the x axis
                                         plot.title = element_text(size=21),legend.box = "vertical") +
        labs(x = "LGA", y = "Total Crash") +
        ggtitle("Total Crash vs Total Volume in different LGA")
      
      if (input$display_rel == "Yes") {
        if (input$display_which == "Crash") { # display crash value
          gg1 <- gg1 +
            geom_text(aes(y = total_crash, label = total_crash), nudge_y = 100, check_overlap = T)
        } else { # display volume value
          gg1 <- gg1 + 
            geom_text(aes(y = total_crash, label = total_volume), nudge_y = 100, check_overlap = T)
        }
      }
    }
    else { # if sort then adding reorder() in first line
      gg1 <- ggplot(data = active_data, aes(reorder(lga, total_crash, sum), total_crash)) + # sort by reorder total crash value
        geom_bar(aes(y = total_crash, fill = "Total Crash"), stat="identity") + # adding bar chart
        geom_path(aes(y = total_volume / scale_range, group = 1, color = "Volume"), size = 1.5) + # adding line chart with value normalized by scale value before
        scale_color_manual(" ", values = c("Volume" = "#FF873A")) +  # adding line chart with value normalized by scale value before
        scale_fill_manual("", values = "#3ACAFF") + # add color for bar chart
        scale_y_continuous(sec.axis = sec_axis(~.*scale_range, name = "Volume")) +  # add second y-axis
        theme(axis.text.x = element_text(angle = 90), # rotate the x axis
                                    plot.title = element_text(size=21),
              legend.box = "vertical") +
        labs(x = "LGA", y = "Total Crash") +
        ggtitle("Total Crash vs Total Volume in different LGA")
      if (input$display_rel == "Yes") {
        if (input$display_which == "Crash") {  # display crash value
          gg1 <- gg1 +
            geom_text(aes(y = total_crash, label = total_crash), nudge_y = 100, check_overlap = T)
        } else { # display volume value
          gg1 <- gg1 +
            geom_text(aes(y = total_crash, label = total_volume), nudge_y = 100, check_overlap = T)

        }
      }
    }
    
    gg1
  })
}

shinyApp(ui, server)