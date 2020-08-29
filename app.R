#
# HDAT 9800 Group Assessment — Health Explorer
#
# Group member task 1: Thu Vuong (z3390621)
# Group member task 2: Collin Sakal
# Group member task 3: Murray Keogh
#

library(shiny)
library(rgdal)
library(sp)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)

#TASK 1 Data Pre-Processing

# import hospitals data file
hospitals <- read.csv("data/hospitals.csv", na.strings = c("", "NA"), fileEncoding="UTF-8-BOM")

# recoding the labels for State variable to match with task requirement
hospitals <- hospitals %>% 
  mutate(State=recode(State, 
                      "ACT" ="Australian Capital Territory",
                      "NSW" ="New South Wales",
                      "NT" ="Northern Territory",
                      "Qld" ="Queensland",
                      "QLD" ="Queensland",
                      "SA" ="South Australia",
                      "Tas" ="Tasmania",
                      "Vic" ="Victoria",
                      "WA" ="Western Australia"
  )) %>% 
  # replacing the empty values of Beds variable with "Other" to match with task requirement
  mutate(Beds = replace(Beds, is.na(Beds),"Other"
  ))

# creating a list of HospitalIcons
HospitalIcons <- awesomeIconList(
  Public = makeAwesomeIcon(icon = 'hospital-o', markerColor = 'green', iconColor = 'white', library = "fa"),
  Private = makeAwesomeIcon(icon = 'hospital-o', markerColor = 'orange', iconColor = 'white', library = "fa")
)

# # Setting the latitude and longitude of Australia with a proper zoom

mean_LAT <- mean(hospitals$Latitude)
mean_LONG <- mean(hospitals$Longitude)
set_zoom = 4


# TASK 2 Data Pre-Processing
# Note data already imported from task 1

# Determining which hospitals have an emergency department
# Creating data set containing only hospitals with emergency departments
hospitals$ED_yes_no <- grepl("with an emergency department", hospitals$Description)
EDs <- hospitals %>% filter(ED_yes_no == TRUE) 


#TASK 3 Data Pre-Processing

#import the data

hosp_los <- read.csv("data/myhospitals-average-length-of-stay-data.csv",fileEncoding="UTF-8-BOM")

#remove columns that are not needed and cast correct data types to columns


hosp_los <- hosp_los %>% 
  
  #remove columns that are not needed
  select(-c('X','X.1','X.2','X.3','X.4','X.5','Total.number.of.stays',
            'Total.overnight.patient.bed.days','Percentage.of.overnight.stays'))%>%
  
  #cast correct data types to columns
  mutate(Peer.group = as.factor(Peer.group),
         Category = as.factor(Category),
         Time.period = as.ordered(Time.period),
         Average.length.of.stay..days. = as.numeric(Average.length.of.stay..days.),
         Number.of.overnight.stays = as.numeric(Number.of.overnight.stays),
         Peer.group.average..days. = as.numeric(Peer.group.average..days.))



# Define UI for application
ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("Task 1 — Hospitals",  p(),
             
             sidebarLayout(
               sidebarPanel(
                 # input for state
                 selectInput(inputId = "Task1.state",
                             label = "State",
                             choices = c("All","New South Wales", "Victoria",
                                         "Queensland", "South Australia", "Tasmania",
                                         "Western Australia","Northern Territory",
                                         "Australian Capital Territory"),
                             selected = "Victoria"),
                 #input for sector
                 selectInput(inputId = "Task1.sector",
                             label = "Sector",
                             choices = c("All", "Public", "Private"),
                             selected = "All"),
                 #input for beds
                 selectInput(inputId = "Task1.beds",
                             label = "Numbers of beds",
                             choices = c("Any", ">500", "200-500", "100-199",
                                         "50-99", "<50","Other"),
                             selected = "100-199")
               ),
               
               mainPanel(
                 #insert the leaflet
                 leafletOutput("Task1_leaflet")
                 
               )
             )
             
    ),
    tabPanel("Task 2 — Distance from Emergency Department", p(),
             
             sidebarLayout(
               sidebarPanel(
                 #input for state
                 selectInput(inputId = "Task2.state", label = "Choose a State:",
                             choices = c("New South Wales",
                                         "Victoria",
                                         "Queensland",
                                         "South Australia",
                                         "Tasmania",
                                         "Western Australia",
                                         "Northern Territory",
                                         "Australian Capital Territory",
                                         "Other Territories"),
                             selected = "Victoria"),
                 #input for max distance
                 numericInput(inputId = "Task2.maxdistance", label = "Maximum distance (in km):",
                              min = 0, max = 1000, value = 150),
                 #input for grid points
                 numericInput(inputId = "Task2.npoints", label = "Number of grid points:",
                              min = 0, max = 10000, value = 10000),
                 #input for show markers
                 checkboxInput("Task2.show.markers", label = "Show Markers", value = FALSE),
                 #update button
                 actionButton(inputId = "update", label = "Update")
                 
                 
                 
               ),
               
               mainPanel(
                 #insert the leaflet
                 leafletOutput("Task2_leaflet")
               )
             )
             
    ),
    tabPanel("Task 3 — Length of Hospital Stay", p(),
             
             sidebarLayout(
               sidebarPanel(
                 
                 #input for Peer Group
                 selectInput(inputId = "Task3.peer_group", label = "Peer group",
                             choices = levels(hosp_los$Peer.group),
                             selected = "Medium hospitals"),
                 #input for Category
                 selectInput(inputId = "Task3.category", label = "Category",
                             choices = levels(hosp_los$Category),
                             selected = "Knee replacement")),
               
               mainPanel(
                 #insert the plot
                 plotOutput("ALOS_plot")
               )
             )
             
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Task 1 slot render functions
  
  
  output$Task1_leaflet = renderLeaflet({
    
    # If 'All' are selected, then consider all States. Otherwise, consider only the state selected
    if(input$Task1.state == "All") {
      states <- c("New South Wales", "Victoria",
                  "Queensland", "South Australia", "Tasmania",
                  "Western Australia","Northern Territory",
                  "Australian Capital Territory")
    } else {
      states <- input$Task1.state
    }
    # If 'All' are selected, then consider all Sectors. Otherwise, consider only the sector selected
    if(input$Task1.sector == "All") {
      sector <- c("Public", "Private")
    } else {
      sector <- input$Task1.sector
    }
    # If 'Any' are selected, then consider all types of beds. Otherwise, consider only the beds selected
    if (input$Task1.beds == "Any"){
      beds <- c(">500", "200-500", "100-199",
                "50-99", "<50","Other")
    } else {
      beds <- input$Task1.beds
    }
    
    # Subseting dataset according to the selection of the inputs
    df1=subset(hospitals, hospitals$State %in% states &
                 hospitals$Beds %in% beds &
                 hospitals$Sector %in% sector)
    
    # Name, contact details and website of the hospitals if present in the data file
    
    label_hover <- paste("Name: ",df1$Hospital.name)
    
    label_click <- paste("Name: ",df1$Hospital.name,"<br>",
                         "Website: ",df1$Website, "<br>",
                         "Phone: ",df1$Phone.number, "<br>", 
                         "Description: ",df1$Description)
    
    
    # If there's no data available, then plot Australian map with a proper message
    if(nrow(df1) == 0){
      df2 <- data.frame(lng=mean_LONG, lat=mean_LAT, label="No data")
      map1<-leaflet(df2) %>% setView(lng=mean_LONG, lat=mean_LAT, zoom = set_zoom) %>%
        addTiles() %>% addLabelOnlyMarkers(lat=~lat, lng=~lng, 
                                           label=~as.character("No data available for these selections"), 
                                           labelOptions = labelOptions(noHide = T, direction = 'top',
                                                                       textOnly = T, textsize = "30px"))
      map1 # Plot Australian map with message
    }
    
    
    # Otherwise, plot the hospitals available in the selection made by the user (using subset data).
    else ({
      
      map2 <- leaflet (df1)  %>%
        addTiles() %>%
        addAwesomeMarkers(lng=df1$Longitude, lat=df1$Latitude , icon= ~HospitalIcons[Sector],label =~label_hover,popup=~label_click)
      map2
    })
  })
  
  
  
  
  
  # Task 2 slot render functions
  
  # Reading in the states
  states <- readOGR("./data/1270055001_ste_2016_aust_shape", "STE_2016_AUST")
  states <- rgeos::gSimplify(states, tol = 0.01)
  
  # Creating icons
  ED_icons <- makeAwesomeIcon(icon = "hospital-o", markerColor = "red", iconColor = "white", library = "fa")
  
  # Defining reactive functions
  
  # Convert state selection to integer for polygon indexing
  state_poly <- eventReactive(input$update, {
    if (input$Task2.state == "New South Wales") {
      states[1]
    } else if (input$Task2.state == "Victoria") {
      states[2]
    } else if (input$Task2.state == "Queensland") {
      states[3]
    } else if (input$Task2.state == "South Australia"){
      states[4]
    } else if (input$Task2.state == "Western Australia"){
      states[5]
    } else if (input$Task2.state == "Tasmania"){
      states[6]
    } else if (input$Task2.state == "Northern Territory") {
      states[7]
    } else if (input$Task2.state == "Australian Capital Territory"){
      states[8]
    } else if (input$Task2.state == "Other Territories"){
      states[9]
    } 
  })
  
  # Creating eventReactive functions for all user input. Invalidated when update button pressed
  task2_state <- eventReactive(input$update, {input$Task2.state})
  task2_markers <- eventReactive(input$update,{input$Task2.show.markers})
  task2_maxd <- eventReactive(input$update, {input$Task2.maxdistance})
  task2_npoints <- eventReactive(input$update, {input$Task2.npoints})
  
  # Base Graph
  output$Task2_leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = mean(hospitals$Latitude), lng = mean(hospitals$Longitude), zoom = 3)
  })
  
  
  # Observe Event for Markers
  observeEvent(input$update, {
    
    # Create state subset based on user input
    EDs_state_subset <- EDs %>%
      filter(State == task2_state())
    
    # Hover labels for EDs in selected state
    label_hover <- paste("Name: ", EDs_state_subset$Hospital.name)
    
    # ED info for EDs in selected state
    label_click <- paste("Name: ", EDs_state_subset$Hospital.name,"<br>",
                         "Website: ", EDs_state_subset$Website, "<br>",
                         "Phone: ", EDs_state_subset$Phone.number, "<br>", 
                         "Description: ", EDs_state_subset$Description)
    
    # Displaying markers when show markers selected 
    if (task2_markers() == TRUE){
      leafletProxy("Task2_leaflet", data = states) %>%
        clearShapes() %>%
        addPolygons(data=state_poly(),fill = FALSE, weight = 2) %>%
        setView(lat = mean(EDs_state_subset$Latitude), lng = mean(EDs_state_subset$Longitude), zoom = 4) %>%
        clearMarkers() %>%
        addAwesomeMarkers(data = EDs_state_subset, lat =~Latitude, lng =~Longitude, label =~label_hover,popup=~label_click, icon = ~ED_icons)}
    
    # Clearing markers when show markers is not selected (and update button pressed)
    if (task2_markers() == FALSE) {
      leafletProxy("Task2_leaflet", data = states) %>%
        clearShapes() %>%
        addPolygons(data=state_poly(),fill = FALSE, weight = 2) %>%
        setView(lat = mean(EDs_state_subset$Latitude), lng = mean(EDs_state_subset$Longitude), zoom = 4) %>%
        clearMarkers()
    }
    
    
  })
  
  #Observe EventM for Heat Map
  
  observeEvent(input$update, {
    EDs_state_subset <- EDs %>%
      filter(State == task2_state())
   
    #Calculations for heatmap 
    #color
    dist_pal <- colorNumeric(c("red","orange","yellow","white"), c(0,task2_maxd()))
    
    #grid
    grid <- reactive({
      # make a grid spanning the state polygon
      makegrid(state_poly(), n = task2_npoints())
    })
    
    #grid resolution
    gridresolution <- reactive({
      # take the grid and return the distance between the first two points
      g <- grid()
      sp::spDists(data.matrix(g[1,]), data.matrix(g[2,]))
    })
    
    #calculate grid points
    grid.points <- sp::SpatialPointsDataFrame(grid(), 
                                              data.frame(n=1:nrow(grid())), 
                                              proj4string = CRS(proj4string(state_poly())))
    grid.points <- grid.points[state_poly(), ]
    
    # Matching grid projection to polygon
    coordinates(EDs_state_subset) <- c("Longitude", "Latitude")
    proj4string(EDs_state_subset) <- CRS(proj4string(state_poly()))
    
    # Intializing distance matrix
    distance.matrix <- sp::spDists(grid.points, EDs_state_subset)
    grid.points$ed.distance = apply(distance.matrix, 1, min)
    
    grid.points$ed.distance <- ifelse(grid.points$ed.distance>task2_maxd(),task2_maxd(),grid.points$ed.distance)
    
    grid.squares <- rgeos::gBuffer(grid.points, 
                                   width = gridresolution() / 2, 
                                   quadsegs = 1, 
                                   capStyle = "SQUARE", 
                                   byid = TRUE)
    
    # Displaying map with heat grid
    leafletProxy("Task2_leaflet", data = states) %>%
      setView(lat = mean(EDs_state_subset$Latitude), lng = mean(EDs_state_subset$Longitude), zoom = 4) %>%
      addPolygons(data=state_poly(),fill = FALSE, weight = 2) %>%
      addPolygons(data=grid.squares,color= dist_pal(grid.squares$ed.distance))
  })
  
  # Task 3 Render Functions
  
  # Subset input data into new dataframe
  hosp_los_subset <- reactive({
    filter(hosp_los,Peer.group == input$Task3.peer_group & Category== input$Task3.category) 
  })
  
  output$ALOS_plot <- renderPlot({
    
    # Render the plot
    ggplot(hosp_los_subset(), aes(x=as.factor(Time.period), y=Average.length.of.stay..days.)) + 
      #plot points and size
      geom_point(aes(size = Number.of.overnight.stays,colour=Number.of.overnight.stays),alpha=.75,na.rm=TRUE) + 
      #add lines for peer group average
      geom_segment(aes(x=as.integer(Time.period)-.25,xend=as.integer(Time.period)+.25,
                       y = Peer.group.average..days.,yend = Peer.group.average..days.),size=.75,na.rm=TRUE) +
      #set scale for size
      scale_size_continuous(limits=c(0,500),breaks=seq(0,500,by=100),name ="Number of Overnight Stays") +
      #set scale for colour
      scale_colour_continuous(limits=c(0,500),breaks=seq(0,500,by=100),name ="Number of Overnight Stays") +
      #combine legends into single legend
      guides(colour=guide_legend(), size = guide_legend()) +
      #set x label
      xlab("Time Period") + 
      #set y label
      ylab("Average Length of Stay (Days)") +
      #set y limits
      ylim(1,6)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

