library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinymanager)
library(readxl)
library(leaflet)
library(sf)
library(readr)
library(dplyr)
library(shinyjs)
library(rgdal)
library(htmltools)
library(plyr)
library(purrr)
library(devtools)
library(leaflegend)
library(shinyWidgets)
library(leaflet.extras2)
library(htmlwidgets)
library(mapview)
library(mapedit)
library(leaflet.extras)
library(DT)
library(shinyTree)
library(stringr)
library(shinydashboardPlus)
library(shinyscreenshot)




credentials <- data.frame(
  user = c("NIF", "immap"), # mandatory
  password = c("azerty", "12345"), # mandatory
  stringsAsFactors = FALSE
)



css <- HTML(".panel{
              background-color: #ffffff3b
            }
            #auth-shinymanager-auth-head{
            display:none
            }
            .panel-auth{
            overflow-y:hidden
            }
            .panel-auth{
            background: url(child3a.png)no-repeat center fixed !important; 
              -webkit-background-size: cover;
              -moz-background-size: cover;
              -o-background-size: cover;
              background-size: cover;

            }
            "
)


lab <- st_read("shapefiles/LGAs/label.shp")

ad <- st_read("shapefiles/admin/lga.shp")

admin1 <- st_read("shapefiles/admin/state1.shp")

admin3 <- st_read("shapefiles/admin/ward.shp")
ngo <- st_read("shapefiles/partners/ingos.shp")

cholera <- st_read("shapefiles/Cholera/col.shp")

idplga <- st_read("shapefiles/idpp/idplga.shp")
idpstate <- st_read("shapefiles/idpp/idpstate.shp")


# Accessibility town
access_point <- st_read("shapefiles/Accessibility Points/All_new_points.shp")
# Accessible town
access_a <- st_read("shapefiles/Accessibility Points/All_new_points.shp") %>% filter( Status == "Accessible Town")
# inaccessible town
access_in <- st_read("shapefiles/Accessibility Points/All_new_points.shp") %>% filter( Status == "Inaccessible Town")
# hard to reach town
access_h <- st_read("shapefiles/Accessibility Points/All_new_points.shp") %>% filter( Status == "Hard to Reach Town")
# Accessibility town
access_pointunas <- st_read("shapefiles/Accessibility Points/All_new_points.shp")%>% filter( Status == "Hard to reach town - UNHAS only")
# Camps
idp <- st_read("shapefiles/camp/camp1.shp")

idpF <- st_read("shapefiles/camp/camp1.shp")%>% filter( Status == "Formal site")
idpI <- st_read("shapefiles/camp/camp1.shp")%>% filter( Status == "Informal site")


# Accessibility road access
roadsstatusRa <- st_read("shapefiles/Accessibility Roads/All_new_roads.shp") %>% filter( type == "Accessible road")




roadsstatusRac <- st_read("shapefiles/Accessibility Roads/All_new_roads.shp") %>% filter( type == "Cargo Movement Only")
# Transform coordinate system
roadsstatusR1 <- st_transform(roadsstatusRa, CRS("+init=epsg:4326"))
roadsstatusR1ac <- st_transform(roadsstatusRac, CRS("+init=epsg:4326"))
# Accessibility road inaccessible
roadsstatusRI <- st_read("shapefiles/Accessibility Roads/All_new_roads.shp") %>% filter( type == "Extremely Hard-to-reach road")
# Transform coordinate system
roadsstatusR2 <- st_transform(roadsstatusRI, CRS("+init=epsg:4326"))
# Accessibility road hard
roadsstatusRh <- st_read("shapefiles/Accessibility Roads/All_new_roads.shp")%>% filter( type == "Hard-to-reach road")
# Transform coordinate system
roadsstatusR3 <- st_transform(roadsstatusRh, CRS("+init=epsg:4326"))
# Accessibility town
access_point <- st_read("shapefiles/Accessibility Points/All_new_points.shp")
accessi_point <- access_point %>%
  mutate(lat = unlist(map(access_point$geometry,2)),
         long = unlist(map(access_point$geometry,1)))

accessible <- st_read("shapefiles/Accessibility Polygon/accessible.shp")
hard <- st_read("shapefiles/Accessibility Polygon/hard.shp")

# Relocation
relocations <- st_read("shapefiles/Relocation/relo1.shp")

relocationsA <- st_read("shapefiles/Relocation/relo1.shp")%>% filter( Status == "Announced for Relocation")
relocationsT <- st_read("shapefiles/Relocation/relo1.shp")%>% filter( Status == "Relocation Took Place")

# Flood risks areas
floods <- st_read("shapefiles/Floods/BAY_Flood_Shapefile.shp")
# Flood affected pop
pop_flood <- st_read("shapefiles/Floods/pop_flood/Flooded_LGAs_BAY.shp")
# Accessibility road
roadsstatus <- st_read("shapefiles/Accessibility Roads/All_new_roads.shp")
# Transform coordinate system
roadsstatus2 <- st_transform(roadsstatus, CRS("+init=epsg:4326"))
# Partners presence
partners <- read_csv("shapefiles/partners/partners.csv")
inso <- read_csv("shapefiles/inso/insodata.csv")
state <- read_csv("shapefiles/idpp/state.csv")
lga <- read_csv("shapefiles/idpp/lga.csv")
flood <- read_csv("shapefiles/Floods/floodpop.csv")
# IPC 
proj_acute_poly <- st_read("shapefiles/Projected Acute Food Insecurity/Projected_Acute_Mal.shp")

food <-  st_read("shapefiles/food/ipc.shp")

severity_poly <- st_read("shapefiles/Severity of Needs/Ocha_Severity_Map1.shp")
# people in need
PIN_poly <- st_read("shapefiles/PIN/PIN.shp")
access_poly <- rgdal::readOGR(
  dsn   = "shapefiles/accessibility Polygon/inaccessible.shp", 
  layer = "inaccessible", 
  GDAL1_integer64_policy = T
)
access_ <- rgdal::readOGR(
  dsn   = "shapefiles/Accessibility Polygon/accessible.shp", 
  layer = "accessible", 
  GDAL1_integer64_policy = T
)
access_p <- rgdal::readOGR(
  dsn   = "shapefiles/Accessibility Polygon/hard.shp", 
  layer = "hard", 
  GDAL1_integer64_policy = T
)
# Hatching
access.hatch <- HatchedPolygons::hatched.SpatialPolygons(access_poly, density = 40, angle = 135)
access <- HatchedPolygons::hatched.SpatialPolygons(access_, density = 40, angle = 135)
inaccess <- HatchedPolygons::hatched.SpatialPolygons(access_p, density = 40, angle = 135)
# Color palette labels
val <- c('Areas announced for relocation', 'Areas where relocation took place')
val1 <- c('Formal site', 'Informal site')
val2 <- c('Accessible town', 'Hard-to-reach town', "Extremely Hard-to-reach town", "Hard-to-reach town UNHAS Only")
leafImg <- c("img/leaf-Relocation Took Place.png", "img/leaf-Announced for Relocation.png" )
leafImg1 <- c("img/formal.png", "img/informal.png" )
leafImg2 <- c("img/accesble.png", "img/hard.png", "img/inace.png","img/helipad_neww.png")
Img1 <- c("img/state.png", "img/lga.png","img/ward.png")

Img2 <- c("img/ac.png", "img/h.png","img/e.png")
#Color Palette
roadpal <- colorFactor(c("seagreen", "#58280F","red","orange"), 
                       domain =roadsstatus2$type)
pal <- colorFactor(
  palette = c("lightgreen", "lightsalmon", "ghostwhite"),
  domain = access_poly$Types)

sev_pal <- colorFactor(
  palette = c( "#C8D7EE","#9AB9E0", "#4E90CD", "#0873BB", "#0E5D9D","gray"),
  domain = severity_poly$serv)




pop_pal <- colorNumeric(
  palette = c("gray", "#F5B7B1","#F1948A", "#EC7063","#E74C3C" ),
  domain = pop_flood$Total_in_D)

part_pal <- colorNumeric(
  palette = "Blues",
  domain = partners$partners_2)




prj_acute_pal <- colorFactor(
  palette = c( "#D6E7CA", "#F6E818", "gray", "#C82027", "#E67925"),
  domain = proj_acute_poly$pro)

prj_acute_p <- colorFactor(
  palette = c(  "gray","#D6E7CA", "#F6E818", "#E67925", "#C82027"),
  domain = proj_acute_poly$curr)


prj_acute_pall <- colorFactor(
  palette = c( "gray", "#F6E818",  "#E67925", "#C82027"),
  domain = proj_acute_poly$la)


prj_acute_pallll <- colorFactor(
  palette = c( "#E67925", "#C82027", "#C9F7DE", "#F6E818"),
  domain = food$cu)

prj_acute_pallllz <- colorFactor(
  palette = c( "#C9F7DE", "#F6E818","#E67925", "#C82027" ),
  domain = food$label)

prj_acute_pallll1 <- colorFactor(
  palette = c( "#E7781D", "#C82027", "#F6E818"),
  domain = food$ch)

prj_acute_pallll1z <- colorFactor(
  palette = c("#F6E818", "#E7781D", "#C82027"),
  domain = food$label2)

prj_acute_pa<- colorFactor(
  palette = c( "#F6E818","gray.",  "#E67925"),
  domain = proj_acute_poly$pro)
par_icon <- makeIcon(
  iconUrl = "https://gdurl.com/HXa6",
  iconWidth = 8, iconHeight = 8,
)
leafIconss <- icons(
  iconUrl = ifelse(relocations$Status == "Relocation Took Place",
                   "https://gdurl.com/meaC",
                   "https://gdurl.com/zPpr"
  ),
  iconWidth = 30, iconHeight = 30,
)

iconn <- icons(
  iconUrl = ifelse(idp$Status == "formal",
                   "https://gdurl.com/xCYB",
                   "https://gdurl.com/8p32"
  ),
  iconWidth = 10, iconHeight = 10,
)



html_legend <- "<img src='https://gdurl.com/XuZD'>"

html_legens <- "<img src='https://gdurl.com/X2YC'>"

html_legendp <- "<img src='https://gdurl.com/kOb2'>"

html_legendf <- "<img src='https://gdurl.com/PHb0'>"

html_legendc <- "<img src='https://gdurl.com/YDLq'>"
html_legendse <- "<img src='https://gdurl.com/vLe0g'>"
# Define UI
ui <- fluidPage(htmlTemplate("www/index.html"),
                
              
                
                
                
                
                #Reset Button
                actionBttn(
                  inputId = "reset",
                  label = "Reset Map",
                  value = 1,
                  style = "bordered",
                  color = "danger",
                  icon = icon("sliders")
                ),
                #Screenshot
                actionBttn(
                  inputId = "print",
                  label = "Print Map",
                  style = "unite",
                  color = "primary",
                  icon = icon("camera")
                ),
                
              
              
        
                
                
                
                # Idpss here ................................................................................
                
                awesomeCheckbox(
                  inputId = "no",
                  label = "Population of IDP by LGA", 
                  value = FALSE,
                  status = "danger",
                ),
                awesomeCheckbox(
                  inputId = "no1",
                  label = "Population of IDP by state", 
                  value = FALSE,
                  status = "danger",
                ),
                
                
                # ............................................................................................
                
                # INSO here ..................................................................................
                
                awesomeCheckbox(
                  inputId = "inso",
                  label = "Conflict related incidents in 2022", 
                  value = FALSE,
                  status = "danger",
                ),
                
            
                # ............................................................................................
                
                # Cholera here ...............................................................................
                
                awesomeCheckbox(
                  inputId = "col",
                  label = "Cholera cases and deaths", 
                  value = FALSE,
                  status = "danger",
                ),
                
                
                # ............................................................................................
                
                # Accessibility Area here ....................................................................
                
                awesomeCheckbox(
                  inputId = "accessibleareas",
                  label = "Accessible Areas", 
                  value = FALSE,
                  status = "danger",
                ),
                
                
                awesomeCheckbox(
                  inputId = "exhardtoreach",
                  label = "Extremely Hard-to-Reach Areas", 
                  value = FALSE,
                  status = "danger"
                ),
                
                awesomeCheckbox(
                  inputId = "hardtoreach",
                  label = "Hard-to-Reach Areas", 
                  value = FALSE,
                  status = "danger"
                ),
                # ............................................................................................
                
                # Accessibility Town here ....................................................................
                
                awesomeCheckbox(
                  inputId = "accessibleT",
                  label = "Accessible towns", 
                  value = FALSE,
                  status = "danger",
                ),
                
                
                awesomeCheckbox(
                  inputId = "exhardtoreachT",
                  label = "Hard-to-Reach towns", 
                  value = FALSE,
                  status = "danger"
                ),
                
                
                
                awesomeCheckbox(
                  inputId = "hardtoreachT",
                  label = "Extremely Hard-to-Reach towns", 
                  value = FALSE,
                  status = "danger"
                ),
                # ............................................................................................
                
                
                # Accessibility roads here ...................................................................
                
                awesomeCheckbox(
                  inputId = "accessibleR",
                  label = "Accessible roads", 
                  value = FALSE,
                  status = "danger",
                ),
                
                
                awesomeCheckbox(
                  inputId = "hardtoreachR",
                  label = "Hard-to-Reach roads", 
                  value = FALSE,
                  status = "danger"
                ),
                
                awesomeCheckbox(
                  inputId = "CAhardtoreachR",
                  label = "Accessible for cargo movement only", 
                  value = FALSE,
                  status = "danger"
                ),
                
                awesomeCheckbox(
                  inputId = "exhardtoreachR",
                  label = "Extremely Hard-to-Reach roads", 
                  value = FALSE,
                  status = "danger"
                ),
                # ............................................................................................
                
                
                # Relocations here ...........................................................................
                
                awesomeCheckbox(
                  inputId = "took",
                  label = "Areas where relocations took place", 
                  value = FALSE,
                  status = "danger",
                ),
                
                
                awesomeCheckbox(
                  inputId = "ana",
                  label = "Areas announced for relocation", 
                  value = FALSE,
                  status = "danger"
                ),
                
                
                # ............................................................................................
                
                # IDPs here ..................................................................................
                
                awesomeCheckbox(
                  inputId = "idp1",
                  label = "Formal sites", 
                  value = FALSE,
                  status = "danger",
                ),
                
                
                awesomeCheckbox(
                  inputId = "idp2",
                  label = "Informal sites", 
                  value = FALSE,
                  status = "danger"
                ),
                
                
                # ............................................................................................
                # HNO here ...................................................................................
                
                awesomeCheckbox(
                  inputId = "HNO1",
                  label = "Severity of needs", 
                  value = FALSE,
                  status = "danger",
                ),
                
                
                awesomeCheckbox(
                  inputId = "HNO2",
                  label = "People in need", 
                  value = FALSE,
                  status = "danger"
                ),
                
                
                # ............................................................................................
                
                # Nutrition here .............................................................................
                
                awesomeCheckbox(
                  inputId = "Cu",
                  label = "Current situation Oct - Dec 2022", 
                  value = FALSE,
                  status = "danger",
                ),
                
                
                awesomeCheckbox(
                  inputId = "Pro",
                  label = "Projected situation Jan - April 2023", 
                  value = FALSE,
                  status = "danger"
                ),
                
                
                # ............................................................................................
                
                # FOOD here ..................................................................................
                
                awesomeCheckbox(
                  inputId = "food",
                  label = "Current situation Oct - Dec 2022", 
                  value = FALSE,
                  status = "danger",
                ),
                
                
                awesomeCheckbox(
                  inputId = "food1",
                  label = "Projected situation June - Aug 2023", 
                  value = FALSE,
                  status = "danger"
                ),
                
                
                # ............................................................................................
                
                # FLOOD here .................................................................................
                
                awesomeCheckbox(
                  inputId = "flood",
                  label = "Flood risk areas (source: IJAGR 2022)", 
                  value = FALSE,
                  status = "danger",
                ),
                
                
                awesomeCheckbox(
                  inputId = "flood1",
                  label = "Impacted Population (WFP 2022)", 
                  value = FALSE,
                  status = "danger"
                ),
                
                
                # ............................................................................................
                
                # admin here .................................................................................
                
                awesomeCheckbox(
                  inputId = "State",
                  label = "State boundaries", 
                  value = TRUE,
                  status = "danger",
                ),
                
                
                awesomeCheckbox(
                  inputId = "LGA",
                  label = "LGA /INGO presence", 
                  value = FALSE,
                  status = "danger"
                ),
                awesomeCheckbox(
                  inputId = "LG",
                  label = "LGA Labels", 
                  value = FALSE,
                  status = "danger"
                ),
                awesomeCheckbox(
                  inputId = "Ward",
                  label = "Ward boundaries", 
                  value = FALSE,
                  status = "danger"
                ),
                
                
                # ............................................................................................
                
                
                
                
                
                
                
                leafletOutput("mymap"),
)

#UI responsible for showing the login page
# ui <- secure_app( ui = htmlTemplate("www/index.html"), 
                  
                  
#                   theme = shinythemes::shinytheme("flatly"),
#                   background  = "linear-gradient(#00203b, #00203b)",
#                   tags_top = 
#                     tags$div(
#                       tags$img(
#                         src = "assets/img/niff.png", width = 150, height = 60
#                       )
#                     ),
#                   # add information on bottom ?
#                   tags_bottom = tags$div(
#                     tags$p(
#                       "For any question, please  contact ",
#                       tags$a(
#                         href = "mailto:damos@immap.com?Subject=Shiny%20aManager",
#                         target="_top", "administrator"
#                       )
#                     )
#                   )
# )




# Server logic
server <- function(session = session, input, output) {
  
  
  
  # Authenticate
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(credentials)
  # )
  
  output$mymap <- renderLeaflet({
    
    
    leaflet(options = leafletOptions(zoomControl = FALSE,  minZoom = 7, maxZoom = 18) )%>%
      
      setView(lng = 12.195, lat = 10.611, zoom = 1) %>% 
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles("CartoDB.Positron", group = 'Default')%>%
      # Reset map to default setting
      leaflet.extras::addResetMapButton() %>% 
      # Add an inset minimap
      addMiniMap(
        position = "topright",
        tiles = providers$OpenStreetMap,
        toggleDisplay = TRUE,
        minimized = FALSE
      ) %>%
      # Add measurement tool
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters"
      ) %>%
      # Add scale bar
      addScaleBar(
        position = "topright",
        options = scaleBarOptions(imperial = FALSE)
      ) %>% 
      addDrawToolbar(
        targetGroup = "draw",
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()
        )
      ) %>% 
      addStyleEditor(position = "topleft", 
                     openOnLeafletDraw = TRUE) %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
      
      
      # Layering
      addLayersControl(
        baseGroups = c("Carto DB", "OpenStreetMap", "World Imagery"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
      htmlwidgets::onRender("
        function() {
             $('.leaflet-control-layers-list').prepend('<label style=\"text-align:left\">Basemap Selection</label>');
        }
    ")
    
    
  })
  
  
  #Reset Button
  
  
  
  observeEvent(input$reset, {
    
    
    if(TRUE %in% input$Ward){
      updateAwesomeCheckbox(session = session, inputId = "Ward", value = FALSE)
      
    }
    
    if(TRUE %in% input$LG){
      updateAwesomeCheckbox(session = session, inputId = "LG", value = FALSE)
      
    }
    
    if(TRUE %in% input$LGA){
      updateAwesomeCheckbox(session = session, inputId = "LGA", value = FALSE)
      
    }
    
    
    if(TRUE %in% input$State){
      updateAwesomeCheckbox(session = session, inputId = "State", value = FALSE)
      
    }
    
    if(TRUE %in% input$flood1){
      updateAwesomeCheckbox(session = session, inputId = "flood1", value = FALSE)
      
    }
    if(TRUE %in% input$flood){
      updateAwesomeCheckbox(session = session, inputId = "flood", value = FALSE)
      
    }
    
    if(TRUE %in% input$Pro){
      updateAwesomeCheckbox(session = session, inputId = "Pro", value = FALSE)
      
    }
    if(TRUE %in% input$Cu){
      updateAwesomeCheckbox(session = session, inputId = "Cu", value = FALSE)
      
    }
    
    
    if(TRUE %in% input$LGA){
      updateAwesomeCheckbox(session = session, inputId = "LGA", value = FALSE)
      
    }
    
    if(TRUE %in% input$HNO1){
      updateAwesomeCheckbox(session = session, inputId = "HNO1", value = FALSE)
      
    }
    
    if(TRUE %in% input$HNO2){
      updateAwesomeCheckbox(session = session, inputId = "HNO2", value = FALSE)
      
    }
    
    if(TRUE %in% input$idp1){
      updateAwesomeCheckbox(session = session, inputId = "idp1", value = FALSE)
      
    }
    if(TRUE %in% input$idp2){
      updateAwesomeCheckbox(session = session, inputId = "idp2", value = FALSE)
      
    }
    
    if(TRUE %in% input$took){
      updateAwesomeCheckbox(session = session, inputId = "took", value = FALSE)
      
    }
    
    if(TRUE %in% input$ana){
      updateAwesomeCheckbox(session = session, inputId = "ana", value = FALSE)
      
    }
    
    
    
    
    if(TRUE %in% input$exhardtoreachR){
      updateAwesomeCheckbox(session = session, inputId = "exhardtoreachR", value = FALSE)
      
    }
    
    
    if(TRUE %in% input$CAhardtoreachR){
      updateAwesomeCheckbox(session = session, inputId = "CAhardtoreachR", value = FALSE)
      
    }
    
    
    if(TRUE %in% input$hardtoreachR){
      updateAwesomeCheckbox(session = session, inputId = "hardtoreachR", value = FALSE)
      
    }
    
    
    if(TRUE %in% input$accessibleR){
      updateAwesomeCheckbox(session = session, inputId = "accessibleR", value = FALSE)
      
    }
    if(TRUE %in% input$hardtoreachT){
      updateAwesomeCheckbox(session = session, inputId = "hardtoreachT", value = FALSE)
      
    }
    if(TRUE %in% input$hardtoreachR){
      updateAwesomeCheckbox(session = session, inputId = "hardtoreachR", value = FALSE)
      
    }
    if(TRUE %in% input$exhardtoreachT){
      updateAwesomeCheckbox(session = session, inputId = "exhardtoreachT", value = FALSE)
      
    }
    if(TRUE %in% input$accessibleT){
      updateAwesomeCheckbox(session = session, inputId = "accessibleT", value = FALSE)
      
    }
    if(TRUE %in% input$exhardtoreach){
      updateAwesomeCheckbox(session = session, inputId = "exhardtoreach", value = FALSE)
      
    }
    if(TRUE %in% input$hardtoreach){
      updateAwesomeCheckbox(session = session, inputId = "hardtoreach", value = FALSE)
      
    }
    if(TRUE %in% input$accessibleareas){
      updateAwesomeCheckbox(session = session, inputId = "accessibleareas", value = FALSE)
      
    }
    
    if(TRUE %in% input$col){
      updateAwesomeCheckbox(session = session, inputId = "col", value = FALSE)
      
    }
    
    if(TRUE %in% input$inso){
      updateAwesomeCheckbox(session = session, inputId = "inso", value = FALSE)
      
    }
    
    
    if(TRUE %in% input$no1){
      updateAwesomeCheckbox(session = session, inputId = "no1", value = FALSE)
      
    }
    
    if(TRUE %in% input$no){
      updateAwesomeCheckbox(session = session, inputId = "no", value = FALSE)
      
    }
    
    if(TRUE %in% input$food1){
      updateAwesomeCheckbox(session = session, inputId = "food1", value = FALSE)
      
    }
    
    if(TRUE %in% input$food){
      updateAwesomeCheckbox(session = session, inputId = "food", value = FALSE)
      
    }
    
    
    
    if(input$reset ){
      leafletProxy("mymap") %>%
      
        addLayersControl(
          baseGroups = c("Carto DB", "Carto DB - No Labels", "OpenStreetMap", "World Imagery"),
          options = layersControlOptions(collapsed = TRUE)
        )
      
      
      
      
    }
  })
  
  
  
  
  
  #Print Map
  
  observeEvent(input$print,{
    screenshot(selector = "#mymap", filename = "NIF Interactive Map", scale = 3)
  })
  
  
  # ACCESS AREA ...................................................................................
  
  observeEvent(input$exhardtoreach, {
    if(!is.null(input$exhardtoreach )) {
      leafletProxy("mymap") %>% clearGroup(group = "A") %>% removeControl("A")
      
      if(TRUE %in% input$exhardtoreach){
        leafletProxy("mymap") %>% clearGroup(group = "A") %>% removeControl("A")
        
        
      }
      
      if(TRUE %in% input$exhardtoreach ){
        leafletProxy("mymap") %>% 
          addPolylines(
            data = access.hatch,
            color = "red",
            group = "A",
            weight = 1
          ) %>% 
          addLegendImage(images = Img2,
                         labels = c("Accessible area", "Hard-to-reach area", "Extremely Hard-to-reach area" ),
                         
                         width = 20,
                         height = 20,
                         layerId = "A",
                         labelStyle = "font-size: 14px; vertical-align: middle;",
                         group = "A",
                         title = 'Area Accessibility - Source: NIF/INSO MAP',
                         position = 'bottomleft')
        
        
      }
      
    }
    
  })    
  
  
  
  observeEvent(input$accessibleareas, {
    
    
    
    
    if(!is.null(input$accessibleareas)) {
      leafletProxy("mymap") %>% clearGroup(group = "B") %>% removeControl("B")
      
      if(TRUE %in% input$accessibleareas){
        leafletProxy("mymap") %>% clearGroup(group = "B")%>% removeControl("B")
        
        
      }
      
      
      
      if(TRUE %in% input$accessibleareas ){
        leafletProxy("mymap") %>%  removeControl("A")   %>% 
          addPolygons(data = access, # Accessible
                      color = "#2ca25f",
                      opacity = 0.7,
                      weight = 1,
                      group = "B",
                      highlight = highlightOptions(
                        weight = 1,
                        bringToFront = FALSE)) %>% 
          addLegendImage(images = Img2,
                         labels = c("Accessible area", "Hard-to-reach area", "Extremely Hard-to-reach area" ),
                         
                         width = 20,
                         height = 20,
                         layerId = "B",
                         labelStyle = "font-size: 14px; vertical-align: middle;",
                         group = "B",
                         title = 'Area Accessibility - Source: NIF/INSO MAP',
                         position = 'bottomleft')
        
        
        
        
      }
      
    }
    
  })    
  
  
  observeEvent(input$hardtoreach, {
    if(!is.null(input$hardtoreach)) {
      leafletProxy("mymap") %>% clearGroup(group = "C") %>% removeControl("C")
      
      if(TRUE %in% input$hardtoreach){
        leafletProxy("mymap") %>% clearGroup(group = "C")%>% removeControl("C")
        
        
      }
      
      
      
      if(TRUE %in% input$hardtoreach ){
        leafletProxy("mymap") %>%    removeControl("A")%>% removeControl("B") %>% 
          addPolygons(data = inaccess, # 
                      color = "#fec44f",
                      opacity = 0.7,
                      weight = 1,
                      group = "C",
                      highlight = highlightOptions(
                        weight = 1,
                        bringToFront = FALSE)) %>% 
          addLegendImage(images = Img2,
                         labels = c("Accessible area", "Hard-to-reach area", "Extremely Hard-to-reach area" ),
                         
                         width = 20,
                         height = 20,
                         layerId = "C",
                         labelStyle = "font-size: 14px; vertical-align: middle;",
                         group = "C",
                         title = 'Area Accessibility - Source: NIF/INSO MAP',
                         position = 'bottomleft')
        
      }
      
    }
    
  })   
  
  # .............................................................................................................
  # ACCESS TOWN ...................................................................................
  
  observeEvent(input$accessibleT, {
    
    iconn_acces_poina <- icons(
      iconUrl = ifelse( access_a$Status == "Accessible Town", "https://gdurl.com/AGyu",
                        ifelse(access_a$Status == "Hard to Reach Town", "https://gdurl.com/aeXx",
                               ifelse(access_a$Status== "Inaccessible Town", "https://gdurl.com/TJUa",
                                      ifelse(access_a$Status == "Accessible Village","https://gdurl.com/AGyu",
                                             ifelse(access_a$Status == "Hard to reach town - UNHAS only", "https://gdurl.com/w5tC",
                                                    ifelse(access_a$Status == "Inaccessible Village", "https://gdurl.com/TJUa",
                                                           ifelse(access_a$Status == "Hard to Reach Village", "https://gdurl.com/aeXx",
                                                                  ifelse(access_a$Status == "Airport","https://gdurl.com/Re1E",
                                                                         ifelse(access_a$Status == "Accessible LGA HQ", "https://gdurl.com/AGyu",
                                                                                ifelse(access_a$Status  == "Inaccessible LGA HQ", "https://gdurl.com/TJUa",
                                                                                       ifelse(access_a$Status == "Hard to Reach LGA HQ", "https://gdurl.com/aeXx"," "))))))))))
      ),
      iconWidth = 9, iconHeight = 10,
    )
    iconn_acces_poinin <- icons(
      iconUrl = ifelse( access_in$Status == "Accessible Town", "https://gdurl.com/AGyu",
                        ifelse(access_in$Status == "Hard to Reach Town", "https://gdurl.com/aeXx",
                               ifelse(access_in$Status== "Inaccessible Town", "https://gdurl.com/TJUa",
                                      ifelse(access_in$Status == "Accessible Village","https://gdurl.com/AGyu",
                                             ifelse(access_in$Status == "Hard to reach town - UNHAS only", "https://gdurl.com/w5tC",
                                                    ifelse(access_in$Status == "Inaccessible Village", "https://gdurl.com/TJUa",
                                                           ifelse(access_in$Status == "Hard to Reach Village", "https://gdurl.com/aeXx",
                                                                  ifelse(access_in$Status == "Airport","https://gdurl.com/Re1E",
                                                                         ifelse(access_in$Status == "Accessible LGA HQ", "https://gdurl.com/AGyu",
                                                                                ifelse(access_in$Status  == "Inaccessible LGA HQ", "https://gdurl.com/TJUa",
                                                                                       ifelse(access_in$Status == "Hard to Reach LGA HQ", "https://gdurl.com/aeXx"," "))))))))))
      ),
      iconWidth = 10, iconHeight = 10,
    )
    
    
    if(!is.null(input$accessibleT )) {
      leafletProxy("mymap") %>% clearGroup(group = "AT") %>% removeControl("ac")
      
      if(TRUE %in% input$accessibleT){
        leafletProxy("mymap") %>% clearGroup(group = "AT") %>% removeControl("ac")
        
        
      }
      
      
      
      if(TRUE %in% input$accessibleT ){
        leafletProxy("mymap") %>%  
          
          # Town
          addMarkers(data = access_a, access_a$lon, access_a$lat,group = "AT", label = ~paste("Location: ",access_a$Settlement,  access_a$Status), icon = iconn_acces_poina) %>% 
          addLegendImage(images = leafImg2,
                         labels = val2,
                         width = 10,
                         height = 10,
                         labelStyle = "font-size: 14px; vertical-align: middle;",
                         group = "AT",
                         title = 'Town Accessibility - Source: NIF/INSO MAP ',
                         layerId = "ac",
                         position = 'bottomleft')
        
        
      }
      
    }
    
  })    
  
  
  
  observeEvent(input$exhardtoreachT, {
    
    iconn_acces_poinh <- icons(
      iconUrl = ifelse( access_h$Status == "Accessible Town", "https://gdurl.com/AGyu",
                        ifelse(access_h$Status == "Hard to Reach Town", "https://gdurl.com/aeXx",
                               ifelse(access_h$Status== "Inaccessible Town", "https://gdurl.com/TJUa",
                                      ifelse(access_h$Status == "Accessible Village","https://gdurl.com/AGyu",
                                             ifelse(access_h$Status == "Hard to reach town - UNHAS only", "https://gdurl.com/w5tC",
                                                    ifelse(access_h$Status == "Inaccessible Village", "https://gdurl.com/TJUa",
                                                           ifelse(access_h$Status == "Hard to Reach Village", "https://gdurl.com/aeXx",
                                                                  ifelse(access_h$Status == "Airport","https://gdurl.com/Re1E",
                                                                         ifelse(access_h$Status == "Accessible LGA HQ", "https://gdurl.com/AGyu",
                                                                                ifelse(access_h$Status  == "Inaccessible LGA HQ", "https://gdurl.com/TJUa",
                                                                                       ifelse(access_h$Status == "Hard to Reach LGA HQ", "https://gdurl.com/aeXx"," "))))))))))
      ),
      iconWidth = 10, iconHeight = 10,
    )
    iconn_acces_poinhh <- icons(
      iconUrl = ifelse( access_pointunas$Status == "Accessible Town", "https://gdurl.com/AGyu",
                        ifelse(access_pointunas$Status == "Hard to Reach Town", "https://gdurl.com/aeXx",
                               ifelse(access_pointunas$Status== "Inaccessible Town", "https://gdurl.com/TJUa",
                                      ifelse(access_pointunas$Status == "Accessible Village","https://gdurl.com/AGyu",
                                             ifelse(access_pointunas$Status == "Hard to reach town - UNHAS only", "https://gdurl.com/w5tC",
                                                    ifelse(access_pointunas$Status == "Inaccessible Village", "https://gdurl.com/TJUa",
                                                           ifelse(access_pointunas$Status == "Hard to Reach Village", "https://gdurl.com/aeXx",
                                                                  ifelse(access_pointunas$Status == "Airport","https://gdurl.com/Re1E",
                                                                         ifelse(access_pointunas$Status == "Accessible LGA HQ", "https://gdurl.com/AGyu",
                                                                                ifelse(access_pointunas$Status  == "Inaccessible LGA HQ", "https://gdurl.com/TJUa",
                                                                                       ifelse(access_h$Status == "Hard to Reach LGA HQ", "https://gdurl.com/aeXx"," "))))))))))
      ),
      iconWidth = 15, iconHeight = 15,
    )
    
    if(!is.null(input$exhardtoreachT)) {
      leafletProxy("mymap") %>% clearGroup(group = "HT") %>% removeControl("acT")
      
      if(TRUE %in% input$exhardtoreachT){
        leafletProxy("mymap") %>% clearGroup(group = "HT") %>% removeControl("acT")
        
        
      }
      
      
      
      if(TRUE %in% input$exhardtoreachT ){
        leafletProxy("mymap") %>%   removeControl("ac") %>% 
          
          # Town Hard to reach 
          addMarkers(data = access_h, access_h$lon, access_h$lat,group = "HT",  label = ~paste(" Location: ",access_h$Settlement,  access_h$Status), icon = iconn_acces_poinh) %>% 
          # Town  Hard to reach 
          addMarkers(data = access_pointunas, access_pointunas$lon, access_pointunas$lat, group = "HT", label = ~paste(" Location: ",access_pointunas$Settlement," Status: ", access_pointunas$Status), icon = iconn_acces_poinhh) %>% 
          addLegendImage(images = leafImg2,
                         labels = val2,
                         width = 10,
                         height = 10,
                         labelStyle = "font-size: 14px; vertical-align: middle;",
                         group = "AT",
                         title = 'Town Accessibility - Source: NIF/INSO MAP ',
                         layerId = "acT",
                         position = 'bottomleft')
      
        
        
        
        
      }
      
    }
    
  })    
  observeEvent(input$hardtoreachT, {
    
    iconn_acces_poinin <- icons(
      iconUrl = ifelse( access_in$Status == "Accessible Town", "https://gdurl.com/AGyu",
                        ifelse(access_in$Status == "Hard to Reach Town", "https://gdurl.com/aeXx",
                               ifelse(access_in$Status== "Inaccessible Town", "https://gdurl.com/TJUa",
                                      ifelse(access_in$Status == "Accessible Village","https://gdurl.com/AGyu",
                                             ifelse(access_in$Status == "Hard to reach town - UNHAS only", "https://gdurl.com/w5tC",
                                                    ifelse(access_in$Status == "Inaccessible Village", "https://gdurl.com/TJUa",
                                                           ifelse(access_in$Status == "Hard to Reach Village", "https://gdurl.com/aeXx",
                                                                  ifelse(access_in$Status == "Airport","https://gdurl.com/Re1E",
                                                                         ifelse(access_in$Status == "Accessible LGA HQ", "https://gdurl.com/AGyu",
                                                                                ifelse(access_in$Status  == "Inaccessible LGA HQ", "https://gdurl.com/TJUa",
                                                                                       ifelse(access_in$Status == "Hard to Reach LGA HQ", "https://gdurl.com/aeXx"," "))))))))))
      ),
      iconWidth = 10, iconHeight = 10,
    )
    if(!is.null(input$hardtoreachT)) {
      leafletProxy("mymap") %>% clearGroup(group = "HTH") %>% removeControl("acTT")
      
      if(TRUE %in% input$hardtoreachT){
        leafletProxy("mymap") %>% clearGroup(group = "HTH") %>% removeControl("acTT")
        
        
      }
      
      
      
      if(TRUE %in% input$hardtoreachT ){
        leafletProxy("mymap") %>%  removeControl("ac") %>%  removeControl("acT") %>%     
          
          # Town inaccessible
          addMarkers(data = access_in, access_in$lon, access_in$lat, group = "HTH", label = ~paste("Location: ",access_in$Settlement,  access_in$Status),  icon = iconn_acces_poinin) %>% 
          addLegendImage(images = leafImg2,
                         labels = val2,
                         width = 10,
                         height = 10,
                         labelStyle = "font-size: 14px; vertical-align: middle;",
                         group = "AT",
                         title = 'Town Accessibility - Source: NIF/INSO MAP ',
                         layerId = "acTT",
                         position = 'bottomleft')
        
        
        
        
        
      }
      
    }
    
  }) 
  
  
  # ................................................................................................
  
  # ACCESS ROADS ...................................................................................
  
  observeEvent(input$accessibleR, {
    
    
    if(!is.null(input$accessibleR)) {
      leafletProxy("mymap") %>% clearGroup(group = "AR") %>% removeControl("R1")
      
      if(TRUE %in% input$accessibleR){
        leafletProxy("mymap") %>% clearGroup(group = "AR")%>% removeControl("R1")
        
        
      }
      
      
      
      if(TRUE %in% input$accessibleR ){
        leafletProxy("mymap") %>%   removeControl("R2") %>% removeControl("R3") %>%  
          # road ACCESSIBLE
          addPolylines(data = roadsstatusR1, color = "seagreen", weight = 3, group = "AR", popup = ~paste("Road Status:",roadsstatusR1$type ) ) %>% 
          addLegend(
            title = "Road Accessibility - Source: NIF/INSO MAP ",
            pal = roadpal,
            opacity = 1,
            values = roadsstatus2$type, 
            group = "AR",
            layerId = "R1",
            position = "bottomleft")
        
        
        
      }
      
    }
    
  })    
  
  
  
  observeEvent(input$hardtoreachR, {
    
    
    
    
    if(!is.null(input$hardtoreachR)) {
      leafletProxy("mymap") %>% clearGroup(group = "BR") %>% removeControl("R2")
      
      if(TRUE %in% input$hardtoreachR){
        leafletProxy("mymap") %>% clearGroup(group = "BR")%>% removeControl("R2")
        
        
      }
      
      
      
      if(TRUE %in% input$hardtoreachR ){
        leafletProxy("mymap") %>%  removeControl("R1") %>% removeControl("R3") %>%  
          
          
          # road HARD
          addPolylines(data = roadsstatusR3, color ="orange", group = "BR", weight = 3,popup = ~paste("Road Status:", roadsstatusR3$type ) ) %>% 
          addLegend(
            title = "Road Accessibility - Source: NIF/INSO MAP ",
            pal = roadpal,
            opacity = 1,
            values = roadsstatus2$type, 
            group = "AR",
            layerId = "R2",
            position = "bottomleft")
        
        
        
      }
      
    }
    
  })    
  
  
  observeEvent(input$CAhardtoreachR, {
    if(!is.null(input$CAhardtoreachR)) {
      leafletProxy("mymap") %>% clearGroup(group = "CW") %>%  removeControl("R4")  
      
      if(TRUE %in% input$CAhardtoreachR){
        leafletProxy("mymap") %>% clearGroup(group = "CW") %>%  removeControl("R4")  
        
        
      }
      
      
      
      if(TRUE %in% input$CAhardtoreachR ){
        leafletProxy("mymap") %>%   removeControl("R1")   %>%  removeControl("R2")  %>%  removeControl("R3")  %>%   
          # road CARGO
          addPolylines(data = roadsstatusR1ac , weight = 3, color = "brown", group = "CW" ) %>% 
          addLegend(
            title = "Road Accessibility - Source: NIF/INSO MAP ",
            pal = roadpal,
            opacity = 1,
            values = roadsstatus2$type, 
            group = "AR",
            layerId = "R4",
            position = "bottomleft")
        
        
        
      }
      
    }
    
  })   
  observeEvent(input$exhardtoreachR, {
    if(!is.null(input$exhardtoreachR)) {
      leafletProxy("mymap") %>% clearGroup(group = "road") %>%  removeControl("R5")  
      
      if(TRUE %in% input$exhardtoreachR){
        leafletProxy("mymap") %>% clearGroup(group = "road") %>%  removeControl("R5")  
        
        
      }
      
      
      
      if(TRUE %in% input$exhardtoreachR ){
        leafletProxy("mymap") %>%  removeControl("R1")%>%  removeControl("R2")    %>%  removeControl("R3")    %>%  removeControl("R4")  %>%     
          # road
          addPolylines(data = roadsstatusR2, color = "red", group = "road", weight = 3, popup = ~paste("Road Status:", roadsstatusR2$type ) ) %>% 
          addLegend(
            title = "Road Accessibility - Source: NIF/INSO MAP ",
            pal = roadpal,
            opacity = 1,
            values = roadsstatus2$type, 
            group = "AR",
            layerId = "R5",
            position = "bottomleft")
        
        
        
      }
      
    }
    
  }) 
  
  # .............................................................................................................
  
  # Relocations         ....................................................................................
  
  observeEvent(input$took, {
    
    leafIconss <- icons(
      iconUrl = ifelse( relocationsT$Status == "Relocation Took Place",
                        "https://gdurl.com/meaC",
                        " "
                        
                        
      ),
      iconWidth = 30, iconHeight = 30,
    )
    
    if(!is.null(input$took)) {
      leafletProxy("mymap") %>% clearGroup(group = "Re") %>%  removeControl("leg")
      
      if(TRUE %in% input$took){
        leafletProxy("mymap") %>%  removeControl("lege")
         
        
        
      }
      
      
      
      if(TRUE %in% input$took ){
        leafletProxy("mymap") %>% removeControl("lege") %>% 
          
          # Relocation took
          addMarkers(data = relocationsT,  relocationsT$lon,  relocationsT$lat, popup = ~paste("<h4>Relocation area</h4>","<b>Location:</b>",Locations), group = "Re", icon = leafIconss) %>% 
          addLegendImage(images = leafImg,
                         labels = val,
                         width = 30,
                         height = 30,
                         labelStyle = "font-size: 14px; vertical-align: middle;",
                         group = "Re",
                         title = 'Source: NIF AOR MAP',
                         layerId = "leg",
                         position = 'bottomright') %>% clearGroup(group = "rek") 
        
        
        
      }
      
    }
    
  })    
  
  
  
  observeEvent(input$ana, {
    
    
    leafIconsA <- icons(
      iconUrl = ifelse( relocationsA$Status == "Announced for Relocation",
                        "https://gdurl.com/zPpr",
                        " "
      ),
      iconWidth = 30, iconHeight = 30,
    )
    
    if(!is.null(input$ana)) {
      leafletProxy("mymap") %>% clearGroup(group = "rek") %>% removeControl("lege")  
      
     
      
      if(FALSE %in% input$ana ){
        leafletProxy("mymap") %>% clearGroup(group = "rek") %>%  removeControl("leg")
        
      }
      
      
      
      if(TRUE %in% input$ana ){
        leafletProxy("mymap") %>% removeControl("leg") %>%   
        
          addMarkers(data = relocationsA, relocationsA$lon,  relocationsA$lat, popup = ~paste("<h4>Relocation area</h4>","<b>Location:</b>",Locations), group = "rek", icon = leafIconsA) %>% 
        
          addLegendImage(images = leafImg,
                         
                         labels = val,
                         width = 30,
                         height = 30,
                         labelStyle = "font-size: 14px; vertical-align: middle;",
                         group = "rek",
                         title = 'Source: NIF AOR MAP',
                         layerId = "lege",
                         position = 'bottomright') 
       
        
        
      }
      
    }
    
  })    
  
  # .............................................................................................................
  # IDPS        ....................................................................................
  
  observeEvent(input$idp1, {
    
    iconnnF <- icons(
      iconUrl = ifelse(idpF$Status == "Formal site",
                       "https://gdurl.com/4NAK",
                       " "
      ),
      iconWidth = 17, iconHeight = 17,
    )
    
    if(!is.null(input$idp1)) {
      leafletProxy("mymap") %>% clearGroup(group = c("cap", "g"))%>%   removeControl("legend") 
      
      if(FALSE %in% input$idp1){
        leafletProxy("mymap") %>% clearGroup(group = c("cap", "g"))%>%   removeControl("legen") 
        
        
      }
      
      
      
      if(TRUE %in% input$idp1 ){
        leafletProxy("mymap") %>% removeControl("legend") %>% 
          
          # camp formal
          addMarkers(data = idpF,  lng = idpF$Longitude,  lat = idpF$Latitude, group = "cap", popup  = ~paste( "<h1>IDP Camp</h1>","<br>","<b>DTM Round:</b>","35", "<br>", "<b>Location:</b>",idpF$Status,  idpF$WARD, "<br>", "<b>Site Name:</b>",idpF$Site_Name, "<br>", "<b>Number of IDPs:</b>", idpF$Number_ind, "<br>","<b>Date of establishment:</b>",idpF$Site_Start ), icon = iconnnF) %>% 
          addLegendImage(images = leafImg1,
                         labels = val1,
                         width = 20,
                         height = 20,
                         layerId = "legen",
                         labelStyle = "font-size: 14px; vertical-align: middle;",
                         group = "cap2",
                         title = 'IDP Camps - Source: IOM ',
                         position = 'bottomright')
        
        
        
      }
      
    }
    
  })    
  
  
  
  observeEvent(input$idp2, {
    
    iconnn <- icons(
      iconUrl = ifelse(idpI$Status == "Informal site",
                       "https://gdurl.com/f8W5",
                       " "
      ),
      iconWidth = 17, iconHeight = 17,
    )
    
    if(!is.null(input$idp2)) {
      leafletProxy("mymap") %>% clearGroup(group = "cap2") %>%   removeControl("legend") 
      
      if(TRUE %in% input$idp2){
        leafletProxy("mymap") %>% clearGroup(group = "cap2")%>%   removeControl("legen") 
        
        
      }
      
      
      
      if(TRUE %in% input$idp2 ){
        leafletProxy("mymap") %>%   removeControl("legen") %>%   
          
          # camp informal
          addMarkers(data = idpI,  lng = idpI$Longitude,  lat = idpI$Latitude, group = "cap2", popup = ~paste("<h1>IDP Camp</h1>","<br>","<b>DTM Round:</b>","35", "<br>", "<b>Location:</b>",idpI$Status,  idpI$WARD, "<br>","<b>Site Name:</b>",idpI$Site_Name, "<br>", "<b>Number of IDPs:</b>",idpI$Number_ind, "<br>","<b>Date of establishment:</b>",idpI$Site_Start ), icon = iconnn) %>% 
          addLegendImage(images = leafImg1,
                         labels = val1,
                         width = 20,
                         height = 20,
                         layerId = "legend",
                         labelStyle = "font-size: 14px; vertical-align: middle;",
                         group = "cap2",
                         title = 'IDP Camps - Source: IOM ',
                         position = 'bottomright')
        
      }
      
    }
    
  })    
  
  # ............................................................................................................. 
  

  
  # HNO       ....................................................................................
  
  observeEvent(input$HNO1, {
    
    
    
    if(!is.null(input$HNO1)) {
      leafletProxy("mymap") %>% clearGroup(group = "HNO")  %>%  removeControl("se") 
      
      if(FALSE %in% input$HNO1){
        leafletProxy("mymap") %>% clearGroup(group = "HNO") %>%  removeControl("se") 
        
        
      }
      
      
      
      if(TRUE %in% input$HNO1 ){
        leafletProxy("mymap") %>% 
          
          # severity
          addPolygons(data = severity_poly,
                      fillColor = ~sev_pal(severity_poly$serv),
                      fillOpacity = 0.5,
                      opacity = 1,
                      weight = 0.1,
                      color='white',
                      label = paste("LGA: ", severity_poly$ADM2_EN, " , "," Severity of Needs: ", severity_poly$ser," , ", "PIN: ", severity_poly$range ),
                      group = "HNO",
                      highlight = highlightOptions(
                        weight = 2,
                        bringToFront = FALSE) ) %>% 
          
          addLegend(
            title = "Severity of Needs - Source: OCHA HNO 2022)",
            pal = sev_pal,
            opacity = 1,
            values = severity_poly$serv, 
            group ="Severity of Needs (HNO 2022)",
            layerId = "se",
            position = "bottomright")
        
        
      }
      
    }
    
  })    
  
  
  
  observeEvent(input$HNO2, {
    
    
    if(!is.null(input$HNO2)) {
      leafletProxy("mymap") %>% clearGroup(group = "HNO1") %>% removeControl("p")
      
      if(TRUE %in% input$HNO2){
        leafletProxy("mymap") %>% clearGroup(group = "HNO1")%>% removeControl("p")
        
        
      }
      
      
      
      if(TRUE %in% input$HNO2 ){
        leafletProxy("mymap") %>%     
          
          addPolygons(data = PIN_poly$geometry,
                      fillColor = "#ffcaa1",
                      fillOpacity = 0.5,
                      weight = 4,
                      color='white',
                      group = "HNO1",
                      popup = paste("<h4>People in Need</h4>","<b>LGA:</b>",PIN_poly$ADM2_EN, "<br>","<b>Range:</b>", PIN_poly$range),
                      highlight = highlightOptions(
                        weight = 5,
                        bringToFront = FALSE)) %>% 
          addControl(html = html_legendp,   layerId = "p", position = "bottomright") 
        
        
        
      }
      
    }
    
  })    
  
  # .............................................................................................................
  #####
  # idpp       ....................................................................................
  
  observeEvent(input$no, {
    
  
    
    if(!is.null(input$no)) {
      leafletProxy("mymap") %>% clearGroup(group = "no") %>% removeControl("fg")
      
      if(FALSE %in% input$no){
        leafletProxy("mymap") %>% clearGroup(group = "no") %>% removeControl("fg")
        
        
      }
      
      
      
      if(TRUE %in% input$no ){
        
        labels <- sprintf(
          "<strong>%s LGA</strong><br/><strong>Total number of IDPs:</strong> %s",
          lga$ADM2_REF, prettyNum(lga$IDPs, big.mark = ",")
        ) %>% lapply(htmltools::HTML)
        
        
        leafletProxy("mymap") %>% 
          addPolylines(data = ad, color = "black", weight = 0.5, group = "no" ) %>% 
          addCircleMarkers( data = lga,
                            lng = lga$lon,
                            lat =  lga$lat,
                            radius = lga$IDPs/4000,
                            
                            label = labels,
                            
                            
                            #label = paste(lga$ADM2_REF, " LGA , ", "Total number of IDPs:", lga$IDPs),
                            group = "no",
                            color = "#B4656F",
                            weight = 1,
                            stroke = 1, fillOpacity = 0.5
                            
          ) %>% 
          addControl(html = html_legend,   layerId = "fg", position = "bottomright") 
        
        
        
      }
      
    }
    
  })    
  
  
  
  observeEvent(input$no1, {
    
    
    if(!is.null(input$no1)) {
      leafletProxy("mymap") %>% clearGroup(group = "no1") %>% removeControl("s")
      
      if(TRUE %in% input$no1){
        leafletProxy("mymap") %>% clearGroup(group = "no1") %>% removeControl("s")
        
        
      }
      
      
      
      if(TRUE %in% input$no1 ){
        
        labels <- sprintf(
          "<strong>%s State</strong><br/><strong>Total number of IDPs:</strong> %s",
          state$ADM1_REF, prettyNum(state$IDPs, big.mark = ",")
        ) %>% lapply(htmltools::HTML)
        leafletProxy("mymap") %>%    
          addCircleMarkers( data = state,
                            lng = state$lat,
                            lat =  state$lon,
                            radius = state$IDPs/15000,
                            
                            label = labels,
                            
                            
                            # label = paste(state$ADM1_REF, " state , ", "Total number of IDPs:", state$IDPs),
                            group = "no1",
                            color = "#805D93",
                            weight = 2,
                            stroke = 1, fillOpacity = 0.5
                            
          )%>% 
          addPolylines(data = admin1 , color = "black", weight = 1, group = "no1" )  %>% 
          addControl(html = html_legens,   layerId = "s", position = "bottomright") 
        
       
    
      
      }
      
    }
    
  })    
  
  # .............................................................................................................
  # inso    ....................................................................................
  
  observeEvent(input$inso, {
    

    
    if(!is.null(input$inso)) {
      leafletProxy("mymap") %>% clearGroup(group = "inso") %>% removeControl("fgcc")
      
      if(FALSE %in% input$inso){
        leafletProxy("mymap") %>% clearGroup(group = "inso")%>% removeControl("fgcc")
        
        
      }
      
      
      
      if(TRUE %in% input$inso ){
        leafletProxy("mymap") %>% 
          addHeatmap( data = inso,
            lng = inso$lon, lat = inso$lat,
            group = "inso",
            
            gradient="YlOrRd",
            blur = 20, max = 0.05, radius = 15
          )  %>% 
          addControl(html = html_legendse,   layerId = "fgcc", position = "bottomright") 
        
        
        
        
        
        
        
      }
      
    }
    
  })    
  
  
  
  
  
  # .................................................................................................
  # cholera ....................................................................................
  
  observeEvent(input$col, {
    
    col_pal <- colorNumeric(
      palette = c("white", "#F5B7B1","#F1948A", "#EC7063","#E74C3C", "#7e1024" ),
      domain = cholera$total)
    
    if(!is.null(input$col)) {
      leafletProxy("mymap") %>% clearGroup(group = "col") %>% removeControl("fgc")
      
      if(FALSE %in% input$col){
        leafletProxy("mymap") %>% clearGroup(group = "col") %>% removeControl("fgc")
        
        
      }
      
      
      
      if(TRUE %in% input$col ){
        leafletProxy("mymap") %>% 
          
          addPolygons(data = cholera,
                      fillColor = col_pal(cholera$total),
                      fillOpacity = 0.7,
                      opacity = 3,
                      weight = 0.1,
                      color = "white",
                      label = paste( "LGA :", cholera$ADM2_EN, " , ", " Cases reported: ", cholera$Cases, " , "," Deaths: ", cholera$Deaths),
                      group = "col",
                      highlight = highlightOptions(
                        weight = 2,
                        bringToFront = FALSE))  %>% 
          addControl(html = html_legendc,   layerId = "fgc", position = "bottomright") 
          
     
          
 
        
        
      }
      
    }
    
  })    
  
  
  
   
  
  # .............................................................................................................
  
  
  
  # nut      ....................................................................................
  
  observeEvent(input$Cu, {
    
    
    
    if(!is.null(input$Cu)) {
      leafletProxy("mymap") %>% clearGroup(group = "nut") %>%  removeControl("nut") 
      
      if(FALSE %in% input$Cu){
        leafletProxy("mymap") %>% clearGroup(group = "nut")%>%  removeControl("nut") 
        
        
      }
      
      
      
      if(TRUE %in% input$Cu ){
        leafletProxy("mymap") %>% removeControl("nut1") %>% 
          
          addPolygons(data = proj_acute_poly,
                      fillColor = prj_acute_pal(proj_acute_poly$pro),
                      fillOpacity = 0.6,
                      opacity = 1,
                      weight = 0.1,
                      color='white',
                      # popup  = paste("LGA: ", proj_acute_poly$ADM2_EN, "  ","  ", proj_acute_poly$la),
                      label  = ~paste(proj_acute_poly$ADM2_EN,proj_acute_poly$curr),
                      group = "nut",
                      highlight = highlightOptions(
                        weight = 3,
                        bringToFront = FALSE)  
          ) %>% 
          
          addLegend(
            title = "IPC Acute Malnutrition Analysis (November 2022)",
            pal = prj_acute_p,
            opacity = 1,
            values = proj_acute_poly$curr, 
            group ="nut",
            layerId = "nut",
            position = "bottomright")
        
        
        
      }
      
    }
    
  })    
  
  
  
  observeEvent(input$Pro, {
    
    
    if(!is.null(input$Pro)) {
      leafletProxy("mymap") %>% clearGroup(group = "gh") %>%  removeControl("nut1") 
      
      if(TRUE %in% input$Pro){
        leafletProxy("mymap") %>% clearGroup(group = "gh")%>%  removeControl("nut1") 
        
        
      }
      
      
      if(TRUE %in% input$Pro ){
        leafletProxy("mymap") %>%    removeControl("nut") %>%  
          
          addPolygons(data = proj_acute_poly,
                      fillColor = ~prj_acute_pal(proj_acute_poly$fg),
                      fillOpacity = 0.6,
                      opacity = 1,
                      weight = 0.1,
                      color='white',
                      label = ~paste(proj_acute_poly$ADM2_EN, proj_acute_poly$sd),
                      group = "gh",
                      highlight = highlightOptions(
                        weight = 3,
                        bringToFront = FALSE)  
          ) %>% 
          addLegend(
            title = "IPC Acute Malnutrition Analysis (November 2022)",
            pal = prj_acute_p,
            opacity = 1,
            values = proj_acute_poly$sd, 
            group ="gh",
            layerId = "nut1",
            position = "bottomright")
        
        
      }
      
    }
    
  })    
  
  # .............................................................................................
  
 
  # flood    ....................................................................................
  
  observeEvent(input$flood, {
    
    
    
    if(!is.null(input$flood)) {
      leafletProxy("mymap") %>% clearGroup(group = "flood") %>% removeControl("flo")
      
      if(FALSE %in% input$flood){
        leafletProxy("mymap") %>% clearGroup(group = "flood") %>% removeControl("flo")
        
        
      }
      
      
      
      if(TRUE %in% input$flood ){
        leafletProxy("mymap") %>% removeControl("floo") %>% 
          
          
          addPolygons(data = floods$geometry,
                      stroke = 0.6,
                      opacity = 3,
                      weight = 0.2,
                      color = "#32ADEA",
                      group =  "flood",
                      highlight = highlightOptions(
                        weight = 1,
                        bringToFront = FALSE)
          ) %>% 
          addControl(html = html_legendf,   layerId = "flo", position = "bottomright") 
        
        
      }
      
    }
    
  })    
  
  
  
  observeEvent(input$flood1, {
    
    pop_pal <- colorNumeric(
      palette = c("white", "#F5B7B1","#F1948A", "#EC7063","#E74C3C" ),
      domain = pop_flood$Total_in_D)
    
    if(!is.null(input$flood1)) {
      leafletProxy("mymap") %>% clearGroup(group = "flood1")  %>% removeControl("floo")
      
      if(TRUE %in% input$flood1){
        leafletProxy("mymap") %>% clearGroup(group = "flood1") %>% removeControl("floo")
        
        
      }
      
      
      
      if(TRUE %in% input$flood1 ){
        leafletProxy("mymap") %>%  removeControl("flo") %>% 
          addCircleMarkers( data = flood,
                            lng = flood$lon,
                            lat =  flood$lat,
                            radius = flood$Total_in_D/3000,
                            
                            label  = ~paste( flood$ADM2_EN, "LGA, ", "Number of Affected Population: ", prettyNum(flood$Total_in_D, big.mark = ",")  ),
                            fillColor = "#ffffff00",
                            color = "#276fbf",
                            group = "flood1",
                            stroke = 1, fillOpacity = 0.5
                           
                            
          ) %>% 
          addControl(html = html_legendf,   layerId = "floo", position = "bottomright") 
          
          
        
      }
      
    }
    
  })    
  
  # .............................................................................................................
  
  # adm    ....................................................................................
  
  observeEvent(input$State, {
    
    
    
    if(!is.null(input$State)) {
      leafletProxy("mymap") %>% clearGroup(group = "adm") %>%  removeControl("adm")
      
      if(FALSE %in% input$State){
        leafletProxy("mymap") %>% clearGroup(group = "adm")%>%  removeControl("adm")
        
        
      }
      
      
      
      if(TRUE %in% input$State){
        leafletProxy("mymap") %>%  removeControl("adm2") %>% 
          
          
          addPolylines(data = admin1 , color = "black", weight = 1.5, group = "adm" ) %>% 
          
          addLegendImage(images = Img1,
                         labels = c("State boundaries", "LGA boundaries", "Ward boundaries" ),
                      
                         width = 20,
                         height = 20,
                         layerId = "adm",
                         labelStyle = "font-size: 14px; vertical-align: middle;",
                         group = "adm",
                         title = 'Administrative Boundaries',
                         position = 'bottomright')
        
        
        
      }
      
    }
    
  })  
  
  ###################
  
  
  observeEvent(input$LG, {
    
    
    if(!is.null(input$LG)) {
      leafletProxy("mymap") %>% clearGroup(group = "LG") 
      
      if(TRUE %in% input$LG){
        leafletProxy("mymap") %>% clearGroup(group = "LG")
        
        
      }
      
      
      
      if(TRUE %in% input$LG ){
        
      
        
        leafletProxy("mymap") %>%    
          addLabelOnlyMarkers(lab$lon, lab$lat, label =  lab$ADM2_EN, 
                             labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T), group = "LG") 
          
          
  
        
      }
      
    }
    
  }) 
  
  #######################
  
  observeEvent(input$LGA, {
    
    
    if(!is.null(input$LGA)) {
      leafletProxy("mymap") %>% clearGroup(group = "LGA") %>%  removeControl("adm2")
      
      if(TRUE %in% input$LGA){
        leafletProxy("mymap") %>% clearGroup(group = "LGA")%>%  removeControl("adm2")
        
        
      }
      
      
      
      if(TRUE %in% input$LGA ){
        
        labelss <- sprintf(
          "<strong>LGA: </strong>%s<br/><strong>Number of INGOs:</strong> %s",
          ngo$ADM2_EN, prettyNum( ngo$partners_2, big.mark = ",")
        ) %>% lapply(htmltools::HTML)
        
        leafletProxy("mymap") %>%  removeControl("adm") %>%    
          
          addPolygons(data = ngo,
                      stroke = 0.1,
                      fillOpacity = 0.1,
                      fillColor = "white",
                      opacity = 1,
                      weight = 0.2,
                      color = "black",
                      label = labelss,
                      
                     # label  = ~paste(ngo$ADM2_EN, "LGA, ", "No. of INGOs:", ngo$partners_2),
                      popup = paste(  "<b>LGA name: </b>",ngo$partners_c, "<br>",  "<b>LGA HQ: </b>",ngo$lgaHQ,  "<br>",   "<b>No. of INGOs: </b>",ngo$partners_2,  "<br>", "<b>INGOs present:</b>", ngo$partners_1, "<br>", "<a href=\"", ngo$link , "\"  target='_blank' >", "Quarterly State Government INGO Reporting Dashboard", "</a>"),
                      group = "LGA",
                      highlight = highlightOptions(
                        weight = 2,
                        bringToFront = TRUE)  
          ) %>% 
          addLegendImage(images = Img1,
                         labels = c("State boundaries", "LGA boundaries", "Ward boundaries" ),
                         
                         width = 20,
                         height = 20,
                         layerId = "adm2",
                         labelStyle = "font-size: 14px; vertical-align: middle;",
                         group = "LGA",
                         title = 'Administrative Boundaries',
                         position = 'bottomright')
        
      }
      
    }
    
  })    
  observeEvent(input$Ward, {
    
    
    if(!is.null(input$Ward)) {
      leafletProxy("mymap") %>% clearGroup(group = "Ward") 
      
      if(TRUE %in% input$Ward){
        leafletProxy("mymap") %>% clearGroup(group = "Ward")
        
        
      }
      
      
      
      if(TRUE %in% input$Ward ){
        leafletProxy("mymap") %>%     
          
          addPolygons(data = admin3,
                      stroke = 0.1,
                      fillOpacity = 0.1,
                      fillColor = "white",
                      opacity = 1,
                      weight = 0.2,
                      color = "black",
                      label  = ~paste(admin3$ADM3_EN, " "),  
                      group = "Ward",
                      highlight = highlightOptions(
                        weight = 2,
                        bringToFront = FALSE)  
          )
        
      }
      
    }
    
  })  
  
  # .............................................................................................................
  
  # food   ....................................................................................
  
  observeEvent(input$food, {
    
    
    
    if(!is.null(input$food)) {
      leafletProxy("mymap") %>% clearGroup(group = "food") %>%  removeControl("food") 
      
      if(FALSE %in% input$food){
        leafletProxy("mymap") %>% clearGroup(group = "food")%>%  removeControl("food") 
        
        
      }
      
      
      
      if(TRUE %in% input$food ){
        leafletProxy("mymap") %>%  removeControl("food1") %>% 
          
          addPolygons(data = food,
                      fillColor = prj_acute_pallll(food$cu),
                      fillOpacity = 0.6,
                      opacity = 1,
                      weight = 0.1,
                      color='white',
                      label  = ~paste(food$ADM2_EN," LGA, ", food$ph, ":", food$label),
                      group = "food",
                      highlight = highlightOptions(
                        weight = 3,
                        bringToFront = FALSE)  
          ) %>% 
          addLegend(
            title = "Food Insecurity Situation (Source: CH - November 2022)",
            pal = prj_acute_pallllz,
            opacity = 1,
            values =food$label, 
            group ="food",
            layerId = "food",
            position = "bottomright")
        
        
        
      }
      
    }
    
  })    
  
  
  
  observeEvent(input$food1, {
    
    
    if(!is.null(input$food1)) {
      leafletProxy("mymap") %>% clearGroup(group = "food1") %>%  removeControl("food1") 
      
      if(TRUE %in% input$food1){
        leafletProxy("mymap") %>% clearGroup(group = "food1") %>% removeControl("food1") 
        
        
      }
      
      
      
      if(TRUE %in% input$food1 ){
        leafletProxy("mymap") %>%  removeControl("food") %>% 
          
          addPolygons(data = food,
                      fillColor = ~prj_acute_pallll1(food$ch),
                      fillOpacity = 0.6,
                      opacity = 1,
                      weight = 0.1,
                      color='white',
                      label  = ~paste(food$ADM2_EN, " LGA,",food$projch, ":", food$label2),
                      group = "food1",
                      highlight = highlightOptions(
                        weight = 3,
                        bringToFront = FALSE)  
          ) %>% 
          addLegend(
            title = "Food Insecurity Situation (Source: CH - November 2022)",
            pal = prj_acute_pallll1z,
            opacity = 1,
            values = food$label2, 
            group ="food1",
            layerId = "food1",
            position = "bottomright")
        
      }
      
    }
    
  })    
  
  # .............................................................................................................
  # len   ....................................................................................
  
  observeEvent(input$len, {
    
    
    
    if(!is.null(input$len)) {
      leafletProxy("mymap") 
      
      if(FALSE%in% input$len){
        leafletProxy("mymap") %>% clearControls()
        
        
      }
      
      if(TRUE %in% input$len ){
        leafletProxy("mymap") 
          
          
        
        
        
      }
      
    }
    
  })  
 
  
  
  
  
  

      
      
      
    
  
  # .............................................................................................................
  
  
  #end of server
}   


shinyApp(ui = htmlTemplate("www/index.html"), server = server)
# shinyApp(ui, server)
# shinyApp(ui, server)
