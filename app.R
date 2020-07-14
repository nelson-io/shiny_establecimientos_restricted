library(shiny)
library(dplyr)
library(leaflet)
library(sf)
library(DT)
library(janitor)
library(shinydashboard)


load("data.Rdata")
choices <- c("Todos", sort(unique(x$permitido)))
choices_act <- c("Todas","Permitidas", sort(unique(x$actividadc)))
choices_esenc <- c("Todas", sort(unique(x$esencial)))
choices_zona <- c("CABA", sort(unique(x$intersected)))
choices_comunas <- c("Todas", sort(unique(x$comunas)))

act_excluidas <- c("Industrias","Depósitos","DESOCUPADO",
  "Peluquerías, manicuras,  pedicuras, centros de belleza, spas y  depilación",
  "Hoteles, hoteles familiares, pensiones y geriátricos",
  "Gimnasios, canchas de alquiler y yoga",
  "Locales para esparcimiento (peloteros, teatros, cines, locales bailables)",
  "Locutorios y servicios de Internet","Tatuajes y Piercings",
  "Paseos de compras (puestos con estructura de metal)")

act_incluidas <- unique(x$actividadc)[!(unique(x$actividadc) %in% act_excluidas)]


#meter dentro del rdata!!!
poli_list <- list()
poli_list$zonas <- st_read("zonas_comerciales.geojson")
poli_list$corredores <- st_read("corredores_lineales_modif.shp") %>% 
    st_transform(crs = 5347) %>% ``
    st_buffer(dist = 13) %>%
    st_transform(crs = 4326)




ui <- navbarPage(
    "Comercios permitidos y Zonas estratégicas en la CABA",
    tabPanel("Mapa",
             
             
             bootstrapPage(
                 div(
                     class = "outer",
                     tags$style(
                         type = "text/css",
                         ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"
                     ),
                     leafletOutput("mapa_caba", width = "100%", height = "100%"),
                     absolutePanel(
                         top = 105, 
                         right = 10, 
                         left = "auto",
                         bottom = "auto",
                         
                         
                         
                         selectInput(
                             "perm_input",
                             "Permiso",
                             choices = choices,
                             selected = "Todos",
                             multiple = F,
                             selectize = T
                         ),
                         
                         
                         
                         selectInput(
                             "act_input",
                             "Actividad",
                             choices = choices_act,
                             selected = 'Todas',
                             multiple = T,
                             selectize = T
                             
                             
                         ),
                         
                         selectInput(
                             "esenc_input",
                             "Tipo de actividad",
                             choices = choices_esenc,
                             selected = 'Todas',
                             multiple = F,
                             selectize = T
                             
                             
                         ),
                         
                         selectInput(
                             "zonas_input",
                             "Zona",
                             choices = choices_zona,
                             selected = 'CABA',
                             multiple = F,
                             selectize = T
                             
                             
                         ),
                         
                         selectInput(
                             "comunas",
                             "Comuna",
                             choices = choices_comunas,
                             selected = 'Todas',
                             multiple = T,
                             selectize = T
                             
                         ),
                         
                         infoBoxOutput("total_comercios"),
                     )
                 )
                 
                 
                 
             )),
    tabPanel("Tabla de resumen",
             
             DT::dataTableOutput("table"))
)

server <- function(input, output, session) {
    
    
    filtered_data <- reactive({
        if("Todos" %in% input$perm_input){x}
        else{x %>% filter(permitido %in% input$perm_input)}
    })
    
    filtered_data_2 <- reactive({
        if("Todas" %in% input$act_input){filtered_data()}
        else if("Permitidas" %in% input$act_input){filtered_data() %>% filter(actividadc %in% c(act_incluidas,input$act_input ))}
        else{filtered_data() %>% filter(actividadc %in% input$act_input)}
    })
    
    filtered_data_3 <- reactive({
        if("Todas" %in% input$esenc_input){filtered_data_2()}
        else{filtered_data_2() %>% filter(esencial %in% input$esenc_input)}
    })
    
    filtered_data_4 <- reactive({
        if("CABA" %in% input$zonas_input){filtered_data_3()}
        else{filtered_data_3() %>% filter(intersected %in% input$zonas_input)}
    })
    
    filtered_data_5 <- reactive({
        if("Todas" %in% input$comunas){filtered_data_4()}
        else{filtered_data_4() %>% filter(comunas %in% input$comunas)}
    })
    
    
    
    filtered_data_table <- reactive({
        filtered_data_5() %>% 
            st_drop_geometry() %>% 
            group_by(`Actividad` = actividadc, `Comuna` = comuna) %>% 
            summarise(`Total` = n()) %>% 
            ungroup() %>% 
            adorn_totals()
    })
    
    totals <- reactive({
        
        filtered_data_5() %>% 
            st_drop_geometry() %>% 
            summarise(total = n())

    })
    
    output$mapa_caba <- renderLeaflet({
        leaflet() %>% 
            addProviderTiles(provider = providers$Stamen.TonerLite) %>% 
            addPolygons(data = poli_list$zonas,color = "orange") %>% 
            addPolygons(data = poli_list$corredores,color = "darkred") %>% 
            fitBounds(lng1 = -58.45,lat1 = -34.69, lng2 = -58.35,lat2 = -34.53)
        
    })
    
    observe({
        
        leafletProxy("mapa_caba", data = filtered_data_5()) %>%
            clearMarkerClusters() %>% 
            addMarkers(clusterOptions = markerClusterOptions(),popup = filtered_data_5()$data_labels)
    })
    
    output$table <- DT::renderDataTable({filtered_data_table()})
    
    output$total_comercios <- renderInfoBox({
        infoBox("Total de comercios:", totals())  
    })
    
    
    
}

shinyApp(ui, server)
