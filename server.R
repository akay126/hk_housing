library('shiny')
library('shinydashboard')
library('rgdal')
library('dplyr')
#source("centa_map.R")
library('ggplot2')
library("htmltools")




shinyServer(function(input, output, session) {
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  
  ######################## REACTIVE FUNCTION ###########################################################
  ########### reactive FILTER DATA FROM MAP 1 "yr"#########
  filteredData <- reactive({
    shp_district2@data =left_join(shp_district1@data,
                                Centa_fl_18distYR %>%
                                      filter(., Year == input$yr ) %>% 
                                      mutate(.,input$crit))
    shp_district2
  })
  colorpal <- reactive({
    colorNumeric(input$colors, shp_district2$Average_Price)
  })
  
  filterDist <- reactive({
    Centa_fl_18distYR %>% 
      filter(., Year == input$yr, ENAME == input$dist_id)
  })
  ################ reactive FILTER DATA FROM GRAPH #####
  filteredFloor = reactive({
    Centa_floor =Centa_flr %>% 
      dplyr::filter(input$P_SQFT[1] <= price_per_saleable_area &
                      input$P_SQFT[2] >= price_per_saleable_area &
                      property.floor <= input$floors &
                      sold.year >= input$yr1[1]&
                      sold.year <= input$yr1[2]) %>% 
      dplyr::group_by(.,region,floor =property.floor,sold.year) %>% 
      dplyr::summarise(.,Average_price_per_saleable_area = mean(price_per_saleable_area))
    Centa_floor
  })
  ################ reactive FILTER DATA FOR BOX PLOT ###################
  filterBox = reactive({
    Centa_reduced =Centa_reduced %>% 
      tidyr::drop_na(.,price_per_saleable_area)
    
    Centa_price =Centa_reduced%>% 
      dplyr::filter(., sold.year == input$b_yr) %>% 
      dplyr::arrange(.,desc(region)) %>% 
      dplyr::transmute(.,region,dist,sold.year,price_per_saleable_area)
    
  })
################################## M A P   L A Y E R ########################################################
  ########################## MAP 1 - Time series Price over the years #######
  output$map <- renderLeaflet({
    pal = colorpal ()
    leaflet(shp_district) %>% 
      leaflet::addTiles() %>%
      addProviderTiles('CartoDB.DarkMatter') %>% 
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal(Average_Price),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = paste(sep = "<br/>",
                                paste0("<b><a href=",shp_district$URL,">",shp_district$ENAME,"</a></b>"),
                                shp_district$crit,
                                round(shp_district$Average_Price))) 
  })
  ######################### observe for MAP  1 value Boxes ##############
  observe({
    output$dist_name <- renderValueBox({
      valueBox(input$dist_id, "Name of District", icon = icon("city"),
               color = "purple"
      )
    })
    output$avg_price <- renderValueBox({
      if(input$crit == "Average_Price"){
        valueBox(paste("$ ",formatC(filterDist()$Average_Price, format="f", big.mark=",", digits=1)),
                 'Average Price', icon = icon("money-bill-alt"),
                 color = "green")
      }else if(input$crit == "Average_Saleable_Area"){
        valueBox(paste(formatC(filterDist()$Average_Saleable_Area, big.mark=",", digits=1), " ft2"), 
                 "Average Saleable Area", icon = icon("money-bill-alt"),
                 color = "blue")
      }else{
        valueBox(paste("$ ",formatC(filterDist()$Average_Saleable_Area_Price, format="f", big.mark=",", digits=1), " /ft2"), 
                 'Average Saleable Area Price', icon = icon("money-bill-alt"),
                 color = "red")
      }
    })
  })
    ######################### observe for MAP 1 leaflet MAPS ##############
  observe({
    pal = colorpal ()
    
    if (input$crit== "Average_Price"){
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        # addTiles() %>% 
        addProviderTiles('CartoDB.DarkMatter') %>% 
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,
                    fillColor = ~pal(Average_Price),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    popup = paste(sep = "<br/>",
                                  paste0("<b><a href=",filteredData()$URL,">",filteredData()$ENAME,"</a></b>"),
                                  'Average Price',
                                  round(filteredData()$Average_Price)))
                      # addLegend('topleft', pal = pal , values = filteredData()$Average_Price, opacity = 1,
                      #           title = "Mean Saleable Area (sq.ft)"))
    }else if ( input$crit== "Average_Saleable_Area"){
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        # addTiles() %>%
        addProviderTiles('CartoDB.DarkMatter') %>%
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,
                    fillColor = ~pal(Average_Saleable_Area),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    popup = paste(sep = "<br/>",
                                  paste0("<b><a href=",filteredData()$URL,">",filteredData()$ENAME,"</a></b>"),
                                  'Average Saleable Area',
                                  round(filteredData()$Average_Saleable_Area)))

    }else{
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        # addTiles() %>%
        addProviderTiles('CartoDB.DarkMatter') %>%
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,
                    fillColor = ~pal(Average_Saleable_Area_Price),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    popup = paste(sep = "<br/>",
                                  paste0("<b><a href=",filteredData()$URL,">",filteredData()$ENAME,"</a></b>"),
                                  input$crit,
                                  round(filteredData()$Average_Saleable_Area_Price)))
      # addLegend('topright', pal = ~pal , values = ~filteredData()$Average_Saleable_Area_Price, opacity = 1,
      #            title = 'Average Saleable Area Price'))
    }
    
  })

  ################# MAP 2 ################
  
  output$map2 = renderLeaflet({
    pal_ba <- colorNumeric(
      palette = "RdYlBu",
      reverse = T,
      domain = shp_district$Average_Building_Age)
    
  leaflet(shp_district) %>% 
    addTiles() %>% 
    addProviderTiles('CartoDB.DarkMatter') %>% ##CartoDB.DarkMatter ,OpenStreetMap.Mapnik
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal_ba(Average_Building_Age),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = paste(sep = "<br/>",
                                paste0("<b><a href=",shp_district@data$URL,">",shp_district@data$ENAME,"</a></b>"),
                                "Mean Age of Buidling: ",
                                shp_district@data$Average_Building_Age)) %>% 
      addLegend('topleft', pal = pal_ba , values = ~Average_Building_Age, opacity = 1,
                title = "Mean Bldg Age By Dist.")
    
  })
  
  ################# MAP 3################
  output$map3 = renderLeaflet({
    leaflet(shp_district) %>% 
      leaflet::addTiles() %>%
      leaflet::addPolygons(weight = 6, 
                           fillOpacity = 0,
                           smoothFactor = 0.5 ,
                           color = "blue" , 
                           label = ~ENAME) %>% 
      leaflet::addPolygons(data = shp_subdistrict,
                           weight = 2, 
                           smoothFactor = 0.5 ,
                           popup = ~ENAME,
                           color = "green",
                           highlightOptions = highlightOptions(color = "red", weight = 6,
                                                               bringToFront = TRUE))
  })
  
  
################################# G R A P H   P L O T ##########################################################
  ################### GRAPH PLOT AT TAB 1 ###################
  output$g_age = renderPlot({
    ggplot(Centa_floor, aes (x= Average_price_per_saleable_area, y=  property.floor)) +
      geom_point(aes(x = Average_price_per_saleable_area, y= property.floor)) +
      geom_smooth() +
      facet_grid(input$yr1[1]:input$yr1[2]~.)
  })
  ################## GRAPH PLT AT TAB 2##################
  output$g_floor = renderPlot({
    Centa_age = Centa_reduced %>% 
      tidyr::drop_na(price_per_saleable_area) 
    Centa_age = Centa_age %>% 
      group_by(.,Age = bldg.age.from) %>% 
      summarise(.,Average_price_per_saleable_area = mean(price_per_saleable_area))
    ggplot(Centa_age,aes(x = Age, y = Average_price_per_saleable_area)) + geom_smooth()
  })
  #################### observe Graph side bar inputs #########
  observe({
    output$g_age = renderPlot({
      ggplot(filteredFloor(), aes (x= Average_price_per_saleable_area, y=  floor)) +
        geom_point(aes(x = Average_price_per_saleable_area, y= floor)) +
        geom_smooth() +
        facet_grid(input$yr1[1]:input$yr1[2]~.)
    })
  #################### BOXPLOT  TAB 1 #############
    observe({
      output$b_price = renderPlot({
        ylim1 = boxplot.stats(filterBox()$price_per_saleable_area)$stats[c(1, 5)]
        
        ggplot(filterBox(),aes(y= price_per_saleable_area ,x = dist),fill = dist) +
          geom_violin(aes(color = region))
          # theme(plot.subtitle = element_text(vjust = 1), 
          #       plot.caption = element_text(vjust = 1)) +
          # labs(x = "District", y = "Price per Square Foot",
          #      colour = "Region") + theme(plot.title = element_text(size = 15, 
          #                                                           hjust = 0.5)) +labs(title = paste("Year",filterBox())) +
          # coord_cartesian(ylim = ylim1*2.5)
      })
    })
    
  

    # print(input$yr1)
    # sliderInput("P_SQFT", "Price per saleable SQFT:", 
    #             min= min(filteredFloor()$price_per_saleable_area),
    #             max= max(filteredFloor()$price_per_saleable_area),
    #             value = round(quantile(filteredFloor()$Average_price_per_saleable_area)),
    #             step = 1000 
    # )
    # sliderInput("floors", "Floor:", 
    #             min= 0,
    #             max= max(filteredFloor()$property.floor),
    #             value = range(filteredFloor()$property.floor),
    #             step = 1
    # )
    # sliderInput("yr1", "Year:", 
    #             min= min(filteredFloor()$sold.year),
    #             max= max(filteredFloor()$sold.year),
    #             value = c(2017,2018),
    #             step = 1
    # )
    
  })
################################ D    A     T     A     TABLE##########################################
  ############################ CENTA REDUCED ############## 
  output$Centa_data1 = DT::renderDataTable(Centa_reduced,options = list(
    columnDefs = list(list(className = 'dt-center', targets = 5)),
    pageLength = 10
  ))
  ############################ CENTA FLOORS ############## 
  output$Centa_flr = DT::renderDataTable(Centa_flr,options = list(
    columnDefs = list(list(className = 'dt-center', targets = 5)),
    pageLength = 10
  ))

  ##################

  
  
  
  
  
})