


############################source############### H E A D E R ##########################################
header = dashboardHeader(
  title = "BEGU PROJECT",
  
  titleWidth = 350,
  
  dropdownMenu(type = "messages",
               messageItem(
                 from = "Sales Dept",
                 message = "Sales are steady this month."
               ),
               messageItem(
                 from = "New User",
                 message = "How do I register?",
                 icon = icon("question"),
                 
                 time = "13:45"
               ),
               messageItem(
                 from = "Support",
                 message = "The new server is ready.",
                 icon = icon("life-ring"),
                 time = "2014-12-01"
               )
  )
  #disable = TRUE
  
)

########################################## S I D E B A R #########################################
sidebar = dashboardSidebar(
  width = 400,
  sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                    label = "Search..."),
  ###################### BAR MENU########
  sidebarMenu(
    
    menuItem("Background", 
             tabName = "background", 
             icon = icon("table")),
    menuItem("Data",
             icon = icon("database"),
             menuSubItem( "CENTA DATABASE FROM WEBSCRAP (CLEANED)",
                          tabName = "data1",
                          icon = icon("table")),
             menuSubItem( "CENTA DATABASE FROM WEBSCRAP (FILTERED)",
                          tabName = "data2",
                          icon = icon("table"))
             
    ),
    
    menuItem("Geospatial Analysis", 
             icon = icon("globe"),
             menuSubItem( "Year by Year metrics by Districts in Hong Kong", 
                          tabName = "ga",
                          icon = icon("calendar")),
             menuSubItem( "Age of Buildings in Hong Kong", 
                          tabName = "ga1",
                          icon = icon("map")),
             menuSubItem( "All 431 Sub-Districts in Hong Kong", 
                          tabName = "ga2",
                          icon = icon("map")),
             menuItem("Year by Year metrics by Sub - Districts in Hong Kong", 
                      icon = icon("map"), 
                      tabName = "ga3",
                      badgeLabel = "soon", 
                      badgeColor = "aqua")
    ),
    menuItem("Statistical analysis",
             icon = icon("signal"),
             menuSubItem("Graphs",
                         tabName = "ch",
                         icon = icon("signal")),
             menuSubItem("Violin  Chart",
                         tabName = "ch1",
                         icon = icon("signal"))
    )
  )
)
############################################ B O D Y ############################################
body = dashboardBody(
  tabItems(
    #################################### ABOUT MENU ########################        
    
    tabItem(tabName = "background",
            h1("INTRODUCTION"),
            splitLayout(
              h1("MAPPING TECHNIQUE"),
              
              h1("")
            ),
            fluidRow( 
              ################################# BACKGROUND TEXT COL 1 ##########
              column(width = 5,
                     h2("Mapping Eviction Risk is a project that collages existing 
                                  data released by the government and the developer and 
                                  releasing it to the masses."),
                     br(),
                     h3("HOW TO WE MAP?"),
                     br(),
                     h3("AGE OF THE BUILDINGS IN HONG KONG"),
                     br(),
                     p("The age of most of the buildings in Hong Kong can be
                                  acquired by a subscription fee of 1500 HKD, which all
                                  developers, investors, news agency has access too. They
                                  can check unlimited amount of buildings. Using software
                                  engineering and the GEOJSON data, we can map the
                                  entire Hong Kong based on the age of the building."),
                     br(),
                     h3("PROPERTIES WITH MORE THAN 80% DEVELOPER OWNERSHIP"),
                     p("This data is released by most of the developers as a
                                  medium to attract new investors and communicate with
                                  existing shareholders. It is easily access by the public, but
                                  the sub-divided unit dwellers might not have the knowledge
                                  to do so."),
                     br(),
                     p("According to current development legislations, if a
                                 developer has more than 80% ownership of a building.
                                 They can apply for forced auction, a process which forces
                                 current owners to give up their flat at the market rate in
                                 a short period of time. This process causes mass eviction
                                 of tenants of a small area in an instance, which affects the
                                 rental market of that area."),
                     br(),
                     br(),
                     h3("Participatory Lego Urban Renewal Framework?"),
                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/QkVKTAzGR5Q" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>'),
                     br(),
                     br(),
                     h3("The life and death of Shek Pai Wan Road?"),
                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/Prg2xwoZxRc" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>'),
                     br(),
                     a(href="https://youtu.be/Prg2xwoZxRc", "LINK TO vIDEO 1 "),
                     a(href="https://www.youtube.com/watch?v=QkVKTAzGR5Q&t=", "LINK TO VIDEO 2")
              ),
              
              ######################## BACKGROUND TEXT GROUND 2############
              column(width = 7,
                     img(src="Begu.JPG", width = "100%", height = "100%")
              )
            )
    ),
    tabItem(tabName = "data1",
            fluidRow(
              column(width = 7,
                     h3('All results from Web Scrapping '),a(href="http://www.centadata.com/", "http://www.centadata.com/"),
                     h3("Over 27,150 web pages and 24 million data points were collect "))
            ),
            dataTableOutput('Centa_data1')
            
    ),
    tabItem(tabName = "data2",
            fluidRow(
              column(width =7,
                     h3('All resultsby filter by floors '),a(href="http://www.centadata.com/")
              )
            ),
            dataTableOutput('Centa_flr')
    ),
    #################################### SUB MAP MENU  ########################        
    tabItem(tabName = "ga",
            ################# INFO BOX IN MAP #########
            fluidRow(valueBoxOutput("dist_name"),
                     valueBoxOutput("avg_price")
              # # column(width = 6
              # #        ),
              # column(width = 6, 
              #        valueBoxOutput("avg_price")
              # )
            ),
            br(),
            leafletOutput( "map",width = "100%", height = 400),
            ################## First map ###################
            absolutePanel(
              ######## DRAGABLE BOX  #####
              id = "timectrl", class = "panel panel-default", fixed = FALSE,
              draggable = TRUE, top = 120, left = "auto", right = 30, bottom = "auto",
              width = 210, height = "auto",
              h2("Districts of Hong Kong"),
              sliderInput("yr", "Years", min(Centa_fl_18distYR$Year), max(Centa_fl_18distYR$Year),
                          value = 2017 , step = 1,
                          animate = animationOptions(interval = 5000, loop = TRUE)),
              
              selectInput("crit", "Criteria:", colnames(Centa_fl_18distYR[c(5,6,8)])),
              selectInput("colors", "Color Scheme",
                          rownames(
                            subset(brewer.pal.info, category %in% c("seq", "div")
                                          )
                                        )
                              ),
                  selectInput("dist_id", "District of:", unique(Centa_fl_18distYR$ENAME))
                  
                  )
              
              ),
      ################################### AGE OF BULDINGS IN HK #########
      tabItem(tabName = "ga1",
              leafletOutput( "map2",width = "100%", height = 300)
      ),
      tabItem( tabName = "ga2",
               leafletOutput( "map3",width = "100%", height = 300)
      ),
      #################################### CHART MENU ########################        
      tabItem(tabName = "ch",
              h3("Numerical Analysis of Hong Kong Housing Market from 1995 to 2018"),
              sidebarLayout(
                sidebarPanel(
                    h2("Graph 1 Options:"), 
                    br(),
                    sliderInput("P_SQFT", "Price per saleable SQFT:", 
                                min= 1000, #min(Centa_flr$price_per_saleable_area)
                                max= max(Centa_flr$price_per_saleable_area),
                                value = range(Centa_flr$price_per_saleable_area),
                                step = 10000,
                                round = T 
                    ),
                    sliderInput("floors", "Floor:", 
                                min= 0,
                                max= max(Centa_flr$property.floor),
                                value = 30,
                                step = 1,
                                round = T
                    ),
                    sliderInput("yr1", "Year:", 
                                min= min(Centa_flr$sold.year),
                                max= max(Centa_flr$sold.year),
                                value = c(2015,2017),
                                step = 1,
                                round = T
                    )
                ),
                mainPanel(tabsetPanel(
                  tabPanel("Height Relationships with Price per SQFT", 
                           plotOutput(outputId = "g_age")),
                  tabPanel("Age Relationship with Price per SQFT",
                           plotOutput(outputId = "g_floor"))
                  ))
              )
              
              ),
      tabItem(tabName = "ch1",
              h3("Numerical Analysis of Hong Kong Housing Market from 1995 to 2018"),
              sidebarLayout(
                sidebarPanel(
                  h2("Violin Options"), 
                  br(),
                  sliderInput("b_yr", "Year:", 
                              min= min(Centa_flr$sold.year),
                              max= max(Centa_flr$sold.year),
                              value = 2017,
                              step = 1,
                              round = T
                              )
                  
                ),
                mainPanel(
                  plotOutput(outputId = "b_price")
                ))
              )
    )
  )
  shinyUI(dashboardPage(header, sidebar, body,
                        skin = "black",
                        tags$h2("Add a shiny app background image")
                        
  ))



