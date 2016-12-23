dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Weather Taste: City Recommendation Tool"
  ),
  
  dashboardSidebar(
    includeCSS("www/styles.css"),
    includeScript("www/d3.v3.min.js"),
    includeScript("www/d3-tip.js"),
    useShinyjs(),
    sidebarMenu(
      menuItem("Homepage", tabName = "homepage", 
               icon = icon("home", lib="font-awesome")
      ),
      menuItem("User Analysis", tabName = "analysis",
               icon = icon("play", lib="font-awesome")
      ),
      menuItem("City Recommendation", tabName = "cityrecommendation",
               icon = icon("map-marker", lib="font-awesome")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName ="cityrecommendation",
              fluidRow(
                column(3,
                       selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                ),
                column(3,
                       textInput("text", label = "User ID", value = ""),
                       h5("e.g. enter 'S35223' as user ID, you may have more options in the below dataset:"),
                       helpText(a("User Dataset",href="https://github.com/Sapphirine/Weather_Taste_city_recommendation_tool/blob/master/Data/UserData_Example/user_data_example.csv "))
                )
              ),
              fluidRow(
                dateInput('data1',
                          label='Start Date:',
                          value=Sys.Date()),
                dateInput('data2',
                          label='End Date:',
                          value=Sys.Date())
              ),
              hr(),
              DT::dataTableOutput("ziptable"),
              plotOutput("user_graph"),
              selectInput("rank", "Rank", c("rank"="", 1:20),selected = 1),
              plotOutput("user_station"),
              numericInput("num",label = "Choose How Many Points",value = 10),
              leafletOutput("map")
              
      ),
      tabItem(tabName = "analysis",
              textInput("text1", label = "User ID", value = ""),
              h5("e.g. enter 'S35223' as user ID, you may have more options in the below dataset:"),
              helpText(a("User Dataset",href="https://github.com/Sapphirine/Weather_Taste_city_recommendation_tool/blob/master/Data/UserData_Example/user_data_example.csv ")),
              plotOutput("analysis1"),
              DT::dataTableOutput("table")
              
              
      ),
      
      tabItem(tabName = "map",
              box(width = 12,
                  uiOutput("mymap1")
              )
      ),

      tabItem(tabName = "homepage",
                  uiOutput("hp")
      )
      
    ) ## end tabItems
  ) ## end dashboardBody
) ## end dashboardPage
