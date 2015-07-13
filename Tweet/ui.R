# ui for JAEG Tweet

dashboardPage(skin="purple",

# ---- Dashboard Header ----              
  dashboardHeader(title = "JAEG Tweet!",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Emelie & Ben",
                                 message = "Welcome!",
                                 icon = icon("smile-o"),
                                 time = Sys.Date()
                               )
                  )
  ), # dashboardHeader
 
# ---- Dashboard SideBar ----  
  dashboardSidebar(
              
    # Show current date            
    h3(textOutput(("currentDate")), align="center"),
       
    # Create side menu         
    sidebarMenu(
      menuItem("Tweets!", tabName = "tweet", icon = icon("twitter-square")),
      menuItem("#cdnpoli party leader!", tabName = "cdnpoli_tweet", icon = icon("twitter")),
      menuItem("Gotta luv @('_')@", tabName = "aboutus", icon = icon("heart")),
      menuItem("Emelie", href = "https://ca.linkedin.com/pub/emelie-gustafsson/58/930/647", icon = icon("linkedin")),
      menuItem("Ben", href = "https://ca.linkedin.com/in/beneditochou", icon = icon("linkedin")),
      # Gotta have @('_')@!
      p(img(src="SnowMonkey.jpg", width="100%"))
    )
    
  ), # dashboardSideBar
    
# ---- Dashboard Body ----  
  dashboardBody(
    
    tabItems(
      
      # User Tweet Tab
      tabItem(tabName = "tweet",
              
        fluidPage(
          includeCSS("www/styles.css"), # include custom css
          fluidRow(
            infoBoxOutput("user_fav"),
            infoBoxOutput("user_follower"),
            infoBoxOutput("user_friend")
          ),
          fluidRow(
            box(background = "black",
                uiOutput("image_link")
            ),
            box(
              textInput("handle", "Enter Twitter Handle!", "@taylorswift13"),
              numericInput("num_t", "How many tweets?", 
                           value = 50, min = 10, max = 1000, step = 100),
              helpText("By default, retweet is excluded"),
              checkboxInput("rt_yn", "Include Retweet?", value=FALSE),
              actionButton("go", "Grab Tweet!")
            )
          ),
          fluidRow(
            box(
            selectizeInput("var1_fill", "What do you wanna know?",
                           choices = c(
                             "Any Tweet" = "tweetCount",  
                             "Number of Retweet" = "retweetCount",
                             "Number of Favorite" = "favoriteCount"
                           ),
                           select = "tweetCount")
            )
          ),
          fluidRow(
            # Magic happen with sortablejs!
            tagList(
              tags$div(id = "sortplotty",
                box(plotOutput("user_calender")),
                box(plotOutput("plot_wordcloud")),
                box(title = "Tweets by the Hour",
                    plotOutput("user_hour", height=200),
                    plotOutput("user_hour_wday", height=200)
                ),
                box(title = "Twitter Followers",
                    actionButton("map", "Map Follower"),
                    helpText("Map followers. This will take a couple of seconds, please be patient."),
                    leafletOutput("user_follower_map")
                ) ,sortableR("sortplotty")
              ) # tagdiv
            ) # tagList
          ),
          fluidRow(
            br(),
            dataTableOutput("tweet_tbl")
          )
        )
      ), # tabItem "Tweet"
      
      # Canadian Party Leaders Tweet Tab
      tabItem(tabName = "cdnpoli_tweet",
      # See http://stackoverflow.com/questions/11701311/logo-image-and-h1-heading-on-the-same-line
      # add custom class and style it in css so the image can sit on the same line as the header
        fluidPage(
          div(class = "header",
              img(src="canada_flag.png"),
              h2("Tweets of Canadian Political Party Leader",
                 style = "color: red;")),
          br(),br(),
          wellPanel(
            fluidRow(
              box(height=160,
                  numericInput("num_cdn_t", "How many tweets?", 
                               value = 50, min = 10, max = 1000, step = 100),
                  helpText("By default, retweet is excluded"),
                  checkboxInput("r_twt_yn", "Include Retweet?", value=FALSE)
              ),
              box(height=160,
                  selectizeInput("var_fill", "What do you wanna know?",
                                 choices = c(
                                   "Any Tweet" = "tweetCount",  
                                   "Number of Retweet" = "retweetCount",
                                   "Number of Favorite" = "favoriteCount"
                                 ),
                                 select = "tweetCount"),
                  actionButton("go2", "Grab Tweet!")
              )
            ) 
          ),
          
          # Calender Plot Tab
          tabBox(width = 12,
            title = "Calender Plot",
            # you can use image as the title and still reference it!
            selected = img(src="ndp_party_logo.png", height=20),
              
            tabPanel(title=img(src="con_party_logo.png", height=20),
               fluidPage(
                 box(solidHeader = TRUE, plotOutput("con_calender")),
                 box(solidHeader = TRUE, plotOutput("plot_wordcloud_con"))
               )
              ),
              
            tabPanel(title=img(src="lib_party_logo.png", height=20),
               fluidPage(
                 box(solidHeader = TRUE, plotOutput("lib_calender")),
                 box(solidHeader = TRUE, plotOutput("plot_wordcloud_lib"))
                )
               ),
              
            tabPanel(title=img(src="ndp_party_logo.png", height=20),
               fluidPage(
                box(solidHeader = TRUE, plotOutput("ndp_calender")),
                box(solidHeader = TRUE, plotOutput("plot_wordcloud_ndp"))
                )
               ),
              
            tabPanel(title=img(src="green_party_logo.png", height=20),
               fluidPage(
                box(solidHeader = TRUE, plotOutput("green_calender")),
                box(solidHeader = TRUE, plotOutput("plot_wordcloud_green"))
                )
               ),
              
            tabPanel(title=img(src="bloc_party_logo.png", height=20),
                #"Bloc Québécois", 
               fluidPage(
                 box(plotOutput("bloc_calender")),
                 box(plotOutput("plot_wordcloud_bloc"))
               )
            )
          ) # tabBox
        ), # fluidPage
      
      fluidPage(id = "sortplotty2",
        box(title = "Common Words",
            plotOutput("plot_commoncloud")),
        box(title = "Different Words",
            plotOutput("plot_comparecloud")),
        box(title = "Friends and Favorites", 
            plotOutput("party_metric")),
        box(title = "Followers", 
            plotOutput("treemap")),
        box(title = "Tweet by Hours",
            plotOutput("party_by_hour"))
        ,sortableR("sortplotty2"))
      ), # tabItem "cdnpoli_tweet"
      
      tabItem(tabName = "aboutus",
        fluidPage(
          column(width = 5,
          includeMarkdown("www/aboutus.md")
          )
        )
      ) # tabItem "About Us"
      
    ) # tabItems
  ) # dashboardBody
) # dashboardPage