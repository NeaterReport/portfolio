# server for JAEG Tweet

function(input, output) {
  
# ---- Get User Tweet ----
  
  # Grab Tweet
  user_info <- reactive({
    withProgress({
      setProgress(message = "Grabbing Tweets!")})
    input$go
    isolate({num_set <- input$num_t
    # clean up the @ sign if is there
    tweethandle <- gsub("@", "", input$handle)
    get_user_tweet(tweethandle, num_t = num_set, rt = input$rt_yn)
    })
  })
  
  # ---- Get Picture! ----
  
  # This Work! Use renderUI to insert linked photo
  output$image_link <- renderUI({
    input$go
    HTML(paste0('<img src = "',
                gsub("normal.jpeg", "400x400.jpeg", user_profile$profileImageUrl),
                '" align="middle">')) 
  })
  
# ---- Get Twitter Profile Stats ---- 
  
  output$user_fav <- renderInfoBox({
    input$go
    infoBox("Favorites", comma(user_profile$favoritesCount), icon = icon("heart"),
            color = "purple"
    )
  })
  
  output$user_follower <- renderInfoBox({
    input$go
    infoBox("Follower", comma(user_profile$followersCount), icon = icon("twitter-square"),
            color = "purple"
    )
  })
  
  output$user_friend <- renderInfoBox({
    input$go
    infoBox("Friends", comma(user_profile$friendsCount), icon = icon("group"),
            color = "purple"
    )
  })
  
# ---- Make Calender plot ----
  output$user_calender <- renderPlot({
    
    # Create the dataframe with the custom fn
    create_cal2_df(user_info()$tweet_df)
    
    ggplot(user_day_df, aes(wday, m_week)) + 
      geom_tile(data = user_shadow_df, aes(wday, m_week), fill = "grey92", color = "white") +
      geom_tile(data = user_day_df, aes_string(fill = input$var1_fill), color = "white") + 
      facet_grid(.~month, drop=TRUE) + 
      labs(x = "", y = "Week of the Month") +
      scale_x_discrete(drop=FALSE, limits = rev(levels(wday))) +
      scale_y_discrete(drop=FALSE, limits = rev(levels(user_day_df$m_week))) +
      theme_bw() + 
      scale_fill_gradient(high = "#FF75FF", low = "#EBF0F5") +
      theme(panel.grid.major = element_blank(),
            panel.background = element_rect(fill = "#FAF0E6"),
            strip.background = element_rect(fill = "#6600CC"),
            strip.text = element_text(color = "white", face = "bold", size = rel(1.3)),
            legend.position="right")
  })
 
# ---- Make Tweet by hour plot ----
  
  # Hour by weekday density plot
  output$user_hour_wday <- renderPlot({
    input$go
    ggplot(user_df, aes(hour, fill=wday)) + 
      geom_density(alpha = 1/4, adjust=.2, color=NA) + theme_fivethirtyeight() +
      scale_x_continuous(breaks=seq(0, 24, by = 4)) + 
      scale_y_continuous(name = "", breaks = NULL)
  })
  
  # Hour bar plot
  output$user_hour <- renderPlot({
    input$go
  ggplot(user_df, aes(hour)) + 
    geom_bar(position="stack", alpha=2/3) + theme_fivethirtyeight() + 
    scale_x_continuous(breaks=seq(0, 24, by = 4)) +
    theme(panel.background = element_rect(fill = "#FAF0E6"))
    # ggtitle("Number of Tweet by Hour of the Day")
  })
  
# ---- User Word Cloud ----

  output$plot_wordcloud <- renderPlot({
    input$go
    withProgress({
      setProgress(message = "Drawing Word Cloud")})
    tdm <- TermDocumentMatrix(user_info()$tweet_text)  
    matrix <- as.matrix(tdm)
    v <- sort(rowSums(matrix),decreasing = TRUE)
    d <- data.frame(word = names(v),freq = v)
    wordcloud(d$word, d$freq, colors = brewer.pal(8, "Set1"))
  })
  
  
# ---- Make Tweet Table ----
  
  output$tweet_tbl <- renderDataTable({
    input$go
    data <- subset(user_info()$tweet_df, 
                   select=c("text", "created", "favoriteCount", "retweetCount", "isRetweet"))
    
    # Make prettier label
    names(data) <- c("Tweets","Time","Favorite Count","Retweet Count","Retweet?")
    
    # Customize the DT
    DT::datatable(data, filter = 'top', extensions = c('ColReorder','Scroller'), options = list(
      dom = 'Rlfrtip', colReorder = list(realtime = TRUE),
        deferRender = TRUE,
        dom = "frtiS",
        scrollY = 200,
        scrollCollapse = TRUE,
      paging = FALSE
      )) %>% 
      formatStyle("Tweets", Color = "#666699")
  })
  
# ---- Get Canadian Political Party Leader Tweet ----
  
  cdnpoli_tweet <- reactive({
    withProgress({
      setProgress(message = "Grabbing Tweets!")})
    get_cdnpoli_tweet(num_twt = input$num_cdn_t, r_twt = input$r_twt_yn)
  })
    
  # calculate all the calender at once with the custom fn
  observe({
    create_cal_df(cdnpoli_tweet()$con_df)
    create_cal_df(cdnpoli_tweet()$lib_df)
    create_cal_df(cdnpoli_tweet()$green_df)
    create_cal_df(cdnpoli_tweet()$bloc_df)
    create_cal_df(cdnpoli_tweet()$ndp_df)
  })
    
# ---- Make Calender Plot! ----
    
  # Convservative
  output$con_calender <- renderPlot({
    # create_cal_df(cdnpoli_tweet()$con_df)
    ggplot(pmharper_day_df, aes(wday, m_week)) + 
    geom_tile(data = pmharper_shadow_df, aes(wday, m_week), fill = "grey92", color = "white") +
    geom_tile(data = pmharper_day_df, aes_string(fill = input$var_fill), color="white") + 
    facet_grid(.~month, drop=TRUE) + 
    labs(x = "", y = "Week of the Month") +
    scale_x_discrete(drop=FALSE, limits = rev(levels(wday))) +
    scale_y_discrete(drop=FALSE, limits = rev(levels(pmharper_day_df$m_week))) +
    theme_bw() + 
    scale_fill_gradient(high = "#24476B", low = "#EBF0F5") +
    theme(panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "#FAF0E6"),
          strip.background = element_rect(fill = "#24476B"),
          strip.text = element_text(color="white", face = "bold", size = rel(1.3)),
          legend.position = "bottom")
  })
  
  # NDP
  output$ndp_calender <- renderPlot({
    # create_cal_df(cdnpoli_tweet()$ndp_df)
    ggplot(ThomasMulcair_day_df, aes(wday, m_week)) + 
      geom_tile(data = ThomasMulcair_shadow_df, aes(wday, m_week), 
                fill = "grey92", color = "white") +
      geom_tile(data = ThomasMulcair_day_df, aes_string(fill = input$var_fill), color = "white") + 
      facet_grid(.~month, drop=TRUE) + 
      labs(x = "", y = "Week of the Month") +
      scale_x_discrete(drop=FALSE, limits = rev(levels(wday))) +
      scale_y_discrete(drop=FALSE, limits = rev(levels(ThomasMulcair_day_df$m_week))) +
      theme_bw() + 
      scale_fill_gradient(high="#FF9900",low="#FFF5E6") +
      theme(panel.grid.major = element_blank(),
            panel.background = element_rect(fill = "#FAF0E6"),
            strip.background = element_rect(fill = "#FF9900"),
            strip.text = element_text(color="white", face = "bold", size = rel(1.3)),
            legend.position = "bottom")
  })
  
  # Liberal
  output$lib_calender <- renderPlot({
    # create_cal_df(cdnpoli_tweet()$lib_df)
    ggplot(JustinTrudeau_day_df, aes(wday, m_week)) + 
      geom_tile(data =  JustinTrudeau_shadow_df, aes(wday, m_week),
                fill = "grey92", color = "white") +
      geom_tile(data = JustinTrudeau_day_df, aes_string(fill = input$var_fill), color = "white") + 
      facet_grid(.~month, drop=TRUE) + 
      labs(x = "", y = "Week of the Month") +
      scale_x_discrete(drop=FALSE, limits = rev(levels(wday))) +
      scale_y_discrete(drop=FALSE, limits = rev(levels(JustinTrudeau_day_df$m_week))) +
      theme_bw() + 
      scale_fill_gradient(high = "#FF6347",low = "#FFE4E1") +
      theme(panel.grid.major = element_blank(),
            panel.background = element_rect(fill = "#FAF0E6"),
            strip.background = element_rect(fill = "#FF6347"),
            strip.text = element_text(color="white", face = "bold", size = rel(1.3)),
            legend.position ="bottom")
  })
  
  # Green
  output$green_calender <- renderPlot({
    # create_cal_df(cdnpoli_tweet()$green_df)
    ggplot(ElizabethMay_day_df, aes(wday, m_week)) + 
      geom_tile(data = ElizabethMay_shadow_df, aes(wday, m_week),
                fill = "grey92", color = "white") +
      geom_tile(data = ElizabethMay_day_df, aes_string(fill = input$var_fill), color = "white") + 
      facet_grid(.~month, drop=TRUE) + 
      labs(x = "", y = "Week of the Month") +
      scale_x_discrete(drop=FALSE, limits = rev(levels(wday))) +
      scale_y_discrete(drop=FALSE, limits = rev(levels(ElizabethMay_day_df$m_week))) +
      theme_bw() + 
      scale_fill_gradient(high = "#1F5C1F",low = "#EBF5EB") +
      theme(panel.grid.major = element_blank(),
            panel.background = element_rect(fill = "#FAF0E6"),
            strip.background = element_rect(fill = "#1F5C1F"),
            strip.text = element_text(color="white", face = "bold", size = rel(1.3)),
            legend.position = "bottom")
  })
  
  # Bloc
  output$bloc_calender <- renderPlot({
    # create_cal_df(cdnpoli_tweet()$bloc_df)
    ggplot(GillesDuceppe_day_df, aes(wday, m_week)) + 
      geom_tile(data =  GillesDuceppe_shadow_df, aes(wday, m_week),
                fill = "grey92", color = "white") +
      geom_tile(data = GillesDuceppe_df, aes_string(fill = input$var_fill), color = "white") + 
      facet_grid(.~month, drop=TRUE) + 
      labs(x = "", y = "Week of the Month") +
      scale_x_discrete(drop=FALSE, limits = rev(levels(wday))) +
      scale_y_discrete(drop=FALSE, limits = rev(levels(GillesDuceppe_day_df$m_week))) +
      theme_bw() + 
      scale_fill_gradient(high = "#003366",low = "#00CCFF") +
      theme(panel.grid.major = element_blank(),
            panel.background = element_rect(fill = "#FAF0E6"),
            strip.background = element_rect(fill = "#003366"),
            strip.text = element_text(color="white", face = "bold", size = rel(1.3)),
            legend.position = "bottom")
  })
  
# ---- Comparison and Common Word Cloud ----
  
  # Comparision Cloud
  output$plot_comparecloud <- renderPlot({
    withProgress({
      setProgress(message = "Drawing Comparison Word Cloud")})
    input$go2
    comparison.cloud(cdnpoli_tweet()$corpous_for_cloud, random.order = FALSE, 
                     colors=c("blue","red", "orange","green","darkblue"),
                     title.size = 1.5, max.words = 100)
  })
  
  # Common Cloud
  output$plot_commoncloud <- renderPlot({
    withProgress({
      setProgress(message = "Drawing Common Word Cloud")})
    input$go2
  commonality.cloud(cdnpoli_tweet()$corpous_for_cloud, random.order = FALSE, 
                    colors = brewer.pal(8, "Dark2"),
                    title.size = 1.5, max.words = 100)
  })
  
# ---- Individual Word Cloud ----
  
  # Convservative Cloud
  output$plot_wordcloud_con <- renderPlot({
    input$go
    input$r_twt_yn
    withProgress({
      setProgress(message = "Drawing Word Cloud")})
    tdm <- TermDocumentMatrix(pmharper_text_corpus)  
    matrix <- as.matrix(tdm)
    v <- sort(rowSums(matrix),decreasing = TRUE)
    d <- data.frame(word = names(v),freq = v)
    wordcloud(d$word, d$freq, colors = brewer.pal(8, "Set1"))
  })
  
  # Liberal Cloud
  output$plot_wordcloud_lib <- renderPlot({
    input$go
    input$r_twt_yn
    withProgress({
      setProgress(message = "Drawing Word Cloud")})
    tdm <- TermDocumentMatrix(JustinTrudeau_text_corpus)  
    matrix <- as.matrix(tdm)
    v <- sort(rowSums(matrix),decreasing = TRUE)
    d <- data.frame(word = names(v),freq = v)
    wordcloud(d$word, d$freq, colors = brewer.pal(8, "Set1"))
  })
  
  # NDP Cloud
  output$plot_wordcloud_ndp <- renderPlot({
    input$go
    input$r_twt_yn
    withProgress({
      setProgress(message = "Drawing Word Cloud")})
    tdm <- TermDocumentMatrix(ThomasMulcair_text_corpus)  
    matrix <- as.matrix(tdm)
    v <- sort(rowSums(matrix),decreasing = TRUE)
    d <- data.frame(word = names(v),freq = v)
    wordcloud(d$word, d$freq, colors = brewer.pal(8, "Set1"))
  })
  
  # Green Cloud
  output$plot_wordcloud_green <- renderPlot({
    input$go
    input$r_twt_yn
    withProgress({
      setProgress(message = "Drawing Word Cloud")})
    tdm <- TermDocumentMatrix(ElizabethMay_text_corpus)  
    matrix <- as.matrix(tdm)
    v <- sort(rowSums(matrix),decreasing = TRUE)
    d <- data.frame(word = names(v),freq = v)
    wordcloud(d$word, d$freq, colors = brewer.pal(8, "Set1"))
  })
  
  # Bloc Cloud
  output$plot_wordcloud_bloc <- renderPlot({
    input$go
    input$r_twt_yn
    withProgress({
      setProgress(message = "Drawing Word Cloud")})
    tdm <- TermDocumentMatrix(GillesDuceppe_text_corpus)  
    matrix <- as.matrix(tdm)
    v <- sort(rowSums(matrix),decreasing = TRUE)
    d <- data.frame(word = names(v),freq = v)
    wordcloud(d$word, d$freq, colors = brewer.pal(8, "Set1"))
  })

# ----- Profile Comparision -----

  # Treemap via modified ggtify fn
  output$treemap <- renderPlot({
    # Grouping and label needs to be factor for treemap to work
    cdn_party_user_profile$name <- factor(cdn_party_user_profile$name)
    cdn_party_user_profile$screenName <- factor(cdn_party_user_profile$screenName)
    cdn_party_user_profile$location <- factor(cdn_party_user_profile$location)
    cdn_party_user_profile$lang <- factor(cdn_party_user_profile$lang)
    
    # http://www.kevjohnson.org/making-maps-in-r/
    treemapify(cdn_party_user_profile, area = "followersCount", 
               fill = "friendsCount", label = "name") %>% ggtify() + 
      scale_fill_distiller(name = "Friends Count", palette = "Blues", breaks = pretty_breaks(5)) + 
      guides(fill = guide_legend(reverse = TRUE))
  })
  
  # Bar graph
  output$party_metric <- renderPlot({
    
    # Grouping and label needs to be factor for treemap to work
    cdn_party_user_profile$name <- factor(cdn_party_user_profile$name)
    cdn_party_user_profile$screenName <- factor(cdn_party_user_profile$screenName)
    cdn_party_user_profile$location <- factor(cdn_party_user_profile$location)
    cdn_party_user_profile$lang <- factor(cdn_party_user_profile$lang)
    
    # Melt (gather) the data frame for bar graph
    data <- gather(cdn_party_user_profile, metrics, values, statusesCount:friendsCount)
    
    ggplot(filter(data, metrics != "followersCount" & metrics != "statusesCount"), 
           aes(x = reorder(name, values), y = values, colour = metrics, fill = metrics)) + 
      geom_bar(position = "dodge", stat = "identity") + coord_flip() + 
      geom_text(aes(label = values), position = position_dodge(.9), hjust=-.2) + 
      theme_fivethirtyeight() + ylim(0, 8000) + 
      scale_fill_discrete(name = "Metric", label = c("Favorites", "Friends")) + 
      scale_color_discrete(name = "Metric", label = c("Favorites", "Friends"))
  })
  
#   output$cdn_party_hour_wday <- renderPlot({
#     input$go
#     ggplot(user_df, aes(hour, fill=wday)) + geom_density(alpha = 1/4, adjust=.2, color=NA) + theme_fivethirtyeight() +
#       scale_x_continuous(breaks=seq(0, 24, by = 4)) + 
#       scale_y_continuous(name = "", breaks = NULL)
#   })
  
# ---- Make Tweet by Hour Plot ----
  
  # Hour by weekday density plot
  output$party_by_hour <- renderPlot({
    
    input$go
    
    # Combine the dataframe of all party
    pmharper_df$party <- "Convservative"
    JustinTrudeau_df$party <- "Liberal"
    ThomasMulcair_df$party <- "NDP"
    ElizabethMay_df$party <- "Green"
    GillesDuceppe_df$party <- "Bloc Québécois"

    party_day_df <- rbind(pmharper_df, JustinTrudeau_df, ThomasMulcair_df,
                          ElizabethMay_df, GillesDuceppe_df)
    
    ggplot(party_day_df, aes(hour, fill = party)) + 
      geom_density(alpha = 1/4, adjust = .2, color = NA) + theme_fivethirtyeight() +
      scale_x_continuous(breaks = seq(0, 24, by = 4)) + # Make prettier break
      scale_y_continuous(name = "", breaks = NULL)
  })
   
  
# ----- Lets Map Followers -----
  
 ff_coded <- eventReactive(input$map, {
   withProgress({
     setProgress(message = "Mapping Followers!")})
   handle <- paste0(gsub("@", "", input$handle))
    ff_df <- twitterMap(handle, nMax = 1000) # Lets try 1000!
    ff_df
  })
  
  output$check_ff <- renderDataTable({
    ff_coded()
  })
  
  output$user_follower_map <- renderLeaflet({
    user_f_coded_df <- ff_coded()
    leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(user_f_coded_df$long, user_f_coded_df$lat, 
               popup = user_f_coded_df$location)
  })

}


