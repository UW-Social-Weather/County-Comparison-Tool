

# server portion of R shiny app -------------------------------------------


server <- function(input, output, session) {
  print(input)
  year <- reactive({
    merged_data_for_vacii_sdi[merged_data_for_vacii_sdi$year == input$year,]
  })
  
  output$yeartitle <- renderText({ 
    paste0("Year ", input$year)
  })
  
  observeEvent(year(),{
    choices = sort(unique(merged_data_for_vacii_sdi$year))
    updateSliderInput(session,'year', value=unique(year()$year),
                      min = min(as.numeric(choices)), max = max(as.numeric(choices)), step = 1)
  })
  
  index_year_input <- reactive({
    req(input$index_year_input)
    index_results[index_results$year == input$index_year_input,]
  })
  
  #Vaccination and Disease Trend Tab
  #update select vaccine
  observeEvent(preventable_vac_trend,{
    vacdata = filter(preventable_vac_trend, gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
    disdata = filter(merged_data_for_vac_dis, gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
    merged = dplyr::inner_join(vacdata,disdata,"vaccine_name","vaccine_name")
    choices = sort(unique(merged$vaccine_name))
    updateSelectInput(session,'vaccinations', choices = choices)
  })
  
  dis_data_for_selected_vac <- reactive({
    filter(merged_data_for_vac_dis, merged_data_for_vac_dis$vaccine_name==input$vaccinations)
  })
  
  selected_dis_vac_data <- reactive({
    return(
      list(
        selected_vac_data = filter(preventable_vac_trend, vaccine_trends$vaccine_name==input$vaccinations),
        dis_data_for_selected_vac = filter(merged_data_for_vac_dis, merged_data_for_vac_dis$vaccine_name==input$vaccinations)
      ))
  })
  
  sdi_group_present <- reactive({
    req(input$sdi_group_present)
    if (input$sdi_group_present == "all"){
      all_sdi_group <- year() 
    }
    else{
      filter(year(), sdi_group_present == input$sdi_group_present)
    }
  })
  
  observeEvent(sdi_group_present(),{
    updateSelectInput(session, 'region_table',choices = c("All",unique(sdi_group_present()$region)))
  })
  
  regionstable <- reactive({
    req(input$region_table)
    if (input$region_table =='All'){
      sdi_group_present()
    }
    else{
      filter(sdi_group_present(),region == input$region_table)
    }
  })
  
  sdi_group_present_comp <- reactive({
    req(input$sdi_group_present_comp)
    if (input$sdi_group_present_comp == "all"){
      all_sdi_group <- merged_data_for_vacii_sdi
    }
    else{
      filter(merged_data_for_vacii_sdi, sdi_group_present== input$sdi_group_present_comp)
    }
  })
  
  observeEvent(sdi_group_present_comp(),{
    updateSelectInput(session, 'region', selected = 'Western Sub-Saharan Africa', choices = sdi_group_present_comp()$region)
  })
  
  regionselected <- reactive({
    req(input$region)
    filter(sdi_group_present_comp(),region == input$region)
  })
  
  observeEvent(regionselected(),{
    updateSelectInput(session, 'my_multi', selected = 'Nigeria', choices = regionselected()$location)
  })
  
  
  output$table = DT::renderDataTable({
    sdi_rank_table<-regionstable()[,c("location","result","sdi_group_present")]
    # print("sorting")
    # print(sdi_rank_table)
    sdi_rank_table$rank <- NA
    sdi_rank_table$rank = dense_rank(desc(sdi_rank_table$result))
    sdi_rank_table <- sdi_rank_table[,c("rank","location","result","sdi_group_present")]
    sdi_rank_table$result = round(sdi_rank_table$result,4)
    colnames(sdi_rank_table) <- c('Rank','Location','VIP Index', "2019 SDI Group")
    sdi_rank_table<-sdi_rank_table[order(sdi_rank_table$Rank),]
    true_false_formatter <-
      formatter("span",
                style = x ~ formattable::style(
                  font.weight = "bold",
                  color = ifelse(x == "high", "forestgreen", ifelse(x == "low", "red", "black"))
                ))
    
    formattable(
      sdi_rank_table,
      list(
        ## a coloured bar with length proportional to value
        'Vaccination Improvement Index' = color_tile("white", "#569eca"),
        #'SDI' = color_tile("white", "pink"),
        ## use custom formatter for TRUE/FALSE values
        '2019 SDI Group' = true_false_formatter
      )
    ) %>%
      as.datatable(rownames = FALSE, 
                   selection = list(mode = 'multiple',target="cell", selected = matrix(c(0, 1), nrow = 1,ncol = 2)), 
                   options = list(paging = FALSE,
                                  scrollY = '400px', 
                                  scrollY=TRUE, 
                                  autoWidth = FALSE,
                                  ordering = FALSE,
                                  #dom = 'Bfrtip',
                                  pageLength=1000)
      )
  })
  
  output$index_trend_plot_multi <- renderPlotly({
    if (input$sdi_group_present == 'medium'){
      country = 'Uruguay'
    }
    else if (input$sdi_group_present == 'low'){
      country = 'Eswatini'
    }
    else{
      country = 'United States of America'
    }
    index_trend_data <- filter(index_results,location == country)
    fig_a <- plot_ly(index_trend_data, x = ~year)
    fig_a <- fig_a  %>% add_trace(y=~result,type='scatter', name = country, mode = 'lines', line = list(color = 'rgba(49,130,189, 1)',width=2))
    fig_a <- fig_a %>% 
      layout( autosize = T,
              title = paste0("Time Series of VIP Index"), 
              showlegend = TRUE,
              xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
              yaxis = list(title = "VIP Index",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
    fig_a
  })
  
  observeEvent(regionstable(),{
    # print("map data")
    map_data <- regionstable()
    # print(map_data)
    
    indicatorsdata <- reactive({
      req(input$indicators)
      indicators <-as.data.frame.matrix(map_data[,-c("gbd_location_id","iso_num_code")])
    })
    
    observeEvent(indicatorsdata(),{
      output$indicator_map <- renderPlotly({ #### Improvement Indicator
        height  = 1500
        units="px"
        
        # light grey boundaries
        l <- list(color = toRGB("white"), width = 0.5)
        
        # specify map projection/options
        g <- list(
          showframe = FALSE,
          showcoastlines = FALSE,showland = TRUE,showcountries = TRUE,
          resolution = 150,
          countrycolor = toRGB("white"),
          landcolor = toRGB("grey85"),
          projection = list(scale=1.2))
        
        fig <- plot_ly(indicatorsdata())
        if (input$indicators == "Socio-demographic Index"){
          fig <- fig %>% 
            add_trace(
              z = ~sdi, color = ~sdi, type = 'choropleth', locations = ~iso_code, colors="Purples", 
              text = ~paste0(location),
              marker = list(line = l)) %>%
              colorbar(title = paste0("Socio-demographic \n Index"))
        }
        # resultindex-1
        #else if (input$indicators == "Development Assistance Per Total Health Spending Categorical"){
        # fig <- fig %>% 
        #  add_trace(
        #   z = ~dah_per_the_mean_cat, color = ~dah_per_the_mean_cat, type = 'choropleth', locations = ~iso_code, colors="Purples", 
        #  text = ~paste0(location),
        # marker = list(line = l))
        #}
        else if (input$indicators == "Eligibility to Receive DAH"){
          colfunc <- colorRampPalette(c("#A2A2A1FF", "#4b2e83"))
          fig <- fig %>% 
            add_trace(
              z = ~as.numeric(indicatorsdata()$dah_eligible), type = 'choropleth',locations = ~iso_code, colors=colfunc(2),
              text = ~paste0(location),
              marker = list(line = l))%>% 
              colorbar(title = paste0("Eligibility to Receive DAH",'<br>',"(1: True, 0: False)"),brks=c(0,1),labels=c("True",' ',"False"))
        }
        else if (input$indicators == "Total Health Spending per Person"){
          colfunc <- colorRampPalette(c("#C7C5E7", "#4b2e83"))
          fig <- fig %>% 
            add_trace(
              z = ~the_per_cap_mean, color = ~the_per_cap_mean, type = 'choropleth', locations = ~iso_code, colors=colfunc(10),
              text = ~paste0(location),
              marker = list(line = l)) %>%
              colorbar(title = paste0("Mean Spending \n per Person"))
        }
        else if (input$indicators == "Government Health Spending per Total Health Spending"){
          fig <- fig %>% 
            add_trace(
              z = ~ghes_per_the_mean, color = ~ghes_per_the_mean, type = 'choropleth', locations = ~iso_code,  colors="Purples", 
              text = ~paste0(location),
              marker = list(line = l)) %>%
              colorbar(title = paste0("Mean Government \n Spending"))
        }
        #add resultindex2
        else if (input$indicators == "Development Assistance per Person"){
          colfunc <- colorRampPalette(c("#C7C5E7", "#4b2e83"))
          fig <- fig %>% 
            add_trace(
              z = ~dah_per_cap_ppp_mean, color = ~dah_per_cap_ppp_mean, type = 'choropleth', locations = ~iso_code, colors=colfunc(10),
              text = ~paste0(location),
              marker = list(line = l)) %>%
              colorbar(title = paste0("Mean Development \n Assistance per \n Person"))
        }
        
        else if (input$indicators == "HAQI"){
          fig <- fig %>% 
            add_trace(
              z = ~haqi, color = ~haqi, type = 'choropleth', locations = ~iso_code, colors="Purples", 
              text = ~paste0(location),
              marker = list(line = l)) %>%
                colorbar(title = paste0("HAQI"))
        }
        else if (input$indicators == "Corruption Perception Index"){
          fig <- fig %>% 
            add_trace(
              z = ~cpi, color = ~cpi, type = 'choropleth', locations = ~iso_code, colors="Purples",
              text = ~paste0(location),
              marker = list(line = l)) %>%
            colorbar(title = paste0("Corruption \n Perception Index"))
        }
        else if (input$indicators == "Skilled Attendants at Birth"){
          fig <- fig %>% 
            add_trace(
              z = ~perc_skill_attend, color = ~perc_skill_attend, type = 'choropleth', locations = ~iso_code, colors="Purples", 
              text = ~paste0(location),
              marker = list(line = l)) %>%
              colorbar(title = paste0("Skilled \n Attendants at \n Birth"))
          
        }
        else if (input$indicators == "Immigrant Population (%)"){
          fig <- fig %>% 
            add_trace(
              z = ~imm_pop_perc, color = ~imm_pop_perc, type = 'choropleth', locations = ~iso_code, colors="Purples", 
              text = ~paste0(location),
              marker = list(line = l)) %>%
              colorbar(title = paste0("Immigrant \n Population (%)"))
        }
        else if (input$indicators == "Urbanicity (%)"){
          fig <- fig %>% 
            add_trace(
              z = ~perc_urban, color = ~perc_urban, type = 'choropleth', locations = ~iso_code, colors="Purples", 
              text = ~paste0(location),
              marker = list(line = l)) %>%
              colorbar(title = paste0("Urbanicity (%)"))
        }
        
        fig <- fig%>% #### Improvement Indicator
          layout(
            autosize = T,
            title = paste0(input$year," Global VIP Index Component Mapper"),
            mapbox=list(
              style="carto-positron",
              center = list(lon = -90, lat = 80)),
            geo = g)
      })
    })
    
    map_data$hover <- with(map_data, paste(location, '<br>','<br>',
                                           ###########################add 2
                                           "Eligibility to Receive DAH: " ,dah_eligible,'<br>',
                                           #"Development Assistance Per Total Health Spending Categorical: ",round(dah_per_the_mean_cat,3),'<br>',
                                           ##########################
                                           
                                           "Socio-demographic Index: ", round(sdi,3),'<br>',
                                           "Total Health Spending per Personn: ",round(the_per_cap_mean,3),'<br>',
                                           "Government Health Spending per Total Health Spending: ",round(ghes_per_the_mean,3),'<br>',
                                           
                                           #####################
                                           "Development Assistance per Person: ", round(dah_per_cap_ppp_mean,3),'<br>',
                                           ######################
                                           
                                           "HAQI: ",round(haqi,3),'<br>',
                                           "Corruption Perception Index:", round(cpi,3),'<br>',
                                           "Skilled Attendants at Birth: ",round(perc_skill_attend,3),'<br>',
                                           "Immigrant Population (%): ",round(imm_pop_perc,3),'<br>',
                                           "Urbanicity (%)",round(perc_urban,3)
                                           #"Agreement Vaccines are Safe",round(mean_agree_vac_safe,3),'<br>',
                                           #"Agreement Vaccines are Important",round(mean_agree_vac_important,3),'<br>',
                                           #"Agreement Vaccines are Effective: ",round(mean_agree_vac_effective,3)
    ))
    output$index_map <- renderPlotly({
      height  = 1500
      units="px"
      
      # light grey boundaries
      l <- list(color = toRGB("white"), width = 0.5)
      
      # specify map projection/options
      g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,showland = TRUE,showcountries = TRUE,
        resolution = 150,
        countrycolor = toRGB("white"),
        landcolor = toRGB("grey85"),
        projection = list(scale=1.2))
      
      
      fig <- plot_ly(map_data)
      fig <- fig %>% 
        add_trace(
          z = ~result, color = ~result, type = 'choropleth',
          text = ~hover, locations = ~iso_code, colors="Blues", 
          marker = list(line = l))%>%
        colorbar(title = 'VIP Index')%>% 
        layout(
          autosize = T,
          title = paste0(input$year," Global VIP Index Mapper"),
          mapbox=list(
            style="carto-positron",
            center = list(lon = -90, lat = 80)),
          geo = g)
    })
    ##############test##########t##########t##########t##########t
    reportindextrendplot<-reactive(
      fig_a <- plot_ly(index_results %>%  filter(location %in% input$my_multi), x = ~year) %>% 
        add_trace(y=~result,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2) %>% 
        layout( autosize = T,
                title = paste0("Time Series of Vaccine Improvement Index"), 
                showlegend = TRUE,
                xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = "Vaccine Improvement Index",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
    )
    
    ##############test##########t##########t##########t##########t
    output$index_trend_plot_com <- renderPlotly({
      reportindextrendplot()
    })
    
    output$indicator_trend_plot_multi <- renderPlotly({
      if (input$sdi_group_present == 'medium'){
        country = 'Uruguay'
      }
      else if (input$sdi_group_present == 'low'){
        country = 'Eswatini'
      }
      else{
        country = 'United States of America'
      }
      
      indicator_trend_data <- filter(index_results,location == country)
      fig_a <- plot_ly(indicator_trend_data, x = ~year)
      
      if (input$indicators == "Socio-demographic Index"){
        titles = paste0("Time Series of SDI")
        ytitles = "Socio-demographic Index"
        fig_a <- fig_a  %>% add_trace(y=~sdi,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      else if (input$indicators == "Eligibility to Receive DAH"){
        titles = paste0("Time Series of Eligibility to Receive DAH")
        ytitles = "Eligibility to Receive DAH"
        fig_a <- fig_a  %>% add_trace(y=~dah_eligible,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      else if (input$indicators == "Total Health Spending per Person"){
        titles=paste0("Time Series of Total Health Spending per Person")
        ytitles = "Total Health Spending per Person"
        fig_a <- fig_a  %>% add_trace(y=~the_per_cap_mean,type='scatter', name =~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      else if (input$indicators == "Government Health Spending per Total Health Spending"){
        titles=paste0("Time Series of Government Health Spending per Total Health Spending")
        ytitles = "Government Health Spending \n per Total Health Spending"
        fig_a <- fig_a  %>% add_trace(y=~ghes_per_the_mean,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        
      }
      else if (input$indicators == "Development Assistance per Person"){
        titles=paste0("Time Series of Development Assistance per Person")
        ytitles = "Development Assistance per Person"
        fig_a <- fig_a  %>% add_trace(y=~dah_per_cap_ppp_mean,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      else if (input$indicators == "HAQI"){
        titles=paste0("Time Series of HAQI")
        ytitles = "HAQI"
        fig_a <- fig_a  %>% add_trace(y=~haqi,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        
      }
      else if (input$indicators == "Corruption Perception Index"){
        titles=paste0("Time Series of Corruption Perception Index")
        ytitles = "Corruption Perception Index"
        fig_a <- fig_a  %>% add_trace(y=~cpi,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      else if (input$indicators == "Skilled Attendants at Birth"){
        titles=paste0("Time Series of Skilled Attendants at Birth")
        ytitles = "Skilled Attendants at Birth"
        fig_a <- fig_a  %>% add_trace(y=~perc_skill_attend,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      else if (input$indicators == "Immigrant Population (%)"){
        titles=paste0("Time Series of Immigrant Population (%)")
        ytitles = "Immigrant Population (%)"
        fig_a <- fig_a  %>% add_trace(y=~imm_pop_perc,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      else if(input$indicators == "Urbanicity (%)"){
        titles=paste0("Time Series of Urbanicity (%)")
        ytitles = "Urbanicity (%)"
        fig_a <- fig_a  %>% add_trace(y=~perc_urban,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      fig_a <- fig_a %>% 
        layout( autosize = T,
                title = titles, 
                showlegend = TRUE,
                xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = ytitles,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
      fig_a
    })
    
    
    output$indextable = DT::renderDataTable({
      # print("table")
      index_rank_table<-regionstable()[,-c("year","gbd_location_id","iso_code","iso_num_code")]
      index_rank_table$rank <- NA
      index_rank_table$rank = dense_rank(desc(index_rank_table$result))
      #index_rank_table <- index_rank_table[,c("rank","location_name","sdi","sdi_group_present")]
      index_rank_table<-index_rank_table[order(index_rank_table$rank),]
      # print(colnames(index_rank_table))
      
      #colnames(index_rank_table) = c("Location", "SDI","Development Assistance Per Total Health Spending Categorical","Total Health Spending per Person",
      #                     "Government Health Spending per Total Health Spending","HAQI","Corruption Perception Index","Skilled Attendants at Birth","Immigrant Population (%)",
      #                     "Urbanicity (%)","Agreement Vaccines are Safe","Agreement Vaccines are Important","Agreement Vaccines are Effective","Improvement Index","location_id","level",
      #                     "SDI Group Present","Rank")
      # colnames(index_rank_table) = c("Location", "Region","Eligibility to receie DAH","Socio-demographic Index","Total Health Spending per Person",
      #                                "Government Health Spending per Total Health Spending","Development Assistance Per Person","HAQI","Corruption Perception Index","Skilled Attendants at Birth","Immigrant Population (%)",
      #                                "Urbanicity (%)","Improvement Index","location_id","level","2019 SDI Group","Rank")
      index_rank_table <-  rename(index_rank_table, 
                                  "Location" = "location", 
                                  "Region" = "region", 
                                  "Eligibility to Receive DAH" = "dah_eligible",
                                  "Socio-demographic Index" = "sdi", 
                                  "Total Health Spending per Person" = "the_per_cap_mean",
                                  "Government Health Spending per Total Health Spending" = "ghes_per_the_mean",
                                  "Development Assistance per Person" = "dah_per_cap_ppp_mean",
                                  "HAQI" = "haqi",
                                  "Corruption Perception Index" = "cpi", 
                                  "Skilled Attendants at Birth" = "perc_skill_attend", 
                                  "Immigrant Population (%)" = "imm_pop_perc",
                                  "Urbanicity (%)" = "perc_urban", 
                                  "Improvement Index" = "result", 
                                  "location_id" = "location_id", 
                                  "level" = "level", 
                                  "2019 SDI Group" = "sdi_group_present", 
                                  "Rank" = "rank",
                                  "Agree Vaccines are Safe" = "mean_agree_vac_safe",
                                  "Agree Vaccines are Important" = "mean_agree_vac_important",
                                  "Agree Vaccines are Effective" = "mean_agree_vac_effective",
                                  "Government Trust" = "gov_trust"
                                  )
      # table_column_order <- c("result", "location", "cpi", "gbd_location_id", "iso_code", "iso_num_code")
      # index_rank_table <- index_rank_table[,c(17,1,13,3,4,5,6,7,8,9,10,11,12)]
      index_rank_table <- index_rank_table[, c("Rank", "Location", "Improvement Index", "Eligibility to Receive DAH", "Socio-demographic Index",
                                               "Total Health Spending per Person", "Government Health Spending per Total Health Spending",
                                               "Development Assistance per Person", "HAQI", "Corruption Perception Index", "Skilled Attendants at Birth",
                                               "Immigrant Population (%)", "Urbanicity (%)", "Agree Vaccines are Safe", "Agree Vaccines are Important", "Agree Vaccines are Effective")]     
      # print(colnames(index_rank_table))
      customGreen0 = "#DeF7E9"
      customGreen = "#71CA97"
      
      true_false_formatter <-
        formatter("span",
                  style = x ~ formattable::style(
                    font.weight = "bold",
                    color = ifelse(x == TRUE, "forestgreen", ifelse(x == FALSE, "red", "black"))
                  ))
      
      formattable(
        index_rank_table,
        list(
          ## a coloured bar with length proportional to value
          'Improvement Index' = color_tile("white", "#569eca"),
          "Socio-demographic Index" = color_tile("white", "pink"),
          "Eligibility to Receive DAH" = true_false_formatter,
          #"Development Assistance Per Total Health Spending Categorical" = color_tile(customGreen0, customGreen),
          "Total Health Spending per Person"= color_tile(customGreen0, customGreen),
          "Government Health Spending per Total Health Spending"= color_tile(customGreen0, customGreen),
          "HAQI"= color_tile(customGreen0, customGreen),
          "Development Assistance per Person"=color_tile(customGreen0, customGreen),
          "Corruption Perception Index"= color_tile(customGreen0, customGreen),
          "Skilled Attendants at Birth"= color_tile(customGreen0, customGreen),
          "Immigrant Population (%)"= color_tile(customGreen0, customGreen),
          "Urbanicity (%)"= color_tile(customGreen0, customGreen),
          "Agree Vaccines are Safe"= color_tile(customGreen0, customGreen),
          "Agree Vaccines are Important"= color_tile(customGreen0, customGreen),
          "Agree Vaccines are Effective"= color_tile(customGreen0, customGreen)
        )
      )%>%
        as.datatable(rownames = FALSE, 
                     options = list(paging = FALSE,
                                    scrollY = '520px', 
                                    scrollY=TRUE, 
                                    scrollX=TRUE, 
                                    #searching = FALSE,
                                    autoWidth = FALSE,
                                    ordering = FALSE,
                                    pageLength=1000)
        )
    })
  })
  
  
  # output$indextable = DT::renderDataTable({
  #   index_rank_table<-index_year_input()[,-c("year","gbd_location_id","iso_code","iso_num_code")]
  #   index_rank_table$rank <- NA
  #   index_rank_table$rank = dense_rank(desc(index_rank_table$result))
  #   index_rank_table<-index_rank_table[order(index_rank_table$rank),]
  #   # print("Index_ranktable")
  #   # print(index_rank_table)
  #   
  #   colnames(index_rank_table) = c("Location", "Region","Eligibility to Receive DAH","Socio-demographic Index","Total Health Spending per Person",
  #                                  "Government Health Spending per Total Health Spending","Development Assistance per Person","HAQI","Corruption Perception Index","Skilled Attendants at Birth","Immigrant Population (%)",
  #                                  "Urbanicity (%)","Improvement Index","location_id","level",
  #                                  "2019 SDI Group","Rank")
  #   index_rank_table <- index_rank_table[,c(17,1,13,2,3,4,5,6,7,8,9,10,11)]
  #   
  #   customGreen0 = "#DeF7E9"
  #   customGreen = "#71CA97"
  #   
  #   true_false_formatter <-
  #     formatter("span",
  #               style = x ~ formattable::style(
  #                 font.weight = "bold",
  #                 color = ifelse(x == TRUE, "forestgreen", ifelse(x == FALSE, "red", "black"))
  #               ))
  #   
  #   formattable(
  #     index_rank_table,
  #     list(
  #       ## a coloured bar with length proportional to value
  #       'Improvement Index' = color_tile("white", "#569eca"),
  #       "Socio-demographic Index" = color_tile("white", "pink"),
  #       "Eligibility to receive DAH" = true_false_formatter,
  #       "Total Health Spending per Person"= color_tile(customGreen0, customGreen),
  #       "Government Health Spending per Total Health Spending"= color_tile(customGreen0, customGreen),
  #       "HAQI"= color_tile(customGreen0, customGreen),
  #       "Development Assistance per Person"=color_tile(customGreen0, customGreen),
  #       "Corruption Perception Index"= color_tile(customGreen0, customGreen),
  #       "Skilled Attendants at Birth"= color_tile(customGreen0, customGreen),
  #       "Immigrant Population (%)"= color_tile(customGreen0, customGreen),
  #       "Urbanicity (%)"= color_tile(customGreen0, customGreen)
  #     )
  #   ) %>%
  #     as.datatable(rownames = FALSE, 
  #                  options = list(paging = FALSE,
  #                                 scrollY = '500px', 
  #                                 scrollY=TRUE, 
  #                                 scrollX=TRUE, 
  #                                 #searching = FALSE,
  #                                 autoWidth = FALSE,
  #                                 ordering = FALSE,
  #                                 pageLength=1000)
  #     )
  # })
  
  output$content_vac <- renderText("United States of America")
  output$content_dis <- renderText("United States of America")
  output$content_vac_dis <- renderText("United States of America")
  
  output$all_vaccine_plot <- renderPlotly({
    vac_plotdata <- filter(vaccine_trends,gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
    if (input$vaccine_plot == "line_trend"){
      fig_a <- plot_ly(vac_plotdata, x = ~year_id,y=~prop_val, color = ~vaccine_name)%>%
        add_lines()
      
      fig_a <- fig_a %>% 
        layout(autosize = T,
               title ="Time Series of Vaccination Coverage",  showlegend = T,
               xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(title = "Vaccination Coverage (%)",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
      fig_a
      
    }
    else{
      single_year_vac_plotdata <- filter(vac_plotdata,year_id == input$year)
      fig1 <- plot_ly(x = ~single_year_vac_plotdata$prop_val, y = ~reorder(single_year_vac_plotdata$vaccine_name, single_year_vac_plotdata$prop_val), name = single_year_vac_plotdata$vaccine_name,
                      type = 'bar', orientation = 'h',
                      color = single_year_vac_plotdata$vaccine_name)
      #marker = list(color = 'rgba(50, 171, 96, 0.6)',
      #line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) 
      fig1 <- fig1 %>% layout( autosize = T,
                               title = paste0("Vaccination Coverage in ", input$year),
                               yaxis = list(title = "Vaccine",showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                               xaxis = list(title = "Vaccination Coverage (%)", zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
      fig1 <- fig1 %>% add_annotations(xref = 'x1', yref = 'y',
                                       x = single_year_vac_plotdata$prop_val * 1 + 0.05,  y = single_year_vac_plotdata$vaccine_name,
                                       text = paste(round(single_year_vac_plotdata$prop_val*100, 2), '%'),
                                       font = list(family = 'Arial', size = 12, color = 'rgba(0, 0, 0, 1)'),
                                       showarrow = FALSE)
    }
  })
  
  output$nigeria_vaccine_plot <- renderPlotly({
    vac_plotdata <- filter(vaccine_trends,gsub(" ", "", location_name) == gsub(" ", "", "Nigeria"))
    fig_a <- plot_ly(vac_plotdata, x = ~year_id,y=~prop_val, color = ~vaccine_name)%>%
      add_lines()
    
    fig_a <- fig_a %>% 
      layout(autosize = T,
             title ="Time Series of Vaccination Coverage",  showlegend = T,
             xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
             yaxis = list(title = "Vaccination Coverage (%)",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
    fig_a
  })
  
  output$all_disease_plot <- renderPlotly({
    # print("input")
    # print(input$disease_estimate)
    disease_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
    # print("disease_plotdata")
    # print(disease_plotdata)
    if (input$disease_estimate == "number_val"){
      fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~round(deaths_number_val,8), color = ~cause_name)%>%
        add_lines()
      title = "Time Series of Deaths, Disease or Disability Number"
      y_title = "Number of Deaths \n in Population"
    }
    else if (input$disease_estimate == "percent_val"){
      fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~round(deaths_percent_val,8), color = ~cause_name)%>%
        add_lines()
      title = "Time Series of Deaths, Disease or Disability Percent"
      y_title="Particular Cause \n Death/All Causes Death"
    }
    else{
      fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~round(deaths_rate_val,8), color = ~cause_name)%>%
        add_lines()
      title = "Time Series of Deaths, Disease or Disability Rate"
      y_title="Deaths per 100,000 Population"
    }
    fig_dis <- fig_dis %>% 
      layout( autosize = T,
              title =title,  showlegend = T,
              xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
              yaxis = list(title =y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE,type= "log",tickfont = list(size = 1)))
    fig_dis
  })
  
  output$all_disability_plot <- renderPlotly({
    disability_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
    if (input$disease_estimate == "number_val"){
      fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_number_val, color = ~cause_name)%>%
        add_lines()
      title = "Time Series of Number of Years Lived in Less Than Ideal Health in Population"
      y_title = "Years Lived with \n Disability in Population "
    }
    else if (input$disease_estimate == "percent_val"){
      fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_percent_val, color = ~cause_name)%>%
        add_lines()
      title = "Time Series of Proportion of Years Lived in Less Than Ideal Health in Population"
      y_title="YLDs for Particular \n Cause/YLDs for All Causes"
    }
    else{
      fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_rate_val, color = ~cause_name)%>%
        add_lines()
      title = "Time Series of Years Lived in Less Than Ideal Health per 100,000 Population"
      y_title="YLDs per 100,000 Population"
    }
    fig_dis <- fig_dis %>% 
      layout( autosize = T,
              title =title,  showlegend = T,
              xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
              yaxis = list(title = y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE,type= "log",tickfont = list(size = 1)))
    fig_dis
  })
  
  output$nigeria_disability_plot <- renderPlotly({
    disability_plotdata <- filter(disease_trends,location_name == "Nigeria")
    fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_number_val, color = ~cause_name)%>%
      add_lines()
    title = "Time Series of Number of Years Lived in Less Than Ideal Health in Population"
    y_title = "Years Lived with \n Disability in Population"
    fig_dis <- fig_dis %>% 
      layout( autosize = T,
              title =title,  showlegend = T,
              xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
              yaxis = list(title =y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE,type= "log"))
    fig_dis
  })
  
  observeEvent(selected_dis_vac_data(),{
    output$selected_vac_dis_plot <- renderPlotly({
      selected_vac_plotdata <- filter(selected_dis_vac_data()$selected_vac_data,gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
      selected_dis_plotdata <- filter(selected_dis_vac_data()$dis_data_for_selected_vac,gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
      # print(selected_dis_plotdata)
      merged_selected_plotdata <- dplyr::left_join(selected_vac_plotdata,selected_dis_plotdata, "year_id", "year_id")
      # print("complete")
      print(merged_selected_plotdata)
      fig <- plot_ly()
      # Add traces
      fig <- plot_ly(merged_selected_plotdata)
      fig <- fig %>% add_trace(x= ~year_id, y = ~round(deaths_rate_val,8), type = 'scatter', mode = 'lines+makers', color = ~cause_name) 
      ay <- list(
        overlaying = "y",
        side = "right",
        title = "<b> Vaccine</b> Coverage (%)")
      
      fig <- fig %>% add_trace(x =  ~year_id, y = ~prop_val, type = 'scatter',name = ~vaccine_name.x,yaxis = "y2", mode = 'lines',line = list(color = 'rgba(49,130,189, 1)', width = 4)) 
      # Set figure title, x and y-axes titles
      fig <- fig %>% layout(
        autosize = T,
        title = list(text="Vaccine & Corresponding Disease Trend", x=0.25),
        xaxis = list(title="Year"),
        yaxis = list(title= "<b> Deaths</b> per 100,000 Population"),
        yaxis2 = ay,
        legend = list(x = 3000, y = 1.2)
      )%>%
        layout(xaxis = list(
          zerolinecolor = '#ffff',
          zerolinewidth = 2,
          gridcolor = 'ffff'),
          yaxis = list(
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff')
        )
      
      fig
    })
    
    
    output$selected_nigeria_vac_dis_plot <- renderPlotly({
      print("preventable_vac_trend")
      selected_vac_data = filter(preventable_vac_trend, vaccine_trends$vaccine_name=="MCV1")
      selected_vac_plotdata <- filter(selected_vac_data,location_name == "Nigeria")
      dis_data_for_selected_vac = filter(merged_data_for_vac_dis, merged_data_for_vac_dis$vaccine_name=="MCV1")
      selected_dis_plotdata <- filter(dis_data_for_selected_vac,location_name == "Nigeria")
      merged_selected_plotdata <- dplyr::left_join(selected_vac_plotdata,selected_dis_plotdata, "year_id", "year_id")
      fig <- plot_ly()
      # Add traces
      fig <- plot_ly(merged_selected_plotdata)
      fig <- fig %>% add_trace(x= ~year_id, y = ~round(deaths_rate_val,8), type = 'scatter', mode = 'lines+makers', color = ~cause_name) 
      ay <- list(
        overlaying = "y",
        side = "right",
        title = "<b> Vaccine</b> coverage (%)")
      
      fig <- fig %>% add_trace(x =  ~year_id, y = ~prop_val, type = 'scatter',name = ~vaccine_name.x,yaxis = "y2", mode = 'lines',line = list(color = 'rgba(49,130,189, 1)', width = 4)) 
      # Set figure title, x and y-axes titles
      fig <- fig %>% layout(
        autosize = T,
        title = list(text="Vaccine & Corresponding Disease Trend", x=0.25),
        xaxis = list(title="Year"),
        yaxis = list(title= "<b> Deaths</b> per 100,000 Population"),
        yaxis2 = ay,
        legend = list(x = 3000, y = 1.2)
      )%>%
        layout(xaxis = list(
          zerolinecolor = '#ffff',
          zerolinewidth = 2,
          gridcolor = 'ffff'),
          yaxis = list(
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff')
        )
      
      fig
    })
  })
  
  observe({
    if (input$t2 == "comp_index"){
      output$report_nig_title<- renderText("Comparison with other locations of similar geography or SDI")
      output$report_nig_body<- renderText("The construction of the Vaccine Improvement Index is based, in-part, on research on individual and socioeconomic factors associated with vaccine coverage in Nigeria. Prior research has found that mother’s age, education, and wealth as significantly related to immunization coverage after adjusting for other factors. In addition, the child’s birth order, family size, and place of delivery (home, public, or private facility) were related to vaccination coverage as well (1).")
    }
    else if(input$t2 == "report_vt"){
      output$report_nig_title<- renderText("Vaccination Trends")
      output$report_nig_body<- renderText("Between 2014 and 2019, Nigeria saw greater-than average improvements in seven routine vaccinations (out of 11 measured) (2). The progress demonstrated in this period contrasts to many years of stalled and even worsening vaccine coverage previously. Between 2005 and 2009, barriers to vaccination included structural issues including lack of security and armed conflict (3), supply chain and service delivery issues (4), and cultural and religious beliefs affecting vaccine hesitancy (5).")
    }
    else if(input$t2 == "report_md"){
      output$report_nig_title<- renderText("Mortality and Disability Trends")
      output$report_nig_body<- renderText("Several vaccine-preventable diseases present a large burden on the population in Nigeria. For instance, by 2013 Nigeria was one of three countries in the world with endemic polio; yet, Nigeria also struggled with declining polio vaccine coverage (6).")
    }
    else{
      output$report_nig_title<- renderText("Relationship between Vaccines and Corresponding Diseases")
      output$report_nig_body<- renderText("Greater attention to polio and other vaccine-preventable diseases led to both improved vaccination coverage and decreases in the number of deaths from diseases like measles. Revised national strategic plans for polio and routine immunizations (2013-2015) have also allowed the country to implement additional evidence-based interventions and plans for routine immunization (3–5,7).")
    }
  })
  
  list_all <- reactiveVal()
  observeEvent(input$table_cell_clicked,{
    
    info = input$table_cell_clicked
    if (is.null(info$value) || info$col != 1) return()
    updateTabsetPanel(session, 't1', selected = 'Vaccination Trends')
    if(input$t1 == "t_sdi"){
      updateTabsetPanel(session, 't1', selected = 'Vaccination Trends') 
    }
    else if(input$t1 == "t_vac"){
      updateTabsetPanel(session, 't1', selected = 'Disease Trends') 
    }
    
    vacdata = filter(preventable_vac_trend, gsub(" ", "", location_name) == gsub(" ", "", info$value))
    disdata = filter(merged_data_for_vac_dis, gsub(" ", "", location_name) == gsub(" ", "", info$value))
    merged = dplyr::inner_join(vacdata,disdata,"vaccine_name","vaccine_name")
    choices = sort(unique(merged$vaccine_name))
    updateSelectInput(session,'vaccinations', choices = choices)
    
    output$content_vac <- renderText(info$value)
    output$content_dis <- renderText(info$value)
    output$content_vac_dis <- renderText(info$value)
    
    if (gsub(" ", "", info$value) %in% list_all()){
      lst = list_all()
      list_all(lst[lst!=gsub(" ", "", info$value)])
    }
    else{
      list_all(append(list_all(), gsub(" ", "", info$value)))
    }
    
    output$index_trend_plot_multi <- renderPlotly({
      index_trend_data <- index_results %>% 
        filter(gsub(" ", "",location) %in% gsub(" ", "",list_all()))
      
      fig_a <- plot_ly(index_trend_data, x = ~year)
      fig_a <- fig_a  %>% add_trace(y=~result,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      fig_a <- fig_a %>% 
        layout( autosize = T,
                title = paste0("Time Series of Vaccine Improvement Index"), 
                showlegend = TRUE,
                xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = "Vaccine Improvement Index",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
      fig_a
    })
    
    output$indicator_trend_plot_multi <- renderPlotly({
      indicator_trend_data <- index_results %>% 
        filter(gsub(" ", "",location) %in% gsub(" ", "",list_all()))
      fig_a <- plot_ly(indicator_trend_data, x = ~year)
      
      if (input$indicators == "Socio-demographic Index"){
        titles = paste0("Time Series of SDI")
        ytitles = "Socio-demographic Index"
        fig_a <- fig_a  %>% add_trace(y=~sdi,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      }
      else if (input$indicators == "Eligibility to Receive DAH"){
        titles = paste0("Time Series of Eligibility to Receive DAH")
        ytitles = "Eligibility to Receive DAH"
        fig_a <- fig_a  %>% add_trace(y=~dah_eligible,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      }
      else if (input$indicators == "Total Health Spending per Person"){
        titles=paste0("Time Series of Total Health Spending per Person")
        ytitles = "Total Health Spending per Person"
        fig_a <- fig_a  %>% add_trace(y=~the_per_cap_mean,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      }
      else if (input$indicators == "Government Health Spending per Total Health Spending"){
        titles=paste0("Time Series of Government Health Spending per Total Health Spending")
        ytitles = "Government Health Spending \n per Total Health Spending"
        fig_a <- fig_a  %>% add_trace(y=~ghes_per_the_mean,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      }
      else if (input$indicators == "HAQI"){
        titles=paste0("Time Series of HAQI")
        ytitles = "HAQI"
        fig_a <- fig_a  %>% add_trace(y=~haqi,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
        
      }
      else if (input$indicators == "Corruption Perception Index"){
        titles=paste0("Time Series of Corruption Perception Index")
        ytitles = "Corruption Perception Index"
        fig_a <- fig_a  %>% add_trace(y=~cpi,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
        
      }
      else if (input$indicators == "Skilled Attendants at Birth"){
        titles=paste0("Time Series of Skilled Attendants at Birth")
        ytitles = "Skilled Attendants at Birth"
        fig_a <- fig_a  %>% add_trace(y=~perc_skill_attend,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
        
      }
      else if (input$indicators == "Immigrant Population (%)"){
        titles=paste0("Time Series of Immigrant Population (%)")
        ytitles = "Immigrant Population (%)"
        fig_a <- fig_a  %>% add_trace(y=~imm_pop_perc,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
        
      }
      else if (input$indicators == "Urbanicity (%)"){
        titles=paste0("Time Series of Urbanicity (%)")
        ytitles = "Urbanicity (%)"
        fig_a <- fig_a  %>% add_trace(y=~perc_urban,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      }
      else if (input$indicators == "Agree Vaccines are Safe"){
        titles=paste0("Time Series of Agreement Vaccines are Safe")
        ytitles = "Agree Vaccines are Safe"
        fig_a <- fig_a  %>% add_trace(y=~mean_agree_vac_safe,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
        
      }
      else if (input$indicators == "Agree Vaccines are Important"){
        titles=paste0("Time Series of Agreement Vaccines are Important")
        ytitles = "Agree Vaccines are Important"
        fig_a <- fig_a  %>% add_trace(y=~mean_agree_vac_important,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      }
      else{
        titles=paste0("Time Series of Agree Vaccines are Effectivel")
        ytitles = "Agree Vaccines are Effective"
        fig_a <- fig_a  %>% add_trace(y=~mean_agree_vac_effective,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      }
      
      fig_a <- fig_a %>% 
        layout( autosize = T,
                title = paste0(titles),
                showlegend = TRUE,
                xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = ytitles, showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
      fig_a
      
    })
    
    output$indicator_trend_plot <- renderPlotly({
      indicator_trend_data <- filter(index_results,gsub(" ", "",location) == gsub(" ", "", info$value))
      fig_a <- plot_ly(indicator_trend_data, x = ~year)
      
      if (input$indicators == "Socio-demographic Index"){
        left_text = round(indicator_trend_data$sdi[1],3)
        right_text =round(indicator_trend_data$sdi[30],3)
        left_y = indicator_trend_data$sdi[1]+0.01
        right_y = indicator_trend_data$sdi[30]+0.02
        titles = paste0("Time Series of SDI in United States of America")
        ytitles = "Socio-demographic Index"
        fig_a <- fig_a  %>% add_trace(y=~sdi,type='scatter', name = "SDI", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(sdi[1], sdi[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      #else if (input$indicators == "Development Assistance Per Total Health Spending Categorical"){
      # left_text = round(indicator_trend_data$dah_per_the_mean_cat[1],3)
      # right_text =round(indicator_trend_data$dah_per_the_mean_cat[30],3)
      # left_y = indicator_trend_data$dah_per_the_mean_cat[1]+0.01
      # right_y = indicator_trend_data$dah_per_the_mean_cat[30]+0.02
      # titles = paste0("Time Series of Development Assistance Per Total Health Spending Categorical")
      # ytitles = "Development Assistance Per Total Health Spending Categorical"
      # fig_a <- fig_a  %>% add_trace(y=~dah_per_the_mean_cat,type='scatter', name = "Development Assistance Per Total Health Spending Categorical", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      # fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(dah_per_the_mean_cat[1], dah_per_the_mean_cat[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
      #}
      else if (input$indicators == "Eligibility to Receive DAH"){
        left_text = " "
        right_text =" "
        left_y = as.numeric(indicator_trend_data$dah_eligible[1])+0.01
        right_y = as.numeric(indicator_trend_data$dah_eligible[30])+0.02
        titles = paste0("Time Series of Eligibility to Receive DAH")
        ytitles = "Eligibility to Receive DAH"
        fig_a <- fig_a  %>% add_trace(y=~dah_eligible,type='scatter', name = "Development Assistance Per Total Health Spending Categorical", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(dah_eligible[1], dah_eligible[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
      }
      else if (input$indicators == "Total Health Spending per Person"){
        left_text = round(indicator_trend_data$the_per_cap_mean[1],3)
        right_text =round(indicator_trend_data$the_per_cap_mean[30],3)
        left_y = indicator_trend_data$the_per_cap_mean[1]+0.01
        right_y = indicator_trend_data$the_per_cap_mean[30]+0.02
        titles=paste0("Time Series of Total Health Spending per Person")
        ytitles = "Total Health Spending \n per Person"
        fig_a <- fig_a  %>% add_trace(y=~the_per_cap_mean,type='scatter', name = "Total Health Spending per Person", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(the_per_cap_mean[1], the_per_cap_mean[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "Government Health Spending per Total Health Spending"){
        left_text = round(indicator_trend_data$ghes_per_the_mean[1],3)
        right_text =round(indicator_trend_data$ghes_per_the_mean[30],3)
        left_y = indicator_trend_data$ghes_per_the_mean[1]+0.01
        right_y = indicator_trend_data$ghes_per_the_mean[30]+0.02
        titles=paste0("Time Series of Government Health Spending per Total Health Spending")
        ytitles = "Government Health Spending \n per Total Health Spending"
        fig_a <- fig_a  %>% add_trace(y=~ghes_per_the_mean,type='scatter', name = "Government Health Spending per Total Health Spending", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(ghes_per_the_mean[1], ghes_per_the_mean[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "HAQI"){
        left_text = round(indicator_trend_data$haqi[1],3)
        right_text =round(indicator_trend_data$haqi[30],3)
        left_y = indicator_trend_data$haqi[1]+0.01
        right_y = indicator_trend_data$haqi[30]+0.02
        titles=paste0("Time Series of HAQI")
        ytitles = "HAQI"
        fig_a <- fig_a  %>% add_trace(y=~haqi,type='scatter', name = "HAQI", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(haqi[1], haqi[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "Corruption Perception Index"){
        left_text = round(indicator_trend_data$cpi[1],3)
        right_text =round(indicator_trend_data$cpi[30],3)
        left_y = indicator_trend_data$cpi[1]+0.01
        right_y = indicator_trend_data$cpi[30]+0.02
        titles=paste0("Time Series of Corruption Perception Index")
        ytitles = "Corruption Perception Index"
        fig_a <- fig_a  %>% add_trace(y=~cpi,type='scatter', name = "Corruption Perception Inde", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(cpi[1], cpi[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "Skilled Attendants at Birth"){
        left_text = round(indicator_trend_data$perc_skill_attend[1],3)
        right_text =round(indicator_trend_data$perc_skill_attend[30],3)
        left_y = indicator_trend_data$perc_skill_attend[1]+0.01
        right_y = indicator_trend_data$perc_skill_attend[30]+0.02
        titles=paste0("Time Series of Skilled Attendants at Birth")
        ytitles = "Skilled Attendants at Birth"
        fig_a <- fig_a  %>% add_trace(y=~perc_skill_attend,type='scatter', name = "Skilled Attendants at Birth", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(perc_skill_attend[1], perc_skill_attend[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "Immigrant Population (%)"){
        left_text = round(indicator_trend_data$imm_pop_perc[1],3)
        right_text =round(indicator_trend_data$imm_pop_perc[30],3)
        left_y = indicator_trend_data$imm_pop_perc[1]+0.01
        right_y = indicator_trend_data$imm_pop_perc[30]+0.02
        titles=paste0("Time Series of Immigrant Population (%)")
        ytitles = "Immigrant Population (%)"
        fig_a <- fig_a  %>% add_trace(y=~imm_pop_perc,type='scatter', name = "Immigrant Population (%)", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(imm_pop_perc[1], imm_pop_perc[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "Urbanicity (%)"){
        left_text = round(indicator_trend_data$perc_urban[1],3)
        right_text =round(indicator_trend_data$perc_urban[30],3)
        left_y = indicator_trend_data$perc_urban[1]+0.01
        right_y = indicator_trend_data$perc_urban[30]+0.02
        titles=paste0("Time Series of Urbanicity (%)")
        ytitles = "Urbanicity (%)"
        fig_a <- fig_a  %>% add_trace(y=~perc_urban,type='scatter', name = "Urbanicity (%)", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(perc_urban[1], perc_urban[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "Agree Vaccines are Safe"){
        left_text = round(indicator_trend_data$mean_agree_vac_safe[1],3)
        right_text =round(indicator_trend_data$mean_agree_vac_safe[30],3)
        left_y = indicator_trend_data$mean_agree_vac_safe[1]+0.01
        right_y = indicator_trend_data$mean_agree_vac_safe[30]+0.02
        titles=paste0("Time Series of Agreement Vaccines are Safe")
        ytitles = "Agree Vaccines are Safe"
        fig_a <- fig_a  %>% add_trace(y=~mean_agree_vac_safe,type='scatter', name = "Agree Vaccines are Safe", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(mean_agree_vac_safe[1], mean_agree_vac_safe[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "Agree Vaccines are Important"){
        left_text = round(indicator_trend_data$mean_agree_vac_important[1],3)
        right_text =round(indicator_trend_data$mean_agree_vac_important[30],3)
        left_y = indicator_trend_data$mean_agree_vac_important[1]+0.01
        right_y = indicator_trend_data$mean_agree_vac_important[30]+0.02
        titles=paste0("Time Series of Agreement Vaccines are Important")
        ytitles = "Agree Vaccines are Important"
        fig_a <- fig_a  %>% add_trace(y=~mean_agree_vac_important,type='scatter', name = "Agree Vaccines are Important", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(mean_agree_vac_important[1], mean_agree_vac_important[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else{
        left_text = round(indicator_trend_data$mean_agree_vac_effective[1],3)
        right_text =round(indicator_trend_data$mean_agree_vac_effective[30],3)
        left_y = indicator_trend_data$mean_agree_vac_effective[1]+0.01
        right_y = indicator_trend_data$mean_agree_vac_effective[30]+0.02
        titles=paste0("Time Series of Agreement Vaccines are Effective")
        ytitles = "Agree Vaccines are Effective"
        fig_a <- fig_a  %>% add_trace(y=~mean_agree_vac_effective,type='scatter', name = "Agree Vaccines are Effective", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(mean_agree_vac_effective[1], mean_agree_vac_effective[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      
      vii_left <- list(
        xref = 'paper',
        yref = 'y',
        x = 0.01,
        y = left_y,
        xanchor = 'middle',
        yanchor = 'center',
        text = ~left_text,
        font = list(family = 'Arial',
                    size = 16,
                    color = 'rgba(67,67,67,1)'),
        showarrow = FALSE)
      
      vii_right <- list(
        xref = 'paper',
        yref = 'y',
        x = 0.97,
        y = right_y,
        xanchor = 'middle',
        yanchor = 'center',
        text = ~right_text,
        font = list(family = 'Arial',
                    size = 16,
                    color = 'rgba(67,67,67,1)'),
        showarrow = FALSE)
      
      fig_a <- fig_a %>% 
        layout( autosize = T,
                title = paste0(titles, " in ",info$value),
                showlegend = FALSE,
                xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = ytitles,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
      fig_a <- fig_a %>% layout(annotations = vii_left) 
      fig_a <- fig_a %>% layout(annotations = vii_right) 
      fig_a
    })
    
    output$all_vaccine_plot <- renderPlotly({
      vac_plotdata <- filter(vaccine_trends,gsub(" ", "", location_name) == gsub(" ", "", info$value))
      if (input$vaccine_plot == "line_trend"){
        fig_a <- plot_ly(vac_plotdata, x = ~year_id,y=~prop_val, color = ~vaccine_name)%>%
          add_lines()
        fig_a <- fig_a %>% 
          layout( autosize = T,
                  title ="Time Series of Vaccination Coverage",  showlegend = T,
                  xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                  yaxis = list(title = "Vaccination Coverage (%)",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
        fig_a
      }
      else{
        single_year_vac_plotdata <- filter(vac_plotdata,year_id == input$year)
        fig1 <- plot_ly(x = ~single_year_vac_plotdata$prop_val, y = ~reorder(single_year_vac_plotdata$vaccine_name, single_year_vac_plotdata$prop_val), name = single_year_vac_plotdata$vaccine_name,
                        type = 'bar', orientation = 'h',color = single_year_vac_plotdata$vaccine_name)
        #marker = list(color = 'rgba(50, 171, 96, 0.6)',
        #line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) 
        fig1 <- fig1 %>% layout( autosize = T,
                                 title = paste0("Vaccination Coverage in ", input$year),
                                 yaxis = list(title = "Vaccine",showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                                 xaxis = list(title = "Vaccination Coverage (%)", zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
        fig1 <- fig1 %>% add_annotations(xref = 'x1', yref = 'y',
                                         x = single_year_vac_plotdata$prop_val * 1+ 0.05,  y = single_year_vac_plotdata$vaccine_name,
                                         text = paste(round(single_year_vac_plotdata$prop_val*100, 2), '%'),
                                         font = list(family = 'Arial', size = 12, color = 'rgb(0,0,0,1)'),
                                         showarrow = FALSE)
      }
    })
    output$all_disease_plot <- renderPlotly({
      disease_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", info$value))
      if (input$disease_estimate == "number_val"){
        fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~round(deaths_number_val,8), color = ~cause_name)%>%
          add_lines()
        title = "Time Series of Deaths, Disease or Disability Number"
        y_title = "Number of Deaths \n in Population"
      }
      else if (input$disease_estimate == "percent_val"){
        fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~round(deaths_percent_val,8), color = ~cause_name)%>%
          add_lines()
        title = "Time Series of Deaths, Disease or Disability Percent"
        y_title="Deaths for a Particular \n Cause/Deaths from All Causes"
      }
      else{
        fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~round(deaths_rate_val,8), color = ~cause_name)%>%
          add_lines()
        title = "Time Series of Deaths, Disease or Disability Rate"
        y_title="Deaths per 100,000 Population"
      }
      fig_dis <- fig_dis %>% 
        layout( autosize = T,
                title =title,  showlegend = T,
                xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE,type= "log"))
      fig_dis
    })
    
    output$all_disability_plot <- renderPlotly({
      disability_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", info$value))
      if (input$disease_estimate == "number_val"){
        fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_number_val, color = ~cause_name)%>%
          add_lines()
        title = "Time Series of Years Lived in Less Than Ideal Health in Population"
        y_title = "Years Lived with \n Disability in Population"
      }
      else if (input$disease_estimate == "percent_val"){
        fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_percent_val, color = ~cause_name)%>%
          add_lines()
        title = "Time Series of Proportion of Years Lived in Less Than Ideal Health in Population" 
        y_title="YLDs for Particular \n Cause/YLDs for All Causes" 
      }
      else{
        fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_rate_val, color = ~cause_name)%>%
          add_lines()
        title = "Time Series of Years Lived in Less Than Ideal Health per 100,000 Population"
        y_title="YLDs per 100,000 Population"
      }
      fig_disa <- plot_ly(disability_plotdata, x = ~year_id,y=~ylds_number_val, color = ~cause_name)%>%
        add_lines()
      fig_disa <- fig_disa %>% 
        layout( autosize = T,
                title =title,  showlegend = T,
                xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE,type= "log"))
      fig_disa
    })
    
    observeEvent(selected_dis_vac_data(),{
      output$selected_vac_dis_plot <- renderPlotly({
        selected_vac_plotdata <- filter(selected_dis_vac_data()$selected_vac_data,gsub(" ", "", location_name) == gsub(" ", "", info$value))
        selected_dis_plotdata <- filter(selected_dis_vac_data()$dis_data_for_selected_vac,gsub(" ", "", location_name) == gsub(" ", "", info$value))
        merged_selected_plotdata <- dplyr::left_join(selected_vac_plotdata,selected_dis_plotdata, "year_id", "year_id")
        print(selected_dis_plotdata)
        fig <- plot_ly()
        # Add traces
        fig <- plot_ly(merged_selected_plotdata)
        fig <- fig %>% add_trace(x= ~year_id, y = ~round(deaths_rate_val,8), type = 'scatter', mode = 'lines+makers', color = ~cause_name) 
        ay <- list(
          overlaying = "y",
          side = "right",
          title = "<b> Vaccine</b> Coverage (%)")
        
        fig <- fig %>% add_trace(x =  ~year_id, y = ~prop_val, type = 'scatter',name = ~vaccine_name.x,yaxis = "y2", mode = 'lines',line = list(color = 'rgba(49,130,189, 1)', width = 4)) 
        # Set figure title, x and y-axes titles
        fig <- fig %>% layout(
          autosize = T,
          title = list(text="Vaccine & Corresponding Disease Trend", x=0.25), 
          xaxis = list(title="Year"),
          yaxis = list(title="<b> Deaths</b> per 100,000 Population"),
          yaxis2 = ay,
          legend = list(x = 3000, y = 1.2)
        )%>%
          layout(xaxis = list(
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
            yaxis = list(
              zerolinecolor = '#ffff',
              zerolinewidth = 2,
              gridcolor = 'ffff')
          )
        
        fig
      })
    })
  })
  
  #output$vac_name <- renderText({ 
  #input$vaccinations
  #print(input$vaccinations)
  #})
  
  #output$vac_description <- renderText({ 
  #  unique(filter(vaccine_preventable_diseases,vaccine_name == input$vaccinations)$vaccine_description)
  #})
  
  output$vac_dis <- renderText({ 
    unique(filter(vaccine_preventable_diseases,vaccine_name == input$vaccinations)$cause_name)
  })
  
  output$BCGtable = DT::renderDataTable({
    bcg_table <- vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "BCG",][,c("cause_name")]
    formattable(
      bcg_table
    ) %>%
      as.datatable(rownames = FALSE,  colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE)
      )
  })
  
  
  output$DTPtable = DT::renderDataTable({
    table <- vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "DTP1",][,c("cause_name")]
    formattable(
      table
    ) %>%
      as.datatable(rownames = FALSE, colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE))
  })
  output$HepB3table = DT::renderDataTable({
    table <- vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "HepB3",][,c("cause_name")]
    formattable(
      table
    ) %>%
      as.datatable(rownames = FALSE,  colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE))
  })
  output$MCVtable = DT::renderDataTable({
    table <- unique(vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "MCV1" | vaccine_preventable_diseases$vaccine_name == "MCV2",][,c("cause_name")])
    formattable(
      table
    ) %>%
      as.datatable(rownames = FALSE,  colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE))
  })
  output$RotaCtable = DT::renderDataTable({
    table <- vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "RotaC",][,c("cause_name")]
    formattable(
      table
    ) %>%
      as.datatable(rownames = FALSE,  colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE))
  })
  
  dataexplorer <- reactive({
    req(input$dataset)
    if(input$dataset == "vaccine trends"){
      dataexplorer <- vaccine_trends
    }
    else if (input$dataset == "disease trends"){
      dataexplorer<-disease_trends
    }
    else if (input$dataset == "data dictionary"){
      dataexplorer<-codebook
    }
    else{
      dataexplorer <- index_results
    }
  })
  
  output$alldatatable = DT::renderDataTable({
    data<-dataexplorer()
    if(input$dataset == "vaccine trends"){
      x<-data %>%
        dplyr::select(-c("location_id"))
      pl=17
    }
    else if (input$dataset == "disease trends"){
      x<-data %>%
        dplyr::select(-c("location_id","cause_id"))
      pl=11
    }
    else if(input$dataset == 'data dictionary'){
      x<-data 
      pl=8
    }
    else{
      x<-data 
      pl=11
    }
    formattable(
      x
    ) %>%
      as.datatable(rownames = FALSE,
                   options = list(paging = TRUE,
                                  searching = TRUE,
                                  scrollX=TRUE, 
                                  ordering = TRUE,
                                  dom = '<lf<t>p>',
                                  pageLength=pl,
                                  lengthChange = FALSE))
  })
  output$download <- downloadHandler(
    filename =  paste0(input$dataset,".csv",sep=""),
    content = function(fname){
      write.csv(dataexplorer(), fname)
    }
  )
  
  #*****
  #* Download Report
  output$report <- downloadHandler(
    filename =   paste0("Nigeria-sample-report_", Sys.Date(), ".pdf"),
    content = function(file) {
      rmarkdown::render("report.Rmd",
                        output_file = file, 
                        params = list(
                          data = index_results,
                          muilti = input$my_multi,
                          vactrend = vaccine_trends,
                          dic_trend = disease_trends,
                          preventable_vac_trend = preventable_vac_trend,
                          merged_data_for_vac_dis = merged_data_for_vac_dis
                        ),
                        envir = new.env(parent = globalenv()))
    }
  )
}
 