library(shiny)
library(dplyr)
library(leaflet)
library(randomForest)
library(plotly)

dat <- read.csv("data/kc_house_data.csv")
dat2 <- dat


parse_price <- function(dat){
  num_price <- dat["price"][[1]]
  parsed <- paste0('$',format(num_price,big.mark=','))
  return(parsed)
}

dat$parsed_price <- apply(dat,1,parse_price)


rf_model <- readRDS("rf_model.rds")

dat2$grade <- as.factor(dat2$grade)
dat2$bedrooms <- as.factor(dat2$bedrooms)

bedroom_lvls <- as.vector(levels(dat2$bedrooms))
grade_lvls <- as.vector(levels(dat2$grade))


shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  
  table_df <- reactive({
    
    df <- dat %>% 
              select("price","parsed_price","yr_built","sqft_living",
                     "sqft_lot","bedrooms","bathrooms","floors","view",
                     "condition","grade","lat","long") %>% 
              filter((input$price[1] <= price)&(price <= input$price[2])) %>% 
              filter((input$sqft_living[1] <= sqft_living)&(sqft_living <= input$sqft_living[2])) %>%
              filter((input$sqft_lot[1] <= sqft_lot)&(sqft_lot <= input$sqft_lot[2])) %>%
              filter(bathrooms <= input$bathrooms) %>%
              filter(floors <= input$floors) %>%
              filter(view <= input$view) %>%
              filter((input$condition[1] <= condition)&(condition <= input$condition[2])) %>%
              filter((input$grade[1] <= grade)&(grade <= input$grade[2])) %>%
              filter((input$yr_built[1] <= yr_built)&(yr_built <= input$yr_built[2]))

    
    if(4 %in% input$bedrooms){
      beds_vec <- append(input$bedrooms,c(5,6,7,8,9,10,11,33))
    } else if (is.null(input$bedrooms)){
      beds_vec <- c(0)
    } else {
      beds_vec <- input$bedrooms
    }
    
    df %>% filter(bedrooms %in% beds_vec)

  })
  
  
  output$tabla <- renderDataTable({
      table_df() %>%
                    select(-c("lat","long","price"))
                    
    },
    options=list(
                 pageLength=13,
                 lengthChange=FALSE,
                 searching=FALSE
                 ))
  # columnDefs = list(list(className = 'dt-center', targets = "_all")) no funciona
  
  output$mapa <- renderLeaflet({

    if(nrow(table_df())==0){
      fig_map <- leaflet() %>%
        addTiles() %>%
        setView(lng = -122.05, lat = 47.50, zoom = 9.5)
    } else {
      fig_map <- table_df() %>%
        leaflet() %>%
        setView(lng = -122.05, lat = 47.50, zoom = 9.5) %>%
        addTiles() %>%
        addMarkers(lng = ~long,
                   lat = ~lat,
                   label = ~parsed_price,
                   clusterOptions = markerClusterOptions())
    }
    
    fig_map
  })
  
  observe({
    print(table_df())
  })
  
  observeEvent(input$reset, {
    updateSliderInput(session,'price',value = c(min(dat$price),max(dat$price)))
    updateSliderInput(session,'sqft_living',value = c(min(dat$sqft_living),max(dat$sqft_living)))
    updateSliderInput(session,'sqft_lot',value = c(min(dat$sqft_lot),max(dat$sqft_lot)))
    updateSliderInput(session,'bathrooms',value = max(dat$bathrooms))
    updateSliderInput(session,'floors',value = max(dat$floors))
    updateSliderInput(session,'view',value = max(dat$view))
    updateSliderInput(session,'condition',value = c(min(dat$condition),max(dat$condition)))
    updateSliderInput(session,'grade',value = c(min(dat$grade),max(dat$grade)))
    updateSliderInput(session,'yr_built',value = c(min(dat$yr_built),max(dat$yr_built)))
    updateCheckboxGroupInput(session,"bedrooms",selected = c(1,2,3,4))
  })
  
  values$house_size <- as.integer(1000)
  values$property_size <- as.integer(1000)
  values$bedrooms <- factor(1,levels = bedroom_lvls)
  values$bathrooms <- as.numeric(1)
  values$year_built <- as.integer(2000)
  values$cons_grade <- factor(7,levels = grade_lvls)
  
  pred <- reactive({
    df <- data.frame(bedrooms = values$bedrooms,
               bathrooms = values$bathrooms,
               sqft_living = values$house_size,
               yr_built = values$year_built,
               sqft_lot = values$property_size,
               grade= values$cons_grade)
    predicted <- predict(rf_model,df)
    return(predicted)
  })
  
  
  observeEvent(input$submit,{
    values$house_size <- as.integer(input$sqft_living_model)
    values$property_size <- as.integer(input$sqft_lot_model)
    values$bedrooms <- factor(input$bedrooms_model,levels = bedroom_lvls)
    values$bathrooms <- as.numeric(input$bathrooms_model)
    values$year_built <- as.integer(input$yr_built_model)
    values$cons_grade <- factor(input$grade_model,levels = grade_lvls)
  })
  
  output$prediccion <- renderText({
    output_text <- paste0( '$',format(pred(),big.mark=',') )
    return(output_text)
  })
  
  output$scatter <- renderPlotly({
    fig <- plot_ly(dat,
                   x=~sqft_living,
                   y=~yr_built,
                   z=~price,
                   color = ~condition,
                   colors = c('#FC0202', '#F1A50C','#DFD91E','#8BEA7C','#3BB329'),
                   hoverinfo= 'z') 
    
    fig <- fig %>% add_markers()
    fig <- fig %>% layout(title='Size vs Year Built vs Price (colored by cond.)',
                          scene = list(xaxis = list(title = 'Sqft House'),
                                       yaxis = list(title = 'Year Built'),
                                       zaxis = list(title = 'Price ($)'),
                                       camera = list(eye = list(x = 2.7, y = 2.7, z = 1.5))),
                          margin = 1
                          )
    fig
  })
  
  output$hist_sqft <- renderPlotly({
    fig <- plot_ly(x = dat$sqft_living, type = "histogram",alpha=0.6) %>%
            layout(title="House Size Distribution",margin = 1)
    fig
  })
  
  output$lineplot <- renderPlotly({
    df <- dat %>% 
      group_by(yr_built) %>% 
      summarise(mean_yr = mean(price))

    accumulate_by <- function(dat, var) {
      var <- lazyeval::f_eval(var, dat)
      lvls <- plotly:::getLevels(var)
      dats <- lapply(seq_along(lvls), function(x) {
        cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
      })
      dplyr::bind_rows(dats)
    }
    
    fig <- df %>% accumulate_by(~yr_built)
    fig <- fig %>%
      plot_ly(
        x = ~yr_built, 
        y = ~mean_yr,
        frame = ~frame, 
        type = 'scatter',
        mode = 'lines+markers', 
        line = list(simplyfy = F)
      )
    
    fig <- fig %>% layout(
      xaxis = list(title = "Year Built",zeroline = F),
      yaxis = list(title = "Mean Sold Price ($)",zeroline = F),
      title = "Mean Price over Time",
      margin = 1) 
    
    fig <- fig %>%
      animation_opts(frame = 50, transition = 1, redraw = FALSE) %>% 
      animation_slider(hide = T) %>%
      animation_button(x = 0, xanchor = "right", y = 0, yanchor = "bottom")
    
    fig
  })
  
  output$hometext <- renderUI({
    HTML(paste("<p style='text-align:justify'><br/> House Sales in King County, USA This dataset contains house sale prices for King County,",
          "which includes Seattle. It includes homes sold between May 2014 and May 2015. Within the dataset",
          "we can find information about the size of both the house and the property, basic information like",
          "number of bathrooms, bedrooms, floors, etc., and the selling price. We also have available the",
          "coordinates for each of the sold houses. <br/>",
          "In this interactive Dashboard you will find graphs of the mean price over for each house by year it",
          "was built, histogram of the house size variable, and a scatter plot of positiveley correlated",
          "variables. A tool to help searching for houses based on filters is also available, with the output",
          "either as a table or a map view with the location of the houses and their sold price as a hover label.",
          "Finally, a simple predictive model allows users to estimate their house value based on a model trained",
          "with this dataset. Users can also pass URL parameters to specify initial inputs for the model. <br/> <br/>",
          "<b>*NOTE:</b> The predictive model output was 'trained' with houses of King County",
          "therefore inputs for houses in that area give reasonable outputs. Houses from other areas will very likely",
          "have a different patterns and therefore give biased outputs. <br/> <b>**NOTE:</b><br/>URL parameters are:<br/>",
          "<ul><li>house_size</li><li>property_size</li><li>bedrooms</li><li>bathrooms</li><li>year_built</li><li>grade</li></ul></p>"))
  })
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    h_size <- query[["house_size"]]
    p_size <- query[["property_size"]]
    beds <- query[["bedrooms"]]
    baths <- query[["bathrooms"]]
    y_b <- query[["year_built"]]
    c_grade <- query[["grade"]]
    
    if(!is.null(h_size)){
      updateNumericInput(session,"sqft_living_model",value=as.integer(h_size))
      values$house_size <- as.integer(h_size)
    }
    
    if(!is.null(p_size)){
      updateNumericInput(session,"sqft_lot_model",value=as.integer(p_size))
      values$property_size <- as.integer(p_size)
    }
    
    if(!is.null(beds)){
      updateCheckboxGroupInput(session,"bedrooms_model",selected = as.integer(beds))
      values$bedrooms <- factor(as.integer(beds),levels = bedroom_lvls)
    }
    
    if(!is.null(baths)){
      updateNumericInput(session,"bathrooms_model",value=as.numeric(baths))
      values$bathrooms <- as.numeric(baths)
    }
    
    if(!is.null(y_b)){
      updateNumericInput(session,"yr_built_model",value=as.integer(y_b))
      values$year_built <- as.integer(y_b)
    }
    
    if(!is.null(c_grade)){
      updateCheckboxGroupInput(session,"grade_model",selected = as.integer(c_grade))
      values$cons_grade <- factor(as.integer(c_grade),levels = grade_lvls)

    }
    
  })
  
})
