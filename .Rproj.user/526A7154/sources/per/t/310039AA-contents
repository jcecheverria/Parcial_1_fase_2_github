
library(shiny)
library(dplyr)
library(leaflet)
library(randomForest)
library(plotly)


dat <- read.csv("data/kc_house_data.csv")

shinyUI(fluidPage(
  
  tabsetPanel(
    tabPanel('Home',
             column(4,
                    titlePanel("King County House Sales"),
                    htmlOutput("hometext")),
             column(8,
                    fluidRow(plotlyOutput("lineplot",width = "auto")),
                    fluidRow(column(6,
                                    plotlyOutput("scatter",width = "auto")),
                             column(6,
                                    plotlyOutput("hist_sqft",width = "auto"))
                      
                    ))
    ), # margin-left:300px
    tabPanel('Search', # button#reset {float:right;margin-left:100%;}
             sidebarLayout(
               sidebarPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px;",
                 tags$style(type='text/css', "#reset {margin-left:90%;}"),
                 actionLink('reset','Reset'),          
                 sliderInput('price','Price Range',
                             value = c(min(dat$price),max(dat$price)),
                             min=min(dat$price),
                             max=max(dat$price),
                             pre="$"),
                 sliderInput('yr_built','Year Built',
                             value = c(min(dat$yr_built),max(dat$yr_built)),
                             min=min(dat$yr_built),
                             max=max(dat$yr_built)),
                 sliderInput('sqft_living','House Size Range (sq. ft.)',
                             value = c(min(dat$sqft_living),max(dat$sqft_living)),
                             min=min(dat$sqft_living),
                             max=max(dat$sqft_living)),
                 sliderInput('sqft_lot','Property Size Range (sq. ft.)',
                             value = c(min(dat$sqft_lot),max(dat$sqft_lot)),
                             min=min(dat$sqft_lot),
                             max=max(dat$sqft_lot)),
                 checkboxGroupInput("bedrooms", label = strong("Bedrooms"), 
                                    choices = list("1" = 1, "2" = 2, "3" = 3,"4+"=4),
                                    selected = c(1,2,3,4),inline=TRUE),
                 sliderInput('bathrooms','Bathrooms',
                             value = max(dat$bathrooms),
                             min=min(dat$bathrooms),
                             max=max(dat$bathrooms)),
                 sliderInput('floors','Floors',
                             value = max(dat$floor),
                             min=min(dat$floor),
                             max=max(dat$floor)),
                 sliderInput('view','View Quality',
                             value = max(dat$view),
                             min=min(dat$view),
                             max=max(dat$view)),
                 sliderInput('condition','Condition',
                             value = c(min(dat$condition),max(dat$condition)),
                             min=min(dat$condition),
                             max=max(dat$condition),
                             step=1),
                 sliderInput('grade','Construction Grade',
                             value = c(min(dat$grade),max(dat$grade)),
                             min=min(dat$grade),
                             max=max(dat$grade),
                             step = 1)
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Table",dataTableOutput("tabla")),
                   tabPanel("Map",leafletOutput("mapa",height=550))
                 )
                 
               )
               
             )
             
    ),
    tabPanel('Predict House Value',
             sidebarLayout(
               sidebarPanel(
                 numericInput("sqft_living_model", 'House Size (sq. ft.)', value = 1000 , step=10),
                 numericInput("sqft_lot_model", 'Property Size (sq. ft.)', value = 1000, step=10),
                 selectInput("bedrooms_model","Bedrooms",choices=sort(unique(dat$bedrooms)),selected=1),
                 numericInput("bathrooms_model","Bathrooms",value=1),
                 numericInput("yr_built_model","Year Built",value=2000),
                 selectInput("grade_model","Construction Grade",choices=sort(unique(dat$grade)),selected=7),
                 actionButton('submit','Submit')
                 ),
               mainPanel(
                 h1(strong("Predicted Price")),
                 h3(textOutput("prediccion"))
               )
             )
    )
  )
))
