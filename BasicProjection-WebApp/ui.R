# UI component
models <- c('Linear Regression', 'Recursive Partitioning', 'Random Forest', 'Artificial Nueral Network')

minDate <- '2015-01-01'
maxDate <- '2018-12-27'

minTemp <- -20
maxTemp <- 50

minWind <- 0
maxWind <- 8

addResourcePath("img", "BasicProjection-WebApp/img")

# ====================================================================
# User Interface
ui <- fluidPage(
  
  titlePanel("Visitor Projection - Test 01"),
  
  tabsetPanel(
    tabPanel("Introduction", 
             
             h2('Prediction Pipeline'),
             p('This predictive analysis prototype demonstrates the possibility to utilize collected data 
                and create the basic future projection of some specific data dimensions. Also, it is an 
                example of data science product development with rapid prototyping methodology which
                involves various kinds of data and multidisciplinary skills; delivered as part of 
                SSTM-NSM staff exchange program 2018.'),
             
             h3('Data Preparation'),
             h4('Weather Data Scraper'),
             p('Hypothetical assumption of this analysis is that weather condition could affect the number of
                museum visitors. A web scraper tool was create to automatically retrieve weather from following website. '),
             pre('http://www.tianqihoubao.com/lishi/shanghai'),
             div( 
               'Web scraper was written in Python 3. Please refer to Jupyter Notebook ',
                strong(em('"Weather Scraper.ipynb"')),
              ' for more information. Daily weather data of each year would be saved in each file win following format:',
              #p(img(src = 'img/Introduction-1.png', width="40%")),
              pre('
Scraped Weather Data
---------------------------------------------------------------------
| Date |  Weather 1  | Weather 2 | Min Temp | Max Temp | Wind Speed |
---------------------------------------------------------------------')
              ),
             br(),
             
             h4('Data Mapping & Reformat'),
             div(
               'Real visitor data CSV file is in following format.',
               pre('
Real Visitor Data
-------------------------
| Date |  Visitor Count |
-------------------------'),
               'Mapping script was written a Python 3. Please refer to Jupyter Notebook ',
               strong(em('"DataPrep-Mapping.ipynb"')),
               ' for more information. Final data for modeling process is composed of following columns.',
               pre('
date
minTemp
maxTemp 
clear
cloudy
rain
snow
storm
wind 
weekend
holiday
visitorTotal
visitorMale 
visitorFemale
visitorKids
VisitorTeenage
VisitorAdult
VisitorElderly
OverMonthlyAverage'),
               'Some columns were prepared for future implementation, thus, currently unused by the predictive analysis.'
               ),
             
             h3('Data Modeling'),
             h4('Model Creation'),
             p('In order to project future value of a dataset from a given set od features, 
               many regression models can be used with different characteristics and properties. 
               Following regression models were selected in this prototype due to their readability 
               to work on prepared dataset without much modification.'),
             tags$ul(
                tags$li('Linear Regression'),
                tags$li('Recursive Partitioning and Regression Trees'),
                tags$li('Random Forest'),
                tags$li('Artificial Neural Network')),
                
             h4('Prediction'),
             p('Prediction pipeline in this application can be depiected as follows:'),
             pre('')
    ), 
    
    tabPanel("Model Preview", 
             sidebarLayout(
               position = "right",
               sidebarPanel(
                 dateRangeInput("dates", h3("Date Range:"), start = minDate, end = maxDate),
                 h4('Show Model:'),
                 checkboxInput('lm', 'Linear Regression', value = TRUE),
                 checkboxInput('rpart', 'Recursive Partitioning'),
                 checkboxInput('rf', 'Random Forest'),
                 checkboxInput('ann', 'Artificial Neural Network')
                 
               ), mainPanel(
                 h4('Mapping prediction result with real visitor amount'),
                 plotOutput('plotPreview'),
                 h6('* Available data between 2015-01-01 and 2018-12-17')
                 
               )
               
               
             )
    ), 
    tabPanel("Model Projection", 
             sidebarLayout(
               position = "right",
               sidebarPanel(
                 #selectInput('model', 'Model Selection:', models),
                 h4('Weather Condition:'),
                 sliderInput(inputId = "avgTemp",
                             label = "Average Temperature (c):",
                             min = minTemp,
                             max = maxTemp,
                             value = 20),
                 checkboxInput('cloudy', 'Cloudy'),
                 checkboxInput('rain', 'Rain'),
                 checkboxInput('storm', 'Storm'),
                 checkboxInput('snow', 'Snow'),
                 sliderInput(inputId = "wind",
                             label = "Wind Speed:",
                             min = minWind,
                             max = maxWind,
                             value = 3),
                 h4('Date Condition:'),
                 checkboxInput('weekend', 'Weekend'),
                 checkboxInput('holiday', 'Holiday')
                 
               ), mainPanel(
                 plotOutput("plotPredict"), 
                 #textOutput("textPredict") # text output
                 tags$i(h6("* Disclaimer I: Prediction result depends largely on a number of hypothetical assumptions, algorithimic parameters, and limited available data. Accuracy cannot be guaranteed by any means.")),
                 tags$i(h6("** Disclaimer II: Prediction result in each execution may be slightly different on a regular basis due to random sampling of training dataset."))
                 
               )
               
               
             )
    ),
    tabPanel("Model Description", 
             tabsetPanel(
                  tabPanel("Linear Regression",
                              verbatimTextOutput('lmSummary')
                           ), 
                  tabPanel("Recursive Partitioning",
                              verbatimTextOutput('rpartSummary')
                           ), 
                  tabPanel("Random Forest",
                              verbatimTextOutput('rfSummary')
                           ), 
                  tabPanel("Artificial Neural Network",
                           plotOutput('annPlot'),
                           verbatimTextOutput('annSummary')
                              
                           )
               
             )
    ),
    hr(),
    h5('SSTM Visitor Predictive Analysis Prototype - 2019')
  )
)
