#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(dplyr)
library(outliers)

movies <-
    readr::read_csv(
        'movies.csv'
    )

nums <- unlist(lapply(movies, is.numeric))  
dt_num<- movies[ , nums]


library(shiny)
library(shinydashboard)


# DBSCAN
library(fpc)
library(dbscan)
library(plotly)

## ui.R ##
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Select Data", tabName = "task1"),
        menuItem("Data Distribution",tabName = "task2"),
        menuItem("Outlier Detection",tabName = "task3"),
        menuItem("Clustering",tabName = "task4")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "task1",
                fluidRow(
                    mainPanel(width = 12,
                              DT::dataTableOutput("movies"))
                )),
        
        tabItem(tabName = "task2",
                fluidPage(
                    varSelectInput("variable", "Variable:", movies),
                            plotOutput("movies2")
                )),
        
        tabItem(tabName = "task3",
                fluidPage(
                  tabsetPanel(
                    tabPanel(strong('Statistical Test'), height = 'auto',
                             br(),
                             box(solidHeader = F, status = 'primary', collapsible = T,width = 3,
                                 column(12, varSelectInput("variable_DBout1", "Select a Column:", dt_num, selected = 'imdb_score'),
                                        sliderInput('quantile_range', 'Quantile range (%):',
                                                    min = 0,
                                                    sep = '',
                                                    max = 100, value = c(2.5, 97.5))
                                        
                                        )),
                             box(solidHeader = F, status = 'primary', collapsible = T,width = 9, height=770,
                                 fluidRow(
                                   column(12, plotOutput('quantile_plot', height = 770))
                                 )
                             )
                    )
                  )
                )),
        
        tabItem(tabName = "task4",
                fluidPage(
                  tabsetPanel(
                    tabPanel(strong('kmeans'), height = 'auto',
                             br(),
                             # varSelectInput("variable", "Variable:", movies),
                             box(solidHeader = F, status = 'primary', collapsible = T,width = 3,
                                 column(12, varSelectInput("variableKM1", "X:", dt_num, selected = 'num_critic_for_reviews'),
                                        varSelectInput("variableKM2", "Y:", dt_num, selected = 'imdb_score'),
                                        numericInput("cluster", "Cluster Count:", 2, min = 1, max = 50)
                                 )
                             ),
                             box(solidHeader = F, status = 'primary', collapsible = T,width = 9, height=770,
                                 fluidRow(
                                   column(12, plotlyOutput('kmeans_plot', height = 770))
                                 )
                             )
                    ),
                    
                    tabPanel(strong('DBSCAN'), height = 'auto',
                             br(),
                             box(solidHeader = F, status = 'primary', collapsible = T,width = 3,
                                 column(12, varSelectInput("variable_DB1", "X:", dt_num, selected = 'num_critic_for_reviews'),
                                 varSelectInput("variable_DB2", "Y:", dt_num, selected = 'imdb_score'),
                                 numericInput("radius", "Radius:", 5, min = 1, max = 10),
                                 numericInput("n_minPnts", "Number of minPnts:", 20, min = 1, max = 50)
                                 )),
                             box(solidHeader = F, status = 'primary', collapsible = T,width = 9, height=770,
                                 fluidRow(
                                   column(12, plotlyOutput('dbscan_plot', height = 770))
                                 )
                             )
                             )
  
                  )
                ))
        )
    )


ui <- dashboardPage(
    dashboardHeader(title = "Retake Assignment"),
    sidebar,
    body
)

server <- function(input, output) {
    
    output$movies <- DT::renderDataTable(movies,
                                         options = list(scrollX = TRUE),
                                         rownames = FALSE)
    output$movies2 <-  renderPlot({
        ggplot(movies, aes(!!input$variable)) + geom_histogram(stat="count")
    })
    
    variable_2 <- reactive({ input$variable2})
    
    lowerq <- reactive({quantile(variable_2,na.rm=TRUE)[2]})
    upperq <- reactive({quantile(variable_2,na.rm=TRUE)[4]})
    iqr = #upperq + " / " + lowerq #Or use IQR(data)
    
    mild.threshold.upper = #'(iqr * 1.5) + upperq'
    mild.threshold.lower = #'lowerq - (iqr * 1.5)'
    
    movies3 <- reactive({movies %>%
      mutate(new_column = ifelse(variable_2 < mild.threshold.upper & variable_2 > mild.threshold.lower, 0, 1))})
    
    output$movies3 <-  renderPlot({
      ggplot(movies3, aes(variable_2)) + geom_histogram(stat="count")
    })
    
    
    
    # Statistical Test -----------------------------------------------------------------------------------------------------------------------------------
    
    # library(DDoutlier)
    
    # outliers <- boxplot(dt_num$num_critic_for_reviews, dt_num$imdb_score, plot=FALSE)$out
    # 
    # lower_bound <- quantile(dt_num$imdb_score, 0.025)
    # upper_bound <- quantile(dt_num$imdb_score, 0.975)
    # 
    # outlier_ind <- which(dt_num$imdb_score < lower_bound |dt_num$imdb_score> upper_bound)
    # outlier_ind
    
    
    lower_bound <- reactive({
      quantile(dt_num[, !!input$variable_DBout1], input$quantile_range[1])
    })
    
    
    upper_bound <- reactive({
      print('yes')
      quantile(dt_num[, !!input$variable_DBout1], input$quantile_range[2])
    })
    
    quantile_dt <- reactive({
      
      dt_num %>%
          dplyr::select( !!input$variable_DBout1)
      
    })
    
    
    quantile_plot <- renderPlot({
 
      
      colnames(quantile_dt) <- 'x'
      
      ggplot(quantile_dt(), aes(as.numeric(rownames(quantile_dt())), x ))+
        geom_point(shape = 4,size = 4,  color = 'blue')+
                     # ifelse(dt_num[, !!input$variable_DBout1] < lower_bound() | dt_num[, !!input$variable_DBout1] > upper_bound(), "red", "blue"))+
        labs(x='ID', y =  paste('',input$variable_DBout1)) +  theme_bw()
      
    })
    
    # ggplot(dt_num, aes(as.numeric(rownames(dt_num)), imdb_score ))+
    #   geom_point(shape = 4,size = 4,  color = ifelse(imdb_score < lower_bound |imdb_score> upper_bound, "red", "blue"))+
    #   labs(x='ID') +  theme_bw()
    


    
    
    

    # KMEANS -----------------------------------------------------------------------------------------------------------------------------------
    
    dt_km <- reactive({
      
      x <- dplyr::select(dt_num, !!input$variableKM1,!!input$variableKM2 )
      # x <- x[, get(input$variable1)]
      x[complete.cases(x),]
    })
    
    
    
    
    dt_kmns <- reactive({
      kmeans(dt_km(), centers = input$cluster)
    })
    
    
    kmeans_plot <- renderPlotly({
      # plot(dt_km(), col = dt_mns()$cluster+1)
      x <- dt_km()
      colnames(x) <- c('x', 'y')
      
      plt <- ggplot(x, aes(x,y)) +
        geom_point(aes(color = as.factor(dt_kmns()$cluster)))+theme_bw() + 
        # labs(x = paste0('', input$variableKM1),
        #                                                                           y = paste0('', input$variableKM2),
        #                                                                           color = '')+
        theme(axis.text = element_text(size=12),
              text = element_text(size=12))
      
      # ggplot(x, aes(x,y)) +
      #   geom_point(aes(color = as.factor(dt_mns$cluster)))+theme_bw() + 
      #   theme(axis.text = element_text(size=12),
      #         text = element_text(size=12))
      
      
      ggplotly(plt)
    })
    

    
    
    # DBSCAN
    
    dt_rec <- reactive({
      
      x <- dplyr::select(dt_num, !!input$variable_DB1,!!input$variable_DB2 )
      # x <- x[, get(input$variable1)]
      x[complete.cases(x),]
    })
    
    
    
    dt_dbscan <- reactive({
      fpc::dbscan(dt_rec(), eps = input$radius, MinPts = input$n_minPnts)
    })
    
    # Plot DBSCAN results
    output$dbscan_plot <- renderPlotly({
      x <- dt_rec()
      colnames(x) <- c('x', 'y')
      
      plt <- ggplot(x, aes(x,y)) +
        geom_point(aes(color = as.factor(dt_dbscan()$cluster)))+theme_bw() + labs(x = paste0('', input$variable_DB1),
                                                                                  y = paste0('', input$variable_DB2),
                                                                                  color = '')+
        theme(axis.text = element_text(size=12),
              text = element_text(size=12))
      ggplotly(plt)
      
      # plot(dt_dbscan(),  dt_rec(),frame = F)
      
      # dt_dbscan
      
      # plot(dt_dbscan, x, main = "DBSCAN", frame = FALSE)
      # library("factoextra")
      # fviz_cluster(dt_dbscan(), dt_rec(), stand = FALSE, frame = FALSE, geom = "point")
    })
  
    
    
}

# Run the application
shinyApp(ui = ui, server = server)