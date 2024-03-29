#import libraries
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(ggfortify)
library(DT)
library(stringr)
library(promises)
library(future)
# 
# correlations_by_era <- training_data %>%
#   dplyr::select(era, target, starts_with("feature_")) %>%
#   mutate(era = as.numeric(era)) %>%
#   split(.$era) %>%
#   purrr::map(function(x) {
#     y <- x %>% dplyr::select(-era)
#     res <- cor(y)[1,]
#     res <- c(x$era[1], tail(res, -1))
#     
#     return(res)
#   }) %>%
#   bind_rows() %>%
#   t %>%
#   as_tibble()
# 
# names(correlations_by_era) <- training_data %>%
#   dplyr::select(era, starts_with("feature_")) %>%
#   names()
# 
# write_csv(correlations_by_era, "data/correlations.csv")
# q1 <- function(x) quantile(x, .25)
# q3 <- function(x) quantile(x, .75)
# 
# summary <- training_data %>%
#   dplyr::select(starts_with("feature_")) %>%
#   summarise(across(everything(), list(mean = mean, sd = sd, q1 = q1, q3 = q3, min = min, max = max, median = median))) %>%
#   gather(key = feature, value = value) %>%
#   mutate(stat = gsub(".*\\_([a-z0-9]+)$", "\\1", feature),
#          feature = gsub("(.*)\\_[a-z0-9]+$", "\\1", feature)) %>%
#   spread(key = stat, value = value) %>%
#   dplyr::select(feature, mean, sd, min, q1, median, q3, max)
# 
# write_csv(summary, "data/summary_features.csv")

# era_table <- training_data %>%
#   mutate(era = as.numeric(era)) %>%
#   group_by(era) %>%
#   summarise(n = n())
# 
# write_csv(era_table, "data/era_table.csv")

# target_mean_by_era <- training_data %>%
#   mutate(era = as.numeric(era)) %>%
#   group_by(era) %>%
#   summarise(Mean = mean(target))
# 
# write_csv(target_mean_by_era, "data/target_mean_by_era.csv")

# target_proportion_by_era <- training_data %>%
#   mutate(era = as.numeric(era)) %>%
#   group_by(era, target) %>%
#   summarise(count = n()) %>%
#   group_by(era) %>%
#   mutate(Proportion = count / sum(count))
# 
# write_csv(target_proportion_by_era, "data/target_proportion_by_era.csv")

# column_names <- training_data %>%
#   filter(era == "fake")
# 
# write_csv(column_names, "data/column_names.csv")

# save(training_data, file = "data/training_data.RData")

#import data
correlations_by_era <- read.csv("data/correlations.csv", header=T)
summary <- read.csv("data/summary_features.csv", header=T)
era_table <- read.csv("data/era_table.csv", header=T)
target_mean_by_era <- read.csv("data/target_mean_by_era.csv", header=T)
target_proportion_by_era <- read.csv("data/target_proportion_by_era.csv", header=T)
column_names <- read.csv("data/column_names.csv", header=T)


#change column names of summary statistcs of features
names(summary)[2:8] <- c("Mean", "Standard Deviation", "Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")

target_proportion_by_era$target = as.factor(target_proportion_by_era$target)

# Define UI for application
addResourcePath('omni_numerai.png', 'images/omni_numerai.png')

ui <-fluidPage(theme = shinytheme("cerulean"), 
               navbarPage(title=div(img(src="omni_numerai.png", height =30), 
                                    "Numerai Tournament Training Data Explorer"), windowTitle = "Numerai Tournament Training Data Explorer",
                tabPanel("Data",
                             sidebarLayout(
                               sidebarPanel(
                                HTML(" 
                                <p>Numerai is a crowdsourced hedgefund that hosts machine learning tournaments 
                                which attract thousands of data scientists around the world to compete for 
                                Numeraire cryptocurrency. The company provides clean, regularized, and 
                                obfuscated data, where anyone with expertise in machine learning can freely 
                                participate. Other than that, they also include various guides and coding 
                                samples to help novice data scientists get started. To get access to the data 
                                simply visit the <a href='https://numer.ai/'>official Numerai website</a>. </p>
                                
                                <p>The training_data is 10.7 GB and contains information on 2,412,105 rows across 1074 columns.
                                The first two columns are as follows:</p>
                                <ul>
                                <li>'era' - a time period corresponding to a trading day</li>
                                <li>'data_type' - indication of whether the row is part of train/test/validation/live</li>
                                </ul>
                                
                                <p>These three columns are then followed by 1050 features columns, the target column as well as 20 more target columns, and an identifier column.</p> ")
                                
                               ),
                               
                               # Show Data
                               mainPanel( 
                                 withSpinner(dataTableOutput("training")),
                                 h4("Note: Data shown is subsetted to a 0.1% sample to show structure and maintain app performance.")
                               )
                )),
                
                tabPanel("Static Analysis",
                         mainPanel(
                           tabsetPanel(
                           tabPanel("Summary Statistics",
                                    sidebarLayout(
                                      sidebarPanel(
                                        varSelectInput("sum_stat", "Distribution of Summary Statistics of the Features:", summary[2:8]),
                                      ),
                                      # Show summary table and histogram
                                      mainPanel(
                                        plotOutput("summaryplot"),
                                        DT::dataTableOutput("summary")

                                      )
                                    )
                           ),
                           tabPanel("Distributions",
                                    sidebarLayout(
                                      sidebarPanel(
                                        varSelectInput("feat", "Variable:", column_names[4:314])
                                      ),

                                      # Show histogram
                                      mainPanel(
                                        withSpinner(plotOutput("distribution1")),
                                        withSpinner(plotOutput("distribution2"))
                                      )
                                    )
                           ),
                           tabPanel("Mosaic Plot",
                                    sidebarLayout(
                                      sidebarPanel(
                                        varSelectInput("feat1", "Variable 1:", column_names[4:314]),
                                        varSelectInput("feat2", "Variable 2:", column_names[4:314]),
                                        textOutput("corr")
                                      ),

                                      # Show mosaic plot
                                      mainPanel(
                                        plotOutput("mosaicplot")
                                      )
                                    )
                           )
                         ), style='width: 1000px; height: 1000px')
                ),
                tabPanel("Analysis By Era",
                         mainPanel(
                           tabsetPanel(
                           tabPanel("Era",
                                    sidebarLayout(
                                      sidebarPanel(
                                        HTML("
                                             <p>There are a total of 120 eras: 'era1' to 'era120'.
                                             However, the data is not distributed evenly across the eras.</p>
                                             "),
                                        sliderInput("era1", "Era:", min = 1, max = 120, step=1, value = 1),
                                        textOutput("erainfo1")
                                      ),
                                    # Show era plots
                                    mainPanel(
                                      plotOutput("eraplot")
                                    ))
                           ),
                           tabPanel("Target across Era",
                                    sidebarLayout(
                                      sidebarPanel(
                                        sliderInput("era2", "Era:", min = 1, max = 120, step=1, value = 1),
                                        textOutput("erainfo2")
                                      ),
                                      # Show era plots
                                      mainPanel(
                                        plotOutput("targetmean"),
                                        plotOutput("targetdistribution")
                                      ))
                           ),
                           tabPanel("Pairwise Correlations between Features",
                                    sidebarLayout(
                                      sidebarPanel(
                                        varSelectInput("feature1", "Feature 1:", column_names[4:313]),
                                        varSelectInput("feature2", "Feature 2:", column_names[4:313]),
                                        checkboxInput("smooth1", "Smooth:", FALSE)
                                      ),

                                      # Show correlation plot
                                      mainPanel(
                                        plotOutput("corrplot1")
                                      )
                                    )
                           ),
                           tabPanel("Correlations between Features and Target",
                                    sidebarLayout(
                                      sidebarPanel(
                                        varSelectInput("feature", "Feature:", column_names[4:313]),
                                        checkboxInput("smooth2", "Smooth:", FALSE)
                                      ),

                                      # Show correlation plot
                                      mainPanel(
                                        plotOutput("corrplot2")
                                      )
                                    )
                           )

                         ), style='width: 1000px; height: 1000px' )
                )
                
))


# Define server logic
server <- function(input, output) {
  
  mydata <- reactive({
    load("data/training_data.RData")
    
    return(training_data)
  })

  output$training <- renderDataTable({
    future({ mydata() }) %...>%
      datatable(style = 'bootstrap', class = 'table-bordered',options = list(scrollX = TRUE))
  })
  
  output$summary <- DT::renderDataTable({
    DT::datatable(summary, style = 'bootstrap', class = 'table-bordered',options = list(scrollX = TRUE))
  })    
  
  output$summaryplot <- renderPlot({
      ggplot(summary, aes_string(x=input$sum_stat)) + 
        geom_histogram(fill = "#EA5600", color="grey")+
        labs(title=paste0("Distribution of ", input$sum_stat),x=input$sum_stat, y = "Frequency")        
  })
  
  output$distribution1 <- renderPlot({
    mydat <- mydata()
    if(input$feat == "target"){
      ggplot(mydat, aes(x=target))+
        geom_bar(fill = "#EA5600", color="grey")+
        scale_x_continuous(breaks = seq(0, 1, by = 0.25)) +
        labs(title=paste0("Distribution of target"))+ theme(axis.title.x = element_blank())
      
    } else {
      ggplot(mydat, aes_string(x=input$feat))+
        geom_bar(fill = "#EA5600", color="grey")+ theme(axis.title.x = element_blank())+
        scale_x_continuous(breaks = seq(0, 1, by = 0.25)) 
    }
  })
  
  output$distribution2 <- renderPlot({
    mydat <- mydata()
    mydat$target_factor <- as.factor(mydat$target)
    if(input$feat != "target"){
      ggplot(mydat, aes_string(x=input$feat, fill="target_factor"))+
        geom_bar()+ theme(axis.title.x = element_blank())+
        scale_x_continuous(breaks = seq(0, 1, by = 0.25)) +
        facet_wrap(~target_factor)
    }
  })
  
  output$corr <- renderText({
    mydat <- mydata()
      paste0("The correlation between ", input$feat1, " and ", input$feat2, " is ", 
             cor(mydat[c(which(colnames(mydat)==input$feat1),which(colnames(mydat)==input$feat2))])[1,2])
  })
  
  output$mosaicplot <- renderPlot({
    mydat <- mydata()
      mosaicplot(~eval(parse(text=input$feat1))+eval(parse(text=input$feat2)),data=mydat, main="Mosaic Plot", xlab=input$feat1, ylab=input$feat2)
  })      
  
  
  output$erainfo1 <- renderText({

        paste0("There are ", era_table$n[era_table$era==input$era1], " rows corresponding to era", input$era1, ".")
  })  
  output$erainfo2 <- renderText({
        paste0("The mean of the target in era", input$era2, "is ", target_mean_by_era$Mean[target_mean_by_era$era==input$era2], ".")
  })
  
  output$eraplot <- renderPlot({
    ggplot(data = era_table, aes(x = era, y = n, group = 1)) +
      geom_line()+
      scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
      scale_x_continuous(breaks = scales::pretty_breaks(n=15))+
      labs(title="Number of rows corresponding to each era", y="Frequency")
  })
  
  output$targetmean <- renderPlot({
    ggplot(data=target_mean_by_era, aes(x=era, y=Mean))+
      geom_line()+
      scale_x_continuous(breaks = scales::pretty_breaks(n=15))+
      labs(title = "Mean of target across eras", x="era")+
      geom_smooth(se=FALSE)
  })
  
  output$targetdistribution <- renderPlot({
    ggplot(data=target_proportion_by_era, aes(x=era, y=Proportion, color=target))+
      geom_line(aes(group=target), size=0.5)+
      labs(title="Proportion of the target across eras")+
      scale_x_continuous(breaks = scales::pretty_breaks(n=15))+
      scale_y_continuous(breaks = seq(0, 1, by = 0.1))
  })
  
  
  output$corrplot1 <- renderPlot({
    mydat <- mydata()
    
    if (input$smooth1){
      mydat %>% mutate(era = as.numeric(str_extract(era, "[^era]+$"))) %>% group_by(era) %>% 
        summarise(Cor=cor(eval(parse(text=input$feature1)),eval(parse(text=input$feature2)))) %>%
      ggplot(aes(x=era, y=Cor))+
        geom_line()+
        labs(title=paste0("Correlation of ",input$feature1," and ",input$feature2 ," across era"), y="Correlation")+
        scale_y_continuous(breaks = scales::pretty_breaks(n=10))+scale_x_continuous(breaks = scales::pretty_breaks(n=15))+geom_smooth(se=FALSE)       
    } else{
      mydat %>% mutate(era = as.numeric(str_extract(era, "[^era]+$"))) %>% group_by(era) %>% 
        summarise(Cor=cor(eval(parse(text=input$feature1)),eval(parse(text=input$feature2)))) %>%
        ggplot(aes(x=era, y=Cor))+
        geom_line()+
        labs(title=paste0("Correlation of ",input$feature1," and ",input$feature2 ," across era"), y="Correlation")+
        scale_y_continuous(breaks = scales::pretty_breaks(n=10))+scale_x_continuous(breaks = scales::pretty_breaks(n=15))
    }
    
  })
  
  output$corrplot2 <- renderPlot({
    if (input$smooth2){
      ggplot(data=correlations_by_era, aes_string(x="era", y=input$feature))+
        geom_line()+
        labs(title=paste0("Correlation of ", input$feature ," and the target across era"), y="Correlation")+
        scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
        scale_x_continuous(breaks = scales::pretty_breaks(n=15))+geom_smooth(se=FALSE)            
      
    } else {
      ggplot(data=correlations_by_era, aes_string(x="era", y=input$feature))+
        geom_line()+
        labs(title=paste0("Correlation of ", input$feature ," and the target across era"), y="Correlation")+
        scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
        scale_x_continuous(breaks = scales::pretty_breaks(n=15)) 
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
