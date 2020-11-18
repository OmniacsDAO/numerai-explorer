<a href="https://omnianalytics.io" target="_blank"><img src="images/omni_numerai.png" align="right"/></a>

# Numerai Tournament Training Data Explorer
> R Shiny Web Interface for Exploring Numerai Tournament Training Data

Numerai is a crowdsourced hedgefund that hosts machine learning tournaments which attract thousands of data scientists around the world to compete for 
Numeraire cryptocurrency. The company provides clean, regularized, and obfuscated data, where anyone with expertise in machine learning can freely participate.

You can use this app to explore the data before diving into any model! Here is a snapshot of what the app looks like in action:

<a href="https://crypto.omnianalytics.io/apps/numerai/" target="_blank"><img src="images/numerexplorer_screenshot.png"/></a>

# Running the App

We are hosting our own instance of the app at the following URL:

https://crypto.omnianalytics.io/apps/numerai/
    
For optimal performance, you can run your own instance of the app. This requires R 4.3.x and associated R packages.

## Installing Dependencies

    ## Install CRAN dependencies
    install.packages("shiny")
    install.packages("shinyWidgets")
    install.packages("shinythemes")
    install.packages("shinycssloaders")
    install.packages("dplyr")
    install.packages("ggfortify")
    install.packages("DT")
    install.packages("stringr")
    
## Running with RStudio
    
If you use RStudio, you can clone or download the repository, and open the app.R file. A "Run App" button will appear at the top of RStudio. Clicking this will run the app. Note that by default, the app will open in a small pop-up window. You can click the Open in Browser link at the top to open it in your browser of choice, instead.

## Running with R

If you use R or another R GUI other than RStudio, run the following lines to execute the dashboard:

    ## Set the working directory containing the folder of the app code
    setwd("~/Work")
    
    ## Run the app using the name of the folder containing the app code 
    runApp("numerai")
    

    
