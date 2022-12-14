# Elo_shiny_app
A shiny app for creating and optimizing Elo ranking systems from any sport or league 
 
By Simon D. Weaver

Online version can be found at  
https://sweaver4.shinyapps.io/EloCreationOptimization/

To run locally in R:  

 * Download and install [R](https://www.r-project.org/) and optionally (but encouraged), [RStudio](https://www.rstudio.com/products/rstudio/download/).
 * Either clone this directory, or download a zip file containing its contents by navigating to [https://github.com/weaversd/Elo_shiny_app/zipball/main](https://github.com/weaversd/Elo_shiny_app/zipball/main). If necessary, unzip this file. The location of this on your local computer is the 'App directory'.
 * Open R (or RStudio if installed).
 * Ensure that the shiny package is loaded by running `install.packages("shiny")` and active `library(shiny)` in the R console.
 * Ensure that all of the packages you need are installed on your computer. The required packages can be found in 'global.R'. You can either install them manually, or run this code in the R console:
 ```
 install.packages(c("stringr", "ggplot2", "plotly", "scales", "dplyr", "tidyr", "shinycssloaders", "markdown"))
 ```  
 * Change your working directory to the path of the cloned or downloaded app directory `setwd('path/to/directory')`
 * In Rstudio, open either 'ui.R' or 'Server.R'.
 * A 'Run App' button should appear in the top right of the '.R' file that you just opened. Click this to start a local version of the app. 
 * Alternatively, you can launch the app directly from the R console: `runApp("path/to/appDirectory")`. If you already set the working directory to this path, you can simply run: `runApp()`.
 * If there are errors, you may not have all the correct packages installed. Open the 'global.R' file and ensure that all the packages used are in your 'packages' tab.
