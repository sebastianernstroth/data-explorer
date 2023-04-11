library(shiny)
library(rprojroot)

root <- rprojroot::is_rstudio_project

appDir <- root$find_file("code")

runApp(appDir = appDir)
