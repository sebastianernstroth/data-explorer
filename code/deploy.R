library(rsconnect)
library(rprojroot)

root <- rprojroot::is_rstudio_project

appDir <- root$find_file("code")

appName <- basename(root$find_file())

recordDir <- root$find_file()

deployApp(appDir = appDir,
          appName = appName,
          recordDir = recordDir)
