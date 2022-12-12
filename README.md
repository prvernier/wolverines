# Wolverine survey design

This repository contains code, data, and an app to facilitate the design of a wolverine survey in the Yukon.

The app can be run from a local machine using the following steps:

  1. Install R (download from r-project.org and follow instructions)
  2. Install the following additional packages:

    install.packages(c("sf","DT","tmap","dplyr","terra","leaflet","shiny","shinydashboard", "magrittr", "tidyverse", "shinybusy"))

  3. Start the Shiny app:

    shiny::runGitHub("prvernier/wolverines")
