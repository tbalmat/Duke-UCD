# Duke University UCD shiny App, Feb 2020

# Launch app defined in ui.r and server.r in specified appDir
# Note the specification of a tcp port that the process will listen on for http requests

library("shiny")
rm(list=ls())
gc()

# Specify directory containing ui.r and server.r
ad <- c("local"="C:\\Projects\\Duke\\Nursing\\SemanticsOfRareDisease\\UCD\\UCDApp",
        "server"="C:\\Projects\\UCDApp",
        "cloud"="")[1]

# Create ui and server functions
#server <- function(input, output, session) source(paste(ad, "\\server.r", sep=""), local=T, echo=F)
#ui <- function(req) source(paste(ad, "\\ui.r", sep=""), local=T, echo=F)

# Launch
#runApp(appDir=list("ui"=ui, "server"=server),
runApp(appDir=ad,
       launch.browser=F,
       host = getOption("shiny.host", "127.0.0.1"),
       port=4302,
       display.mode = c("auto", "normal", "showcase"))
