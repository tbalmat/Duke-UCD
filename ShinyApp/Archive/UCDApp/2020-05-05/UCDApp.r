# Duke University UCD shiny App, Feb 2020

# Launch app defined in ui.r and server.r in specified appDir
# Note the specification of a tcp port that the process will listen on for http requests

library("shiny")

# Specify directory containing ui.r and server.r
ad <- c("local"="C:\\Projects\\Duke\\Nursing\\SemanticsOfRareDisease\\UCD\\UCDApp",
        "server"="C:\\Projects\\tjb48\\UCDApp",
        "cloud"="")[1]
# Execute 
runApp(appDir=ad,
       launch.browser=T,
       host = getOption("shiny.host", "127.0.0.1"),
       port=4301,
       display.mode = c("auto", "normal", "showcase"))
