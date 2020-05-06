#####################################################################################################
# Duke University UCD Shiny App, Feb 2020
# Shiny user interface
#####################################################################################################

# Information on shiny and visnetwork available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf
# https://cran.r-project.org/web/packages/visnetwork/visnetwork.pdf

library(shiny)
library(shinythemes)
library(visNetwork)
library(DT)

# Dir location
dr <- c("local"="C:\\Projects\\UCDApp", "cloud"="")[1]
setwd(dr)

shinyUI(
  fluidPage(

    includeCSS("style.css"),
    title="UCD",

    # Use a div to provide a slight left margin
    div(
      HTML("<h3>UCD SNOMEDCT-Participant Exploration App</h3><br><br>"),
      style="margin-left: 30px"
    ),

    div(
      fluidRow(

        # Prompts, panel one
        column(width=3,

          sidebarPanel(width="100%",
            HTML("<b>Current Concept Root</b><br>"),
		        htmlOutput("currentRoot"),
            HTML("<br>"),
            actionButton("retParentConcept", "<- Parent concept"),
            HTML("<br><br>"),
            selectInput("conceptSel", "Sub-concept selection", choices=""),
            radioButtons("nodeCat", "Node category", choices=c("UCDProx", "Sex", "UCDDx", "Age"), selected="UCDProx", inline=T),
            actionButton("renderGraph", "Render")
            #HTML("<br><br>")
          ),

          # Prompts, panel two
          sidebarPanel(width="100%",
            #sliderInput("log_10_p", HTML("log<sub>10</sub>(p) min filter"), min=4, max=12, value=5.5, step=0.25),
            sliderInput("nedgemin", "Vertex n-edge (min) filter", min=0, max=100, value=0, step=1),
            radioButtons("physics", "Physics", choiceNames=c("on", "off"), choiceValues=c(T, F), selected=F, inline=T),
            sliderInput("eopacity", "Edge opacity", min=0, max=1, value=0.35, step=0.05),
            sliderInput("nCluster", HTML("Clustering<sub>n</sub>"), min=0, max=20, step=1, value=0)
          ),

          # Prompts, panel three
          sidebarPanel(width="100%",
            HTML("<i>use shift-click to subnet a vertex</i>"),
            div(actionButton("regen", "Regenerate graph"), style="margin-top: 20px"),
            div(actionButton("restoreVertex", "Restore after subnet"), style="margin-top: 5px"),
            div(actionButton("redrawEdge", "Redraw edges"), style="margin-top: 5px"),
            # Hidden reactive fields
            # These are used by functions in server() to direct activity based on current state(s) of the graph
            # Note that the first conditionalPanel() parameter ("false") is a java expression
            conditionalPanel(condition="false",
                             textInput("reactiveInst", "reactiveInst", value=""),
                             textInput("renderInst", "renderInst", value="render"))

          )
        
        ),

        # Graph
        column(width=9, visNetworkOutput("g1", width="100%", height="900px"))

        # Centrality table
        #column(width=2,
        #  div(DT::dataTableOutput("gTable"), style="align:center; margin-right:20px")
        #)

      ),

      style="margin-left: 20px"

    )

  )
)