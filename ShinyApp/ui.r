#####################################################################################################
# Duke University UCD Shiny App, Feb 2020
# Shiny user interface
#####################################################################################################

# Information on shiny and visnetwork available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf
# https://cran.r-project.org/web/packages/visnetw ork/visnetwork.pdf

library(shiny)
library(shinythemes)
library(visNetwork)
library(DT)

# Dir location
dr <- c("local"="C:\\Projects\\Duke\\Nursing\\SemanticsOfRareDisease\\UCD\\UCDApp",
        "VM"="C:\\Projects\\tjb48\\UCDApp",
        "cloud"="")[1]
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

      # Concept query prompt
      fluidRow(
        column(width=10,
          sidebarPanel(width="100%",
            fluidRow(
              column(width=12,
                HTML("<b>1. Query<br><br><b>"),
                div(HTML("<b>Current Concept Root:&nbsp;<b>"), style="display:inline-block;vertical-align:top"),
                div(htmlOutput("currentRoot"), style="display:inline-block;vertical-align:top")
              )
            ),
            fluidRow(
              column(width=7,
                div(HTML("<b>Select sub-concept&nbsp&nbsp<b>"), style="display:inline-block;vertical-align:top;margin-top:25px"),
                div(selectInput("conceptSel", "", choices="", width="400px"), style="display:inline-block;vertical-align:top"),
                div(actionButton("retParentConcept", "<- parent"), style="display:inline-block;vertical-align:top;margin-left:10px;margin-top:20px"),
                div(actionButton("queryConcept", "query"), style="display:inline-block;vertical-align:top;margin-left:35px; margin-top:20px")
              )
            )
          )
        )
      ),

      # Variable connector prompts
      fluidRow(
        column(width=5,
          # Participant variable connection prompts
          sidebarPanel(width="100%",
            HTML("<b>2. Specify variables to connect (participant)</b><br><br>"),
            fluidRow(
              column(width=12,
                checkboxGroupInput("cnUCDProxDist", "UCDProxDist",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "Rx", "findingSiteFSN"),
                  inline=T))
              #column(width=2, div(actionButton("toggleUCDProxDist", "toggle"), style="margin-top:15px"))
              #column(width=2, div(checkboxInput("interactUCDProxDist", "Interact"), style="margin-left:20px; margin-top:25px"))
            ),
            fluidRow(
              column(width=12,
                checkboxGroupInput("cnSex", "Sex",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "Rx", "findingSiteFSN"),
                  inline=T))
              #column(width=2, div(actionButton("toggleSex", "toggle"), style="margin-top:15px"))
              #column(width=2, div(checkboxInput("interactSex", "Interact"), style="margin-left:20px; margin-top:25px"))
            ),
            fluidRow(
              column(width=12,
                checkboxGroupInput("cnUCDDx", "UCDDx",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "Rx", "findingSiteFSN"),
                  inline=T))
              #column(width=2, div(actionButton("toggleUCDDx", "toggle"), style="margin-top:15px"))
              #column(width=2, div(checkboxInput("interactUCDDx", "Interact"), style="margin-left:20px; margin-top:25px"))
            ),
            fluidRow(
              column(width=12,
                checkboxGroupInput("cnAge", "Age (1, 10, 100, 1,000 day(s))",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "Rx", "findingSiteFSN"),
                  inline=T))
              #column(width=2, div(actionButton("toggleAge", "toggle"), style="margin-top:15px"))
              #column(width=2, div(checkboxInput("interactAge", "Interact"), style="margin-left:20px; margin-top:25px"))
            ),
            fluidRow(
              column(width=12,
                checkboxGroupInput("cnHASxLast", "HASxLast",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "Rx", "findingSiteFSN"),
                  inline=T))
              #column(width=2, div(actionButton("toggleHASxLast", "toggle"), style="margin-top:15px"))
              #column(width=2, div(checkboxInput("interactHASxLast", "Interact"), style="margin-left:20px; margin-top:25px"))
            )
          )

        ),

        column(width=5,

          # Rx, concept, finding site connection prompts
          sidebarPanel(width="100%",
            HTML("<b>Variables to connect (SNOMEDCT, Rx, finding site)</b><br><br>"),
            fluidRow(
              column(width=12,
                checkboxGroupInput("cnConceptFSN", "SNOMEDCT concept",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "Rx", "findingSiteFSN"),
                  inline=T))
              #column(width=2, div(actionButton("toggleconceptFSN", "toggle"), style="margin-top:15px"))
              #column(width=2, div(checkboxInput("interactconceptFSN", "Interact"), style="margin-left:20px; margin-top:25px"))
            ),
            HTML("<hr>"),
            fluidRow(
              column(width=12,
                checkboxGroupInput("cnRx", "Rx",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "Rx", "findingSiteFSN"),
                  inline=T))
              #column(width=2, div(actionButton("toggleRx", "toggle"), style="margin-top:15px"))
              #column(width=2, div(checkboxInput("interactRx", "Interact"), style="margin-left:20px; margin-top:25px"))
            ),
            checkboxInput("rxSubsume", "Subsume Rx", value=F),
            HTML("<hr>"),
            fluidRow(
              column(width=12,
                checkboxGroupInput("cnFindingSite", "Finding site",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "Rx", "findingSiteFSN"),
                  inline=T))
              #column(width=2, div(actionButton("toggleFindingSite", "toggle"), style="margin-top:15px"))
              #column(width=2, div(checkboxInput("interactFindingSite", "Interact"), style="margin-left:20px; margin-top:25px"))
            )
          )

        )

      ),

      # Variable interaction (combinitaion) prompts
      fluidRow(

        column(width=5,
          sidebarPanel(width="100%",
            HTML("<b>3. Specify variables to interact<br><br>"),
            fluidRow(
              column(width=12,
                checkboxGroupInput("interactSet1", "Interaction set 1",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "Rx", "findingSiteFSN"),
                  inline=T),
                checkboxGroupInput("interactConn1", "Connected to",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "Rx", "findingSiteFSN"),
                  inline=T)
              )
            )
          )
        ),
        column(width=5,
          sidebarPanel(width="100%",
            HTML("<b>Specify variables to interact<br><br>"),
            fluidRow(
              column(width=12,
                checkboxGroupInput("interactSet2", "Interaction set 2",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "Rx", "findingSiteFSN"),
                  inline=T),
                checkboxGroupInput("interactConn2", "Connected to",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "Rx", "findingSiteFSN"),
                  inline=T)
              )
            )
          )
        )
      ),

      # Graph configuration controls and graph rendering panel
      fluidRow(

        # Graph configuration controls
        column(width=2,
          sidebarPanel(width="100%",
            HTML("<b>4. Adjust<br><br></b>"),
            #sliderInput("log_10_p", HTML("log<sub>10</sub>(p) min filter"), min=4, max=12, value=5.5, step=0.25),
            sliderInput("nedgemin", "Vertex n-edge (min) filter", min=0, max=100, value=0, step=1),
            sliderInput("eopacity", "Edge opacity", min=0, max=1, value=0.35, step=0.05),
            sliderInput("nCluster", HTML("Clustering<sub>n</sub>"), min=0, max=20, step=1, value=0),
            radioButtons("physics", "Physics", choiceNames=c("on", "off"), choiceValues=c(T, F), selected=F, inline=T),
            sliderInput("vMassFactor", "Vertex mass factor", min=0, max=2, value=0.25, step=0.05),
            sliderInput("vSizeFactor", "Vertex size factor", min=0, max=0.5, step=0.01, value=0.25),
            sliderInput("vFontSize", "Vertex label font size", min=6, max=36, value=16, step=1),
            sliderInput("nearestHighlightDeg", "Node nearest highlight degree", min=0, max=10, value=1, step=1)
          )
        ),

        #column(width=2,
        #  # Prompts, panel three
        #  sidebarPanel(width="100%",
        #    HTML("<i>use shift-click to subnet a vertex</i>"),
        #    div(actionButton("regen", "Regenerate graph"), style="margin-top: 20px"),
        #    div(actionButton("restoreVertex", "Restore after subnet"), style="margin-top: 5px"),
        #    div(actionButton("redrawEdge", "Redraw edges"), style="margin-top: 5px"),
        #    # Hidden reactive fields
        #    # These are used by functions in server() to direct activity based on current state(s) of the graph
        #    # Note that the first conditionalPanel() parameter ("false") is a java expression
        #    conditionalPanel(condition="false",
        #                     textInput("reactiveInst", "reactiveInst", value=""),
        #                     textInput("renderInst", "renderInst", value="render"))
        #  )
        #)

        column(width=10,

          fluidRow(
            column(width=4,
              sidebarPanel(width=11,
                HTML("<b>5. Explore<br><br></b>"),
                fluidRow(
                  column(width=6,
                    div(HTML("<i>Alt-click node selection</i><br>"), style="text-align:left; margin-bottom: 15px"),
                    fluidRow(
                      column(width=4, actionButton("altClickSubnet", "subnet")),
                      column(width=4, actionButton("altClickDescend", "expand"))
                    )
                  ),
                  column(width=6,
                    div(HTML("<i>Restore edges</i><br>"), style="text-align:left; margin-bottom: 15px"),
                    fluidRow(
                      column(width=4, actionButton("restoreEdgesPostMove", "after move"))
                    )
                  )
                )
              )
            )
          ),

          # Graph
          visNetworkOutput("g1", width="100%", height="1000px")

          # Centrality table
          #column(width=2,
          #  div(DT::dataTableOutput("gTable"), style="align:center; margin-right:20px")
          #)

        )

      ),

      style="margin-left: 20px"

    )

  )
)