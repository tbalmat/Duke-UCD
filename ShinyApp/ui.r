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
        "VM"="C:\\Projects\\UCDApp",
        "cloud"="")[1]
setwd(dr)

shinyUI(
  fluidPage(

    includeCSS("style.css"),
    title="UCD",

    # Reposition and alter appearance of notification window
    tags$head(
      tags$style(
        HTML(".shiny-notification {font-size:20px; color:red; font-style:bold; width:50%; position:fixed; top:calc(50%); left:calc(25%)}")
        )
    ),

    # Use a div to provide a slight left margin
    div(
      HTML("<h3>UCD SNOMEDCT-Participant Exploration App</h3><br><br>"),
      style="margin-left: 30px"
    ),

    div(

      # Concept selection prompts
      fluidRow(
        column(width=10,
          sidebarPanel(width="100%",
            fluidRow(
              column(width=8,
                HTML("<b>1. Explore concepts</b>")
              ),
              column(width=4,
                div(HTML("<b>Concept path finder&nbsp</b>"), style="display:inline-block;vertical-align:top;margin-top:4px"),
                div(textInput("conceptFinderText", ""), style="width:300px; display:inline-block; margin-top:-25px"),
                htmlOutput("conceptFinderPath")
              )
            ),
            fluidRow(
              column(width=8,
                div(HTML("<b>Concept root:&nbsp;</b>"), style="display:inline-block;vertical-align:top; margin-left:16px; margin-top:-10px"),
                div(htmlOutput("exploreCurrRootPath"), style="display:inline-block;vertical-align:top; margin-left:16px; margin-top:-10px")
              )
            ),
            fluidRow(
              column(width=12,
                div(HTML("<b>Explore sub-concepts&nbsp&nbsp</b>"), style="display:inline-block; vertical-align:top; margin-left:16px; margin-top:10px"),
                div(selectInput("exploreChoices", "", choices="", width="750px", selectize=F, size=16),
                    style="display:inline-block; vertical-align:top; margin-top:-15px")
                #div(actionButton("retParentConcept", "<- parent", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)"), style="display:inline-block; vertical-align:top; margin-left:10px; margin-top:5px")
              )
            ),
            fluidRow(column(width=12, div(HTML("<hr>")), style="margin-top:-15px")),
            fluidRow(
              div(
                column(width=3,
                  HTML("<b>2. Select concepts (one of a, b, or c)</b><br>")
                ),
                column(width=5,
                  div(HTML("<b>Selected concepts</b>"), style="margin-left:110px")
                ),
                style="margin-top:5px"
              )
            ),
            fluidRow(
              div(
                column(width=3,
                  div(HTML("<b>a.</b>&nbsp"), style="display:inline-block; vertical-align:top; margin-left:16px; margin-top:5px"),
                  div(actionButton("conceptSelect", "select current (root) concept", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)"), style="display:inline-block; vertical-align:top; margin-left:5px"),
                  div(actionButton("conceptSelectClear", "clear all", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)"), style="display:inline-block; vertical-align:top; margin-left:10px")
                ),
                column(width=5,
                  div(htmlOutput("queryConceptFSN"), style="margin-left:110px")
                ),
                style="margin-top:15px"
              )
            ),
            fluidRow(
              div(
                column(width=12,
                  div(HTML("<b>b.&nbsp;&nbsp; Concept ID(s)"), style="display:inline-block; vertical-align:top; margin-left:16px; margin-top:10px"),
                  div(textInput("queryConceptID", label="", width="300px"), style="display:inline-block; vertical-align:top; margin-left:28px; margin-top:-15px")
                ),
                style="margin-top:15px"
              )
            ),
            fluidRow(
              div(
                column(width=4,
                 div(HTML("<b>c.&nbsp;&nbsp; FSN keyword(s)"), style="display:inline-block; vertical-align:top; margin-left:16px; margin-top:10px"),
                 div(textInput("queryFSNKeyword", label="", width="300px"), style="display:inline-block; vertical-align:top; margin-left:10px; margin-top:-15px")
                ),
                column(width=5,
                 div(radioButtons("queryFSNKeywordStyle", "", choices=c("exact", "lead", "contains"), selected="exact", inline=T), style="display:inline-block; vertical-align:top; margin-left:-28px; margin-top:-10px"),
                 div(radioButtons("queryFSNKeywordOp", "", choices=c("or", "and"), selected="or", inline=T), style="display:inline-block; vertical-align:top; margin-left:35px; margin-top:-10px")
                ),
                style="margin-top:0px"
              )
            ),
            fluidRow(column(width=12, div(HTML("<hr>")), style="margin-top:-15px")),
            fluidRow(
              div(
                column(width=8,
                  div(HTML("<b>3.</b>"), style="display:inline-block; vertical-align:top; margin-top:5px"),
                  div(actionButton("queryConcepts", "query", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)"), style="display:inline-block; vertical-align:top; margin-left:10px; margin-top:0px")
                ),
                style="margin-top:0px"
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
            HTML("<b>4. Specify variables to connect (participant)</b><br><br>"),
            fluidRow(
              column(width=12,
                checkboxGroupInput("cnUCDProxDist", "UCDProxDist",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptID", "rxID", "findingSiteID"),
                  inline=T)
              )
            ),
            fluidRow(
              column(width=12,
                checkboxGroupInput("cnSex", "Sex",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptID", "rxID", "findingSiteID"),
                  inline=T)
              )
            ),
            fluidRow(
              column(width=12,
                checkboxGroupInput("cnUCDDx", "UCDDx",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptID", "rxID", "findingSiteID"),
                  inline=T)
              )
            ),
            fluidRow(
              column(width=12,
                checkboxGroupInput("cnAge", "Age (1, 10, 100, 1,000 day(s))",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptID", "rxID", "findingSiteID"),
                  inline=T)
              )
            ),
            fluidRow(
              column(width=11,
                checkboxGroupInput("cnHASxLast", "HASxLast",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptID", "rxID", "findingSiteID"),
                  inline=T)
              ),
              column(width=1,
                div(checkboxInput("groupHA", "group", value=F), style="margin-left:-30px; margin-top:25px")
              )
            )
          )

        ),

        column(width=5,

          # Rx, concept, finding site connection prompts
          sidebarPanel(width="100%",
            HTML("<b>4. Variables to connect (SNOMEDCT, Rx, finding site)</b><br><br>"),
            fluidRow(
              column(width=11,
                checkboxGroupInput("cnConceptID", "SNOMEDCT concept",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptID", "rxID", "findingSiteID"),
                  inline=T)
              ),
              column(width=1,
                div(checkboxInput("joinConcept", "join", value=F), style="margin-left:-30px; margin-top:25px")
              )
            ),
            HTML("<hr>"),
            fluidRow(
              column(width=12,
                checkboxGroupInput("cnRx", "Rx",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptID", "rxID", "findingSiteID"),
                  inline=T)
              )
            ),
            checkboxInput("rxSubsume", "Subsume Rx", value=F),
            HTML("<hr>"),
            fluidRow(
              column(width=12,
                checkboxGroupInput("cnfindingSite", "Finding site",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptID", "rxID", "findingSiteID"),
                  inline=T)
              )
            )
          )

        )

      ),

      # Variable interaction (combination) prompts
      fluidRow(

        column(width=5,
          sidebarPanel(width="100%",
            HTML("<b>5. Specify variables to interact</b><br><br>"),
            fluidRow(
              column(width=12,
                checkboxGroupInput("interactSet1", "Interaction set 1",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptID", "rxID", "findingSiteID"),
                  inline=T),
                checkboxGroupInput("interactConn1", "Connected to",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptID", "rxID", "findingSiteID"),
                  inline=T)
              )
            )
          )
        ),
        column(width=5,
          sidebarPanel(width="100%",
            HTML("<b>5. Specify variables to interact</b><br><br>"),
            fluidRow(
              column(width=12,
                checkboxGroupInput("interactSet2", "Interaction set 2",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptID", "rxID", "findingSiteID"),
                  inline=T),
                checkboxGroupInput("interactConn2", "Connected to",
                  choiceNames=c("UCDProxDist", "Sex", "UCDDx", "Age", "HASxLast", "Concept", "Rx", "FndSt"),
                  choiceValues=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptID", "rxID", "findingSiteID"),
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
            HTML("<b>6. Adjust</b><br><br></b>"),
            #sliderInput("log_10_p", HTML("log<sub>10</sub>(p) min filter"), min=4, max=12, value=5.5, step=0.25),
            sliderInput("nedgemin", "Vertex n-edge (min) filter", min=0, max=100, value=0, step=1),
            sliderInput("eopacity", "Edge opacity", min=0, max=1, value=0.35, step=0.05),
            sliderInput("nCluster", HTML("Clustering<sub>n</sub>"), min=0, max=20, step=1, value=0),
            radioButtons("physics", "Physics", choiceNames=c("on", "off"), choiceValues=c(T, F), selected=F, inline=T),
            sliderInput("vMassFactor", "Vertex mass factor", min=0, max=2, value=0.25, step=0.05),
            sliderInput("vSizeFactor", "Vertex size factor", min=0, max=0.5, step=0.01, value=0.25),
            sliderInput("vFontSize", "Vertex label font size", min=6, max=36, value=16, step=1),
            sliderInput("nearestHighlightDeg", "Node nearest highlight degree", min=0, max=10, value=1, step=1),
            radioButtons("renderGeometry", "Render geometry", choices=c("free", "columnar", "radial"), inline=T),
            sliderInput("renderScaleX", "Render x scale (x 100)", min=0, max=10, value=0.5, step=0.25),
            sliderInput("renderScaleY", "Render y scale (x 100)", min=0, max=10, value=0.5, step=0.25)
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
                HTML("<b>7. Explore</b><br>"),
                div(HTML("<i>Shift-click, Alt-click to select/deselect<br></i>"), style="margin-left:0px; margin-top:10px"),
                fluidRow(
                  div(
                    column(width=12,
                      div(actionButton("nodeSubnet", "subnet", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)"), style="display:inline-block; vertical-align:top"),
                      div(actionButton("nodeExpand", "expand", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)"), style="display:inline-block; vertical-align:top"),
                      div(actionButton("nodeNeighborhood1", "neighborhood-1", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)"), style="display:inline-block; vertical-align:top"),
                      div(actionButton("nodeRestorePrevious", "back", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)"), style="display:inline-block; vertical-align:top")
                    ),
                    style="margin-top:10px"
                  )
                ),
                fluidRow(
                  div(
                    column(width=12,
                      div(actionButton("restoreEdgesPostMove", "redraw edges", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)"), style="display:inline-block; vertical-align:top; color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)")
                    )
                  ),
                  style="margin-top:10px"
                )
              )
            )
          ),

          # Rx leading character filter
          fluidRow(width=10,
            column(width=4,
              sidebarPanel(width=11,
                textInput("rxLeadCharFilter", "Rx leading character filter (comma separated values)")
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