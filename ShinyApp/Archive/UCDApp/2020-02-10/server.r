#####################################################################################################
# Duke University UCD Shiny App, Feb 2020
# Shiny server script
#####################################################################################################

# Information on shiny and visnetwork available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf
# https://cran.r-project.org/web/packages/visnetwork/visnetwork.pdf

#####################################################################################################
# Data source:  UCD database developed by University of Nebraska and Duke University
#####################################################################################################

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
#options(device="windows")

library(shiny)
library(visNetwork)
library(RNeo4j)
library(DT)

# Dir location
dr <- c("local"="C:\\Projects\\UCDApp", "cloud"="")[1]
setwd(dr)

##########################################################################################################
# Set static filter and appearance parameters
##########################################################################################################

# Vertex colors (one for participant groups, one for concepts)
vc1 <- "#66AAFF"
vc2 <- "#FFEE66"

# Edge colors
ec1 <- "#080808"
ec2 <- "#C02020"

# Vertex and edge font sizes
vfsz <- 8
efsz <- 8

# Font stroke (outline) color
fsc1 <- "#909090"

# Vertex rendering scale factor
vsizefactor <- 1

##########################################################################################################
# Conect to DB
##########################################################################################################

graph <- startGraph("http://localhost:1105/db/data/", username="neo4j", password="Duke123!")

##########################################################################################################
# Function to assemble graph components (vertices and edges)
##########################################################################################################

assembleNetComponents <- function(prop, concept) {

  # Compose graph data set, with participant groups and concepts as vertices and relationship of participant property and concept as edges

  if(nrow(pConcept)>0) {
    # Tabulate edges by concept and participant property
    edat <- aggregate(1:nrow(pConcept), by=list(pConcept[,concept], pConcept[,prop]), length)
    colnames(edat) <- c("concept", "prop", "n")
    # Tabulate vertices from edge data
    # Vertex set 1 (v1) for concepts, set 2 (v2) for participant groups (by property)
    v1 <- aggregate(1:nrow(edat), by=list(edat[,concept]), length)
    colnames(v1) <- c("lab", "n")
    v2 <- aggregate(1:nrow(edat), by=list(edat[,prop]), length)
    colnames(v2) <- c("lab", "n")
    # Filter by edge count
    # Retain vertices with min edge count, retain all enjoined vertices
    v1 <- subset(v1, n>=nedgemin)
    v2 <- subset(v2, n>=nedgemin)
    edat <- edat[which(edat[,"concept"] %in% v1[,"lab"] | edat[,"prop"] %in% v2[,"lab"]),]
    # Compose edge hover labels
    if(nrow(edat)>0) {
      edat[,"hovtext"] <- paste(edat[,"concept"], "; ", edat[,"prop"], "; n = ", edat[,"n"], sep="")
      # Compose vertex sets from pConcept edges (at this stage, pConcept contains edges where at least
      # one vertex has number of edges at threshold)
      v1 <- aggregate(1:nrow(edat), by=list(edat[,"concept"]), length)
      colnames(v1) <- c("lab", "n")
      v2 <- aggregate(1:nrow(edat), by=list(edat[,"prop"]), length)
      colnames(v2) <- c("lab", "n")
      # Assign vertex color by vertex set, edge color static
      vcolor <- c(rep(vc1, nrow(v1)), rep(vc2, nrow(v2)))
      vtcolor <- vcolor
      ecolor <- ec1
      ehcolor <- ec2
      vertex0 <- data.frame("set"=c(rep(1, nrow(v1)), rep(2, nrow(v2))), "v"=c(v1[,"lab"], v2[,"lab"]),
                            "lab"=c(v1[,"lab"], v2[,"lab"]), "n"=c(v1[,"n"], v2[,"n"]), "hovtext"=c(v1[,"lab"], v2[,"lab"]))
    } else {
      vertex0 <- data.frame()
    }
  } else {
    vertex0 <- data.frame()
  }

  # Compose global vertex and edge sets
  if(nrow(vertex0)>0) {
    # Vertices
    vertex <<- data.frame("id"=1:(nrow(vertex0)),
                          "fixed"=F,
                          "label"=vertex0[,"v"],
                          "color"=vcolor,
                          "font"=list("color"=vtcolor, "size"=vfsz, strokeWidth=1, "strokeColor"=fsc1),
                          "value"=vsizefactor*vertex0[,"n"]/max(vertex0[,"n"], na.rm=T),
                          "title"=vertex0[,"hovtext"])
    # Include groups for legend configuration
    vertex[,"group"] <<- c("Concept", prop)[vertex0[,"set"]]
    rownames(vertex) <<- NULL
    # Compose vertex IDs (they are required for unambiguous identification in edge construction)
    vid <-setNames(vertex[,"id"], vertex[,"label"])
    # Compose edges
    if(nrow(edat)>0) {
      edge <<- data.frame("from"=vid[edat[,"concept"]],
                          "to"=vid[edat[,"prop"]],
                          "label"=paste("n = ", edat[,"lab"], sep=""), 
                          # Hover text
                          "title"=gwas[,"hovtext"],
                          "hoverWidth"=0,
                          "selectionWidth"=0,
                          "color"=list("color"=ecolor, "opacity"=eopacity, "highlight"=ehcolor),
                          "font"=list("color"="white", "size"=efsz, strokeWidth=1, "strokeColor"=fsc1),
                          #"length"=20,
                          "physics"=T,
                          "smooth"=T)
    } else {
      edge <<- data.frame()
    }

  } else {
    vertex <<- data.frame()
    edge <<- data.frame()
  }

  print("net assembled")

}

##########################################################################################################
# Function to compose graph using visNetwork() functions
##########################################################################################################

composeNet <- function() {
  g <- visNetwork(vertex, edge) %>% 
         visGroups(groupname="GWAS 1", color=vc1, font=list("color"="white", "size"=12)) %>%
         visGroups(groupname="GWAS 2", color=vc2, font=list("color"="#202020", "size"=12)) %>%
         visLegend(useGroups=T, position="right") %>%
         visOptions(highlightNearest=list("enabled"=T, "hover"=T)) %>%
         visInteraction(hover=T, hoverConnectedEdges=T, navigationButtons=T) %>%
         visPhysics(timestep=0.25, minVelocity=10, maxVelocity=50, 
                    barnesHut=list("avoidOverlap"=0.5, "springLength"=200, "springConstant"=0.5, "damping"=0.5),
                    repulsion=list("nodeDistance"=100),
                    stabilization=list("enabled"=T, "iterations"=1000)) %>%
         # Enclose java functions in {} brackets, otherwise they hang with no message 
         #visEvents(type="once", startStabilizing="function() {
         #                                           alert('begin stabilization')
         #                                         }") %>%
         visEvents(type="once", stabilized="function() {
                                              //alert('stab')
                                              Shiny.onInputChange('stabilized', '0')
                                            }") %>%
         # Double click events fire two click events, so use shift-click for doubles
         visEvents(type="on", click="function(obj) {
                                       if(obj.event.srcEvent.shiftKey) {
                                         //alert('shift-click')
                                         Shiny.onInputChange('shiftClick', obj)
                                       } else {
                                         //alert('click')
                                         Shiny.onInputChange('click', obj)
                                       }
                                     }")
         #visEvents(type="on", doubleClick="function(obj) Shiny.onInputChange('doubleClick', obj)")

  # Cluster, if requested
  if(nCluster>0)
    g <- g %>% visClusteringByHubsize(size=nCluster)

  print("net composed")
  return(g)
}

##########################################################################################################
# Shiny server function
##########################################################################################################

shinyServer(
  function(input, output, session) {

    # Configure global objects

    # conceptStack contains the stack of concepts chosen by user
    # csPtr points to the position in the stack corresponding to the current select concept
    conceptStack <- data.frame(nodeLabel=character(), sctid=character(), FSN=character())
    csPtr <- 0

    # pConcept contains participant and conept data from which a network os composed (node and edge data)
    pConcept <- data.frame()

    ##########################################################################################################
    # Function to update on-screen concept selection list
    ##########################################################################################################

    conceptSelUpdate <- function(sctid=NULL, FSN=NULL) {}

    ##########################################################################################################
    # Concept selection event (add selection to concept stack)
    ##########################################################################################################

    observeEvent(input$conceptSel,{

      #print(input$conceptSel)
      #print(conceptList[,"FSN"])

      # Identify position of selected concept in current concept list
      k <- which(conceptList[,"FSN"]==input$conceptSel)
      if(length(k)>0) {
        # Advance concept stack position and save selected concept
        csPtr <<- csPtr+1
        conceptStack[csPtr,] <<- conceptList[k,]
        rownames(conceptStack) <- NULL
        #print(conceptStack)
        output$currentRoot <- renderText(HTML(paste("<font color=blue>", conceptStack[csPtr,"FSN"], "</font>", sep="")))
        # Retrieve all nodes leading to the current root node by ISA relationships
        query <- paste(" match(x:ObjectConcept)-[:ISA]-(y:ObjectConcept)",
                       " where y.sctid='", conceptStack[csPtr,"sctid"], "' and x.active='1' and y.active='1'",
                       " return labels(x) as label, x.sctid as sctid, x.FSN as FSN",
                       " order by x.FSN", sep="")
        conceptList <<- cypher(graph, query)
        #print(conceptList)
        # Update selection list with current value of NA to avoid triggering an immediate update event
        updateSelectInput(session, "conceptSel", choices=conceptList[,"FSN"], selected=NA)
      }
    
    }, ignoreInit=T)

    ##########################################################################################################
    # Return to parent concept action (return to previous concept in stack)
    ##########################################################################################################

    observeEvent(input$retParentConcept,{

      if(csPtr>1) {
        csPtr <<- csPtr-1
        print(conceptStack[csPtr,])
        output$currentRoot <- renderText(HTML(paste("<font color=blue>", conceptStack[csPtr,"FSN"], "</font>", sep="")))
        # Retrieve all nodes leading to the current root node by ISA relationships
        query <- paste(" match(x:ObjectConcept)-[:ISA]-(y:ObjectConcept)",
                       " where y.sctid='", conceptStack[csPtr,"sctid"], "' and x.active='1' and y.active='1'",
                       " return labels(x) as label, x.sctid as sctid, x.FSN as FSN",
                       " order by x.FSN", sep="")
        conceptList <<- cypher(graph, query)
        #print(conceptList)
        # Update selection list with current value of NA to avoid triggering an immediate update event
        updateSelectInput(session, "conceptSel", choices=conceptList[,"FSN"], selected=NA)
      }

    })

    ##########################################################################################################
    # Render network action
    ##########################################################################################################

    observeEvent(input$renderGraph,{

      # Retrieve node and edge data for participants connected to the current selected concept
      # 
      query <- paste(" match(x:Participant)-[:P_SCT]-(y:ObjectConcept)-[:ISA]-(z:ObjectConcept)",
                     " where z.sctid='", conceptStack[csPtr,"sctid"], "' and y.active='1' and z.active='1'",
                     " return labels(x) as pLabel, x.ParticipantId as participantID, x.sex as sex,",
                     " x.UCDDx as UCDDx,",
                     " case when(toInteger(x.OnsetAgeDays)<11)then '0-11'",
                     "      when(toInteger(x.OnsetAgeDays)<101)then '11-100'",
                     "      when(toInteger(x.OnsetAgeDays)<1001)then '101-1000'",
                     "      when(toInteger(x.OnsetAgeDays)<10001)then '1001-10000'",
                     "      when(toInteger(x.OnsetAgeDays) is not null)then '>10000'",
                     "      else null",
                     " end as onsetAgeDays, labels(y) as conceptLabel,",
                     " y.sctid as conceptID, y.FSN as FSN", sep="")
      print(query)
      pConcept <<- cypher(graph, query)
      print(pConcept)

    })      

    ##########################################################################################################
    # Execution begins here
    ##########################################################################################################

    # Query SNOMED root node
    csPtr <- 1
    query <- "match(x) where x.FSN contains 'SNOMED CT Concept' and x.active='1' return labels(x) as label, x.sctid as sctid, x.FSN as FSN"
    conceptStack[csPtr,] <- cypher(graph, query)[1,]
    output$currentRoot <- renderText(HTML(paste("<font color=blue>", conceptStack[csPtr,"FSN"], "</font>", sep="")))

    # Retrieve all nodes leading to the current root node by ISA relationships
    query <- paste(" match(x:ObjectConcept)-[:ISA]-(y:ObjectConcept)",
                   " where y.sctid='", conceptStack[csPtr,"sctid"], "' and x.active='1' and y.active='1'",
                   " return labels(x) as label, x.sctid as sctid, x.FSN as FSN",
                   " order by x.FSN", sep="")
    conceptList <- cypher(graph, query)
    #print(conceptList)

    # Update selection list with current value of NA to avoid triggering an immediate update event
    updateSelectInput(session, "conceptSel", choices=conceptList[,"FSN"], selected=NA)

  }
)