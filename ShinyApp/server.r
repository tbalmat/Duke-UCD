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

##########################################################################################################
# Clear memory, in case execution was resumed (for instance, with the browser refresh operation)
# Note that objects in the global environment remain in the R while R is executing, even if a Shiny
# script is terminated
##########################################################################################################

#rm(.GlobalEnv, ls(.GlobalEnv))
#gc()
netData <- data.frame()

##########################################################################################################
# Set static filter and appearance parameters
#########################################################################################################

# Vertex colors (one each for concepts, participant features, prescriptions, and role groups)
vc1 <- "#FFEE66"
vc2 <- "#66AAFF"
vc3 <- "#88AAAA"
vc4 <- "#FF8080"
vc5 <- "#B284BE"
shiftClickColor <- "#FF00FF"

# Edge colors
ec1 <- "#080808"
ec2 <- "#C02020"

# Vertex and edge font sizes
vfsz <- 12
efsz <- 12

# Participant and interaction font colors
pfc <- "#505050"
ifc <- "#505050"

# Font stroke (outline) color
fsc1 <- "#909090"

# Configure vertex and edge properties for each selectable database variable
# Some of the following values are modified prior to use, depending on context
vcfg <- data.frame(
          # Variable ID - column names appearing in query results
          # Note that Rx is a placeholder for either rxName or rxSubsName and is modified as needed 
          "vID"=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "Rx", "findingSiteFSN"),
          # Database var - data frame column containing unique identifier for var level as reported by DB
          "dbID"=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptID", "Rx", "findingSiteID"),
          # Connection cfg IDs - used to positionally relate elements of the graphCfg connect vectors
          # to those selected (on-screen) by the user
          "cID"=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "Rx", "findingSiteFSN"),
          # Menu ID - for presentation
          "mID"=c("UCD proximal/distal", "Sex", "UCD Dx", "Onset age in days", "HA sympt last", "SNOMED concept", "Rx", "Finding site"),
          "vColor"=c(vc1, rgb(matrix(col2rgb(vc1)/255*0.85, nrow=1)), rgb(matrix(col2rgb(vc1)/255*0.7, nrow=1)),
                     rgb(matrix(col2rgb(vc1)/255*0.55, nrow=1)), vc5, vc2, vc3, vc4),
          "tSize"=c(1, 1, 1, 1, 1, 1, 1, 1),
          "tColor"=c(pfc, pfc, pfc, pfc, vc5, vc2, vc3, vc4),
          "elabVar"=c(NA, NA, NA, NA, NA, NA, NA, "fsRole"),
          "accMethod"=c("PID", "PID", "PID", "PID", "PID", "PID", "PID", "PID"))

# Reactive value list (used to copy values of reactive variables prior to being modified
reactVal <- list()

##########################################################################################################
# Load functions
##########################################################################################################

source("UCDFunctionsQuery.r", local=T, echo=F)
source("UCDFunctionsGraph.r", local=T, echo=F)

##########################################################################################################
# Create global variables
##########################################################################################################

# Connect to DB
db <- startGraph(c("http://localhost:7474/db/data/", "http://localhost:7479/db/data/")[1],
                 username="neo4j",
                 password=c("neo4j01", "Duke123!")[1])

# Create a graph configuration stack and a pointer to the current configuration
# The stack is descended into and ascended out of as graph nodes are selected and expanded
# Parameter values stored on the stack are used to requery the DB and construct a graph instance
graphCfg <- list()
gcPtr <- 0

# Interaction flag
interactionRendered <- F

##########################################################################################################
# Shiny server function
##########################################################################################################

shinyServer(
  function(input, output, session) {

    ##########################################################################################################
    # Configure objects that are shared within the Shiny environment 
    ##########################################################################################################

    # conceptStack contains the stack of concepts chosen by user
    # csPtr points to the position in the stack corresponding to the current select concept
    conceptStack <- data.frame(nodeLabel=character(), sctid=character(), FSN=character())
    csPtr <- 0

    # Create a data frame of selected node indicators (using shift-click)
    # These are used in sub-netting operations
    # Note that this must exist prior to using shift-click and alt-click (select and deselect)
    shiftClickNode <- data.frame("nodeID"=integer(), "color"=character())

    ##########################################################################################################
    # Concept selection event (add selection to concept stack)
    ##########################################################################################################

    observeEvent(input$conceptSel,{

      # Identify position of selected concept in current concept list
      k <- which(conceptList[,"FSN"]==input$conceptSel)
      if(length(k)>0) {
        # Advance concept stack position and save selected concept
        csPtr <<- csPtr+1
        conceptStack[csPtr,] <<- conceptList[k,]
        rownames(conceptStack) <- NULL
        output$currentRoot <- renderText(HTML(paste("<font color=blue>", conceptStack[csPtr,"FSN"], "</font>", sep="")))
        # Retrieve all nodes leading to the current node (in current stack pos) by ISA relationships
        # Filter list if within three levels of root SNOMED node
        conceptList <<- queryISAConcept(conceptStack[csPtr,"sctid"], filterOpt=ifelse(csPtr<4, "1", NA))
        # Update selection list with current value of NA to avoid triggering an immediate update event
        updateSelectInput(session, "conceptSel", choices=conceptList[,"FSN"], selected=NA)
      }
      updateActionButton(session, "queryConcept", HTML("query &nbsp;&nbsp; <font color='red'><i>NEEDS REFRESH!</i></font>"))

    }, ignoreInit=T)

    ##########################################################################################################
    # Return to parent concept action (return to previous concept in stack)
    ##########################################################################################################

    observeEvent(input$retParentConcept,{

      if(csPtr>1) {
        csPtr <<- csPtr-1
        output$currentRoot <- renderText(HTML(paste("<font color=blue>", conceptStack[csPtr,"FSN"], "</font>", sep="")))
        # Retrieve all nodes leading to the current node (in current stack pos) by ISA relationships
        # Filter list if within three levels of root SNOMED node
        conceptList <<- queryISAConcept(conceptStack[csPtr,"sctid"], filterOpt=ifelse(csPtr<4, "1", NA))
        # Update selection list with current value of NA to avoid triggering an immediate update event
        updateSelectInput(session, "conceptSel", choices=conceptList[,"FSN"], selected=NA)
      }
      updateActionButton(session, "queryConcept", HTML("query &nbsp;&nbsp; <font color='red'><i><B>NEEDS REFRESH!<B></i></font>"))

    }, ignoreInit=T)

    ##########################################################################################################
    # Configure graph configuration stack
    # Initialize, push/remove current configuration onto/from stack
    # Record currently selected concept IDs, participant var filter values, prescription filters, etc.
    # Note the global declaration to that the function accessible from outside the shiynyServer() env
    ##########################################################################################################

    graphCfgOp <<- function(op, query=NULL, filter=NULL, filterMode=NULL, vcfg=NULL) {

      # Parameters:
      # op ........... "init" ..... initialize graph configuration stack (one element with curr cfg)
      #                "add" ...... push the current on-screen configuration onto the graph configuration
      #                             stack and advance pointer
      #                "upd" ...... update configuration with current on-screen values
      #                "remove" ... remove the current configuration and retreat stack pointer
      #                "vcfg" ..... update the vcfg element of graphCfg
      # query ........ See notes in the queryNetworkData() function for an explanation
      # filter ....... See notes in the assembleNetwokComponents() function for an explanation
      # filterMode ... "filter", "expand", "nbhood1" as used in assembleNetworkComponents()
      # vcfg ......... Data frame of vertex configuration values, a modified version of the global
      #                vcfg data frame that reflects current data set and appearance configuration
      #                These values generally modify defaults and account for additional variables
      #                (such as for interaction) and are used to construct and render the current graph
      #                It is assumed that gcPtr > 0

      if(op %in% c("init", "add", "upd")) {

        # Use current values for elements common to init, add, upd operations
        gcfg <- list("query"=query,
                     "filter"=filter,
                     "filterMode"=filterMode,
                     "rxLeadCharFilter"=tolower(gsub(" ", "", input$rxLeadCharFilter)),
                     "connect"=list("UCDProxDist"=input$cnUCDProxDist,
                                    "Sex"=input$cnSex,
                                    "UCDDx"=input$cnUCDDx,
                                    "onsetAgeDays"=input$cnAge,
                                    "HASxLast"=input$cnHASxLast,
                                    "conceptFSN"=input$cnConceptFSN,
                                    "Rx"=input$cnRx,
                                    "rxSubsume"=input$rxSubsume,
                                    "findingSiteFSN"=input$cnFindingSite),
                     "interact"=list("set1"=input$interactSet1,
                                     "conn1"=input$interactConn1,
                                     "set2"=input$interactSet2,
                                     "conn2"=input$interactConn2),
                     "nedgemin"=input$nedgemin,
                     "eopacity"=input$eopacity,
                     "vmassf"=input$vMassFactor,
                     "vsizefactor"=input$vSizeFactor,
                     "vfontsz"=input$vFontSize,
                     "nCluster"=input$nCluster,
                     "nearestHighlightDeg"=input$nearestHighlightDeg,
                     "vcfg"=vcfg)

        # Assign elements specific to requested operation
        if("op"=="init" | gcPtr<1) {
          graphCfg <<- list()
          # Use current concept as query specification when unspecified
          if(is.null(query))
            gcfg[["query"]] <- list("conceptID"=conceptStack[csPtr,"sctid"])
          gcPtr <<- 1
        } else if(op %in% c("add", "upd") & gcPtr>0) {
          # Copy filters from current cfg if unspecified
          if(is.null(query))
            gcfg[["query"]] <- graphCfg[[gcPtr]][["query"]]
          if(is.null(filter))
            gcfg[["filter"]] <- graphCfg[[gcPtr]][["filter"]]
          if(is.null(filterMode))
            gcfg[["filterMode"]] <- graphCfg[[gcPtr]][["filterMode"]]
          if(op=="add")
            gcPtr <<- gcPtr+1
        }

        # Save new configuration
        if(gcPtr>0)
          graphCfg[[gcPtr]] <<- gcfg

      } else if(op=="remove" & gcPtr>1) {

        # Remove current cfg from stack (truncate stack)
        gcPtr <<- gcPtr-1
        graphCfg <<- graphCfg[1:gcPtr]
        # Restore screen values
        # Isolate to avoid triggering reactive events
        v <- data.frame("cfg"=c("UCDProxDist", "Sex", "UCDDx", "osetAgeDays", "HASxLast", "conceptFSN",
                                "Rx", "rxSubsume", "findingSiteFSN"),
                        "scr"=c("cnUCDProxDist", "cnSex", "cnUCDDx", "cnAge", "cnHASxLast", "cnConceptFSN",
                                "cnRx", "rxSubsume", "cnFindingSite"))
        for(i in 1:nrow(v))
          if(!is.null(graphCfg[[gcPtr]][["connect"]][[v[i,"cfg"]]])) {
            updateCheckboxGroupInput(session, v[i,"scr"], selected=graphCfg[[gcPtr]][["connect"]][[v[i,"cfg"]]])
          } else {
            updateCheckboxGroupInput(session, v[i,"scr"], selected=character())
          }
        v <- data.frame("cfg"=c("set1", "conn1", "set2", "conn2"),
                        "scr"=c("interactSet1", "interactConn1", "interactSet2", "interactConn2"))
        for(i in 1:nrow(v))
          if(!is.null(graphCfg[[gcPtr]][["interact"]][[v[i,"cfg"]]])) {
            updateCheckboxGroupInput(session, v[i,"scr"], selected=graphCfg[[gcPtr]][["interact"]][[v[i,"cfg"]]])
          } else {
            updateCheckboxGroupInput(session, v[i,"scr"], selected=character())
          }
        v <- data.frame("cfg"=c("nedgemin", "eopacity", "vmassf", "vsizefactor", "vfontsz", "nCluster", "nearestHighlightDeg"),
                        "scr"=c("nedgemin", "eopacity", "vMassFactor", "vSizeFactor", "vFontSize", "nCluster", "nearestHighlightDeg"))
        for(i in 1:nrow(v))
          if(!is.null(graphCfg[[gcPtr]][[v[i,"cfg"]]])) {
            updateCheckboxGroupInput(session, v[i,"scr"], selected=graphCfg[[gcPtr]][[v[i,"cfg"]]])
          } else {
            updateCheckboxGroupInput(session, v[i,"scr"], selected=character())
          }
        if(nchar(graphCfg[[gcPtr]][["rxLeadCharFilter"]])>0) {
          updateTextInput(session, "rxLeadCharFilter", value=graphCfg[[gcPtr]][["rxLeadCharFilter"]])
        } else {
          updateTextInput(session, "rxLeadCharFilter", value=NA)
        }

      } else if(op=="vcfg" & gcPtr>0) {

        # Save the supplied vcfg data frame in the current cfg
        graphCfg[[gcPtr]][["vcfg"]] <<- vcfg

      }

      #print("graphCfgOp.op")
      #print(op)
      #print("graphCfgOp.query")
      #print(graphCfg[[gcPtr]][["query"]])
      #print("graphCfgOp.filter")
      #print(graphCfg[[gcPtr]][["filter"]])
      #print("graphCfgOp.filterMode")
      #print(graphCfg[[gcPtr]][["filterMode"]])

    }

    ##########################################################################################################
    # Query concept action
    ##########################################################################################################

    observeEvent(input$queryConcept,{

      # Initialize graph configuration stack
      # Note that this event always establishes a new graph environment (imagine, otherwise, repeatedly
      # clicking render)
      graphCfgOp(op="init", query=list("conceptID"=conceptStack[csPtr,"sctid"]))

      # Query observations using the currenttly selected concept
      # Note the placement of results into a global data frame, since various user triggered actions
      # utilize previously queried data
      netData <<- queryNetworkData()

      if(nrow(netData)>0) {

        netComponents <<- assembleNetworkComponents()
        if(nrow(netComponents[["vertex"]])>0) {

          # Net regen is always done with physics enabled, but we want it to be disabled after regen
          # Direct disabling of physics (using visPhysics(enabled=F)) has no effect when called immediately after
          # renderVisNetwork(), but is effective when executed from within a shiny reactive function
          # So, although not ideal, force disable of physics by toggling the reaction control with physics par val
          updateRadioButtons(session, "physics", selected=T)
          output$g1 <- renderVisNetwork(composeNetwork(netComponents))
   
          # Compose and render centrality table
          #output$gTable <- DT::renderDataTable(composeGraphTable())
          #updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
          updateRadioButtons(session=session, inputId="physics", selected=F)

        } else {
          output$g1 <- NULL
          #output$gTable <- NULL
        }

      } else {

        output$g1 <- NULL
        #output$gTable <- NULL

      }

      updateActionButton(session, "queryConcept", label="query")

    }, ignoreInit=T)      

    ##########################################################################################################
    # Record reactive values prior to update
    # These are useful in evaluating current with prior values 
    ##########################################################################################################

    onFlush(session=session, once=FALSE,
      function() {
        isolate(reactVal[["interactSet1"]] <<- input$interactSet1)
        isolate(reactVal[["interactConn1"]] <<- input$interactConn1)
        isolate(reactVal[["interactSet2"]] <<- input$interactSet2)
        isolate(reactVal[["interactConn2"]] <<- input$interactConn2)
      }
    )  

    ##########################################################################################################
    # Action triggered by a change in screen controls that require graph regeneration
    ##########################################################################################################

    observeEvent(c(input$cnUCDProxDist, input$cnSex, input$cnUCDDx, input$cnAge,
                   input$cnHASxLast, input$cnConceptFSN, input$cnRx, input$rxSubsume,
                   input$cnFindingSite, input$nedgemin, input$eopacity, input$vMassFactor,
                   input$vSizeFactor, input$vFontSize, input$nCluster, input$nearestHighlightDeg), {

      if(exists("netData"))
        if(nrow(netData)>0) {
          # Modify current graph cfg
          graphCfgOp(op="upd")
          netComponents <<- assembleNetworkComponents()
          if(nrow(netComponents[["vertex"]])>0) {
            updateRadioButtons(session, "physics", selected=T)
            output$g1 <- renderVisNetwork(composeNetwork(netComponents))
            #output$gTable <- DT::renderDataTable(composeGraphTable())
            #updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
            updateRadioButtons(session=session, inputId="physics", selected=F)
          } else {
            output$g1 <- NULL
            #output$gTable <- NULL
          }
        } else {
          output$g1 <- NULL
          #output$gTable <- NULL
        }

    }, ignoreInit=T)

    #########################################################################################################
    # Interaction cfg trigger
    # These are evaluated independently from remaining controls because multiple items in the group must be
    # selected (variables in set and variables to be connected to)
    ##########################################################################################################

    observeEvent(c(input$interactSet1, input$interactConn1), {
      if(exists("netData")) {
        # Execute if sufficient number of variables specified either in current reactive values or prior to
        # a change in any of the interaction cfg values (so that, if interactions were included prior to change,
        # but are not to be included at present, then they will be removed and will not be included again until
        # a sufficient number of variables is spepcified)
        # Note that list elements are omitted when an attempt to assign NULL is made (prior value is NULL)
        # However, reference to nonexistent list elements (by name) return NULL and the length of a NULL vector is 0
        if(nrow(netData)>0 & (length(input$interactSet1)>1 & length(input$interactConn1)>0 |
                              length(input$interactSet2)>1 & length(input$interactConn2)>0 |
                              length(reactVal[["interactSet1"]])>1 & length(reactVal[["interactConn1"]])>0 |
                              length(reactVal[["interactSet2"]])>1 & length(reactVal[["interactConn2"]])>0)) {
          # Modify current graph cfg
          graphCfgOp(op="upd")
          netComponents <<- assembleNetworkComponents()
          if(nrow(netComponents[["vertex"]])>0) {
            updateRadioButtons(session, "physics", selected=T)
            output$g1 <- renderVisNetwork(composeNetwork(netComponents))
            #output$gTable <- DT::renderDataTable(composeGraphTable())
            updateRadioButtons(session=session, inputId="physics", selected=F)
          } else {
            output$g1 <- NULL
            #output$gTable <- NULL
          }
        }
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Render with fixed coordinates
    ##########################################################################################################

    observeEvent(c(input$renderFixed, input$renderFixedxScale, input$renderFixedyScale), {
      print("renderFixed")
      if(nrow(netComponents[["vertex"]])>0) {
        # Assign x-axis position by vertex variable class
        # Assign y-axis by alphabetic vertex label
        x <- as.integer(factor(netComponents[["vertex"]][,"varClass"]))
        xn <- aggregate(1:length(x), by=list(x), length)
        names(xn) <- c("x", "n")
        xy <- do.call(rbind, lapply(xn[,"x"],
                               function(ix) {
                                 k <- which(x==ix)
                                 yadj <- (max(xn[,"n"])-xn[ix,"n"])/2*input$renderFixedyScale*100
                                 data.frame("k"=k,
                                            "x"=ix*input$renderFixedxScale*100,
                                            "y"=order(netComponents[["vertex"]][k,"varClass"])*input$renderFixedyScale*100+yadj)
                               }))
        netComponents[["vertex"]][,c("x", "y")] <- xy[order(xy[,"k"]),c("x", "y")] 
        netComponents[["vertex"]][,"fixed"] <- T
        output$g1 <- renderVisNetwork(composeNetwork(netComponents))
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Render with free coordinates (disable fixed coordinates)
    ##########################################################################################################

    observeEvent(input$renderFree, {
      print("renderFree")
      if(nrow(netComponents[["vertex"]])>0) {
        netComponents[["vertex"]][,"fixed"] <- F
        updateRadioButtons(session, "physics", selected=T)
        output$g1 <- renderVisNetwork(composeNetwork(netComponents))
        updateRadioButtons(session, "physics", selected=F)
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Redraw edges after moving nodes edge event
    # Redraw by fixing vertex positions, stabilizing, then freeing vertex psitions
    ##########################################################################################################

    observeEvent(input$restoreEdgesPostMove, {
      print("restoreEdgesPostMove")
      if(nrow(netComponents[["vertex"]])>0) {
        # Fix positions
        visUpdateNodes(visNetworkProxy("g1"), data.frame("id"=netComponents[["vertex"]][,"id"], "fixed"=T))
        # Stabilize
        visStabilize(visNetworkProxy("g1"))
        # Free positions
        #updateTextInput(session=session, inputId="reactiveInst", value="vertexFixedOff")
        visUpdateNodes(visNetworkProxy("g1"), data.frame("id"=netComponents[["vertex"]][,"id"], "fixed"=F))
        output$g1 <- renderVisNetwork(composeNetwork(netComponents))
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Stabilized event
    # Disable physics after stabilization during initial network construction
    # This prevents dynamic repositioning of vertices as connected vertices are moved
    # Note that edges are not redrawn during dynamic movement, but are with the stabilize() function
    ##########################################################################################################

    observeEvent(input$stabilized, {
      print("stabilized")
      visPhysics(visNetworkProxy("g1"), enabled=F)
    })
    
    ##########################################################################################################
    # Physics event
    # Enable or disable physics operations (enabling causes repositioning of nodes, if not fixed, and edges)
    # Do not disable on first evaluation, during program initialization
    ##########################################################################################################

    observeEvent(input$physics, {
      print("physics")
      if(input$physics) {
        visPhysics(visNetworkProxy("g1"), enabled=T, timestep=0.25, minVelocity=10, maxVelocity=50,
                   solver=c("barnesHut", "repulsion")[1],
                   barnesHut=list("avoidOverlap"=0.5, "springLength"=100, "springConstant"=0.5, "damping"=0.5),
                   #repulsion=list("nodeDistance"=1000),
                   stabilization=list("enabled"=T, "iterations"=1000))
      } else {
        visPhysics(visNetworkProxy("g1"), enabled=F)
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Vertex shift-click event
    # Triggered by java script contained in the click element of the visEvents parameter of the graph
    # rendered by composeNetwork()
    # Verify that a vertex has been clicked (input$shiftClick[["nodes"]] length one or greater)
    # Include or remove node from current selection table
    ##########################################################################################################

    observeEvent(input$shiftClick, {
      print("shiftClick")
      # Identify selected vertex
      v <- input$shiftClick[["nodes"]]
      if(length(v)>0) {
        v0 <- v[[1]][1]
        print(v0)
        # Determine whether node is to be included or removed from selection table
        k <- which(shiftClickNode[,"nodeID"]==v0)
        if(length(k)>0) {
          # Node exists in table, so recolor then remove it from table
          visUpdateNodes(visNetworkProxy("g1"), nodes=data.frame("id"=netComponents[["vertex"]][v0,"id"], "color"=shiftClickNode[k,"color"]))
          shiftClickNode <<- shiftClickNode[-k,]
        } else {
          # Record node's row position and current color in vertex data frame
          shiftClickNode[nrow(shiftClickNode)+1,] <<- c(v0, netComponents[["vertex"]][v0,"color"])
          # Apply selection color to node
          visUpdateNodes(visNetworkProxy("g1"), nodes=data.frame("id"=netComponents[["vertex"]][v0,"id"], "color"=shiftClickColor))
        }
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Vertex alt-click event
    # Triggered by java script contained in the click element of the visEvents parameter of the graph
    # rendered by composeNetwork()
    ##########################################################################################################

    observeEvent(input$altClick, {
      print("altClick")
      # Identify selected vertex
      v <- input$altClick[["nodes"]]
      if(length(v)>0)
        print(v[[1]][1])
    }, ignoreInit=T)

    ##########################################################################################################
    # Return to previous graph
    # Decrement graph configuration stack pointer, truncate stack, and regenerate graph using cfg at new pointer
    ##########################################################################################################

    observeEvent(input$nodeRestorePrevious, {
      print("graphRestorePrevious")
      if(gcPtr>1) {
        # Retain query value for comparison to previous graph on stack value (difference warrants requery)
        q0 <- graphCfg[[gcPtr]][["query"]]
        graphCfgOp("remove")
        # Requery?  Subsetting may have omitted records for necessary concepts (or vars with children)
        netData <<- queryNetworkData()
        if(nrow(netData)>0) {
          netComponents <<- assembleNetworkComponents()
          if(nrow(netComponents[["vertex"]])>0) {
            updateRadioButtons(session, "physics", selected=T)
            output$g1 <- renderVisNetwork(composeNetwork(netComponents))
            #output$gTable <- DT::renderDataTable(composeGraphTable())
            updateRadioButtons(session=session, inputId="physics", selected=F)
          } else {
            output$g1 <- NULL
            #output$gTable <- NULL
          }
        } else {
          output$g1 <- NULL
          #output$gTable <- NULL
        }
      # Clear selected node indices
      shiftClickNode <<- shiftClickNode[F,]
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Create a vertex filter list for subsetting
    ##########################################################################################################

    subsetNodeFilterCompose <- function(nodeID) {

      # Parameters:
      # nodeID ........... Vector of node IDs (from netComponents[["vertex"]]) to be treated

      # Include nodes
      if(length(nodeID)>0) {
        # Generate vertex row indices for specified nodes
        k <- match(nodeID, netComponents[["vertex"]][,"id"])
        # Generate a list of row indices by database variable
        v <- split(k, netComponents[["vertex"]][k,"dbVar"])
        # Construct vectors of DB values to filter, by variable
        # Package as a graphCfg filter list
        filter <- lapply(1:length(v), function(i) unique(netComponents[["vertex"]][v[[i]],"dbValue"]))
        # Apply DB variable names
        names(filter) <- names(v)
        # Retain current filter specifications for variables not represented in selected nodes
        if(!is.null(graphCfg[[gcPtr]][["filter"]])) {
          # Index current filter elements not in selected node variables
          k <- which(!names(graphCfg[[gcPtr]][["filter"]]) %in% names(v))
          if(length(k)>0)
            filter <- c(filter, graphCfg[[gcPtr]][["filter"]][k])
        }
      } else {
        filter <- NULL
      }

      return(filter)

    }

    ##########################################################################################################
    # Node subnet event
    # Verify that vertices have been selected (shiftClickNode non-empty) or if Rx char filter has changed
    # Render new graph limited to selected nodes
    ##########################################################################################################

    observeEvent(input$nodeSubnet, {
      print("nodeSubnet")
      # shiftClickNode data frame contains IDs (row positions in vertex DF) of selected nodes
      if(nrow(shiftClickNode)>0 | input$rxLeadCharFilter!=graphCfg[[gcPtr]][["rxLeadCharFilter"]]) {
        # Push a new graph cfg with additional filter for selected nodes
        # Retain current query filter because it is not changing
        graphCfgOp(op="add",
                   query=graphCfg[[gcPtr]][["query"]],
                   # Include filter list
                   filter=subsetNodeFilterCompose(shiftClickNode[,"nodeID"]),
                   filterMode="filter")
        # Render graph
        netComponents <<- assembleNetworkComponents()
        if(nrow(netComponents[["vertex"]])>0) {
          updateRadioButtons(session, "physics", selected=T)
          output$g1 <- renderVisNetwork(composeNetwork(netComponents))
          #output$gTable <- DT::renderDataTable(composeGraphTable())
          #updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
          updateRadioButtons(session=session, inputId="physics", selected=F)
        } else {
          output$g1 <- NULL
          #output$gTable <- NULL
        }
        # Clear selected node indices
        shiftClickNode <<- shiftClickNode[F,]
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Node neighborhood subnet event
    # Verify that vertices have been selected (shiftClickNode non-empty)
    # Render new graph limited to selected nodes along with nodes with an edge to selected nodes
    ##########################################################################################################

    observeEvent(input$nodeNeighborhood1, {

      print("nodeNeighborhood1")
      # shiftClickNode data frame contains IDs (row positions in vertex DF) of selected nodes
      if(nrow(shiftClickNode)>0 | input$rxLeadCharFilter!=graphCfg[[gcPtr]][["rxLeadCharFilter"]]) {
        # Push a new graph cfg with additional filter for selected nodes
        # Retain current query filter because it is not changing
        graphCfgOp(op="add",
                   query=graphCfg[[gcPtr]][["query"]],
                   # Include filter list
                   filter=subsetNodeFilterCompose(shiftClickNode[,"nodeID"]),
                   filterMode="filter")
        # Render graph
        netComponents <<- assembleNetworkComponents()
        if(nrow(netComponents[["vertex"]])>0) {
          updateRadioButtons(session, "physics", selected=T)
          output$g1 <- renderVisNetwork(composeNetwork(netComponents))
          #output$gTable <- DT::renderDataTable(composeGraphTable())
          #updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
          updateRadioButtons(session=session, inputId="physics", selected=F)
        } else {
          output$g1 <- NULL
          #output$gTable <- NULL
        }
        # Clear selected node indices
        shiftClickNode <<- shiftClickNode[F,]
      }

    }, ignoreInit=T)

    ##########################################################################################################
    # Node expand event
    # Verify that vertices have been selected (shiftClickNode non-empty) or if Rx char filter has changed
    # Generate new graph using children of selected nodes, if any, otherwise the selected nodes
    # Advance graph configuration stack then assemble and compose graph using current cfg
    ##########################################################################################################

    observeEvent(input$nodeExpand, {

      print("nodeExpand")
      #print(shiftClickNode)

      # Construct vectors of DB values for querying (concepts) and filtering (concepts or other variables)
      # Package as graphCfg query and filter lists
      if(nrow(shiftClickNode)>0 | input$rxLeadCharFilter!=graphCfg[[gcPtr]][["rxLeadCharFilter"]]) {
        # Configure query and filter instructions
        # Generate vertex row indices for selected nodes
        k <- match(shiftClickNode[,"nodeID"], netComponents[["vertex"]][,"id"])
        # Generate a list of row indices by database variable
        v <- split(k, netComponents[["vertex"]][k,"dbVar"])
        # Identify selected concept nodes and place database IDs in query instruction
        k <- which(names(v)=="conceptID")
        if(length(k)>0) {
          # Concept selected - save it
          query <- list("conceptID"=netComponents[["vertex"]][v[[k]],"dbValue"])
          if(length(v)>1) {
            # Non-concepts also selected - compose filter form those variables, omitting concepts
            filter <- lapply(setdiff(1:length(v), k), function(i) unique(netComponents[["vertex"]][v[[i]],"dbValue"]))
            names(filter) <- names(v)[setdiff(1:length(v), k)]
          } else if(!is.null(graphCfg[[gcPtr]][["filter"]])) {
            # Only concept selected and current filter exists - retain filter, excluding concept, if present 
            k2 <- which(names(graphCfg[[gcPtr]][["filter"]])=="conceptID")
            if(length(k2)>0) {
              if(length(graphCfg[[gcPtr]][["filter"]])>1) {
                # Elements exist in addition to concept - keep only them
                filter <- graphCfg[[gcPtr]][["filter"]][setdiff(1:length(graphCfg[[gcPtr]][["filter"]]), k2)]
              } else {
                # Concept is the only element of current filter - omit it
                filter <- NULL
              }
            } else {
              # Concept not specified in current filter - retain entire filter (may be null)
              filter <- graphCfg[[gcPtr]][["filter"]]
            }
          } else {
            filter <- NULL
          }
        } else {
          # Concept node not selected - must have been non-concepts
          # NULL query instructs to use currently specified (on-screen) concept
          query <- NULL
          # Compose filter from selected nodes
          filter <- lapply(v, function(k) unique(netComponents[["vertex"]][k,"dbValue"]))
          names(filter) <- names(v)
          # Retain current concept filter, if specified
          if("conceptID" %in% names(graphCfg[[gcPtr]][["filter"]]))
            filter[["conceptID"]] <- graphCfg[[gcPtr]][["filter"]][["conceptID"]]
        }
        # Push a new graph cfg with query and filter instructions
        # Retain current value, since no concept nodes were selected
        if(is.null(query))
          query <- list("conceptID"=conceptStack[csPtr,"sctid"]) 
        graphCfgOp(op="add", query=query, filter=filter, filterMode="expand")
        # Requery observations
        netData <<- queryNetworkData()
        if(nrow(netData)>0) {
          # Render graph
          netComponents <<- assembleNetworkComponents()
          if(nrow(netComponents[["vertex"]])>0) {
            updateRadioButtons(session, "physics", selected=T)
            output$g1 <- renderVisNetwork(composeNetwork(netComponents))
            #output$gTable <- DT::renderDataTable(composeGraphTable())
            #updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
            updateRadioButtons(session=session, inputId="physics", selected=F)
          } else {
            output$g1 <- NULL
            #output$gTable <- NULL
          }
        } else {
          output$g1 <- NULL
          #output$gTable <- NULL
        }
        # Clear selected node indices
        shiftClickNode <<- shiftClickNode[F,]
      }

    }, ignoreInit=T)

    ##########################################################################################################
    # Execution begins here
    ##########################################################################################################

    # Query SNOMED root node
    # Initialize pointer to current concept in stack
    csPtr <- 1
    conceptStack[csPtr,] <- queryISAConcept(0)[1,]
    output$currentRoot <- renderText(HTML(paste("<font color=blue>", conceptStack[csPtr,"FSN"], "</font>", sep="")))

    # Retrieve all nodes leading to the root concept node by ISA relationships
    # Filter list to nodes of interest
    conceptList <- queryISAConcept(conceptStack[csPtr,"sctid"], filterOpt="1")

    # Update concept selection list with current value of NA to avoid triggering an immediate update event
    updateSelectInput(session, "conceptSel", choices=conceptList[,"FSN"], selected=NA)

  }
)