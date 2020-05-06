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
altClickColor <- "#FF00FF"

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
          # Database ID - data frame column containing unique identifier for var level as reported by DB
          "dbID"=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptID", "Rx", "findingSiteID"),
          # Connection cfg IDs - used to positionally relate elements of the graphCfg connect vectors
          # to those selected (on-screen) by the user
          "cID"=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "Rx", "findingSiteFSN"),
          # Menu ID - for presentation
          "mID"=c("UCD proximal/distal", "Sex", "UCD Dx", "Onset age in days", "HA sympt last", "SNOMED concept", "Rx", "Finding site"),
          "vColor"=c(vc1, rgb(matrix(col2rgb(vc1)/255*0.85, nrow=1)), rgb(matrix(col2rgb(vc1)/255*0.7, nrow=1)),
                     rgb(matrix(col2rgb(vc1)/255*0.55, nrow=1)), vc5, vc2, vc3, vc4),
          "tSize"=c(1.2, 1.2, 1.2, 1.2, 1.2, 1, 1, 1),
          "tColor"=c(pfc, pfc, pfc, pfc, vc5, vc2, vc3, vc4),
          "elabVar"=c(NA, NA, NA, NA, NA, NA, NA, "fsRole"),
          "accMethod"=c("PID", "PID", "PID", "PID", "PID", "PID", "PID", "PID"))

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

    # Create a vector of selected node indicators while holding the Alt key
    # These are used in sub-netting operations
    # Note that this must exist prior to clicking the Alt-clk buttons
    altClickNode <- vector("integer")

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
    # Initialize graph configuration stack, push/remove current configuration onto/from stack
    # Record currently selected concept IDs, participant var filter values, prescription filters, etc.
    # Note the global declaration to that the function accessible from outside the shiynyServer() env
    ##########################################################################################################

    graphCfgOp <<- function(op, filter=NA, vcfg=NA) {

      # Parameters:
      # op ....... "init" ..... initialize graph configuration stack (one element with curr cfg)
      #            "add" ...... push the current on-screen configuration onto the graph configuration stack and advance pointer
      #            "upd" ...... update configuration with current on-screen values
      #            "remove" ... remove the current configuration and retreat stack pointer
      #            "vcfg" ..... update the vcfg element of graphCfg
      # filter ... List of current query filter parameter values, one element per graph node variable, with name
      #            equal to that of the variable and containing a vector of variable levels (values) to be included
      #            in query results
      #            This list must contain a "conceptID" parameter and associate vector of IDs (may be of length one)
      # vcfg ..... Data frame of vertex configuration values, a modified version of the global vcfg data frame
      #            that reflects current data set and appearance configuration
      #            These values generally modify defaults and account for additional variables (such as for 
      #            interaction) and are used to construct and render the current graph
      #            It is assumed that gcPtr > 0

      if(op %in% c("init", "add", "upd")) {
        # Initialize, if requested
        # Advance pointer if adding an entry
        if(op=="init" | gcPtr<1) {
          graphCfg <<- list()
          gcPtr <<- 1
        } else if(op=="add") {
          gcPtr <<- gcPtr+1
        }
        # Assign filter if not specified (retain current when cfg exists, use concept if initializing)
        if(is.na(filter))
          if(gcPtr>0) {
            filter <- graphCfg[["filter"]]
          } else {
            filter <- list("conceptID"=conceptStack[csPtr,"sctid"])
          }
        # Save configuration
        if(op %in% c("init", "add", "upd")) {
          # Capture on-screen parameter values to be relayed to graph query and construction functions
          graphCfg[[gcPtr]] <<- list("filter"=filter,
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
                                     "nearestHighlightDeg"=input$nearestHighlightDeg)
      } else if(op=="remove") {
        if(gcPtr>1) {
          # Remove current cfg from stack (truncate stack)
          gcPtr <<- gcPtr-1
          graphCfg <<- graphCfg[1:gcPtr]
        } else {
          # Clear all configurations
          gcPtr <<- 0
          graphCfg <<- list()
        }
      }
      } else if(op=="vcfg" & gcPtr>0) {
        # Save the supplied vcfg data frame
        graphCfg[[gcPtr]][["vcfg"]] <<- vcfg
      }
    }

    ##########################################################################################################
    # Query observations for selected concept
    ##########################################################################################################

    observeEvent(input$queryConcept,{

      # Initialize graph configuration stack
      # Note that this event always establishes a new graph environment (imagine, otherwise, repeatedly
      # clicking render)
      graphCfgOp(op="init", filter=list("conceptID"=conceptStack[csPtr,"sctid"]))

      # Query observations using the currenttly selected concept
      # Note the placement of results into a global data frame, since various user triggered actions
      # utilize previously queried data
      netData <<- queryNetworkData(filter=graphCfg[[gcPtr]][["filter"]])

      if(nrow(netData)>0) {

        netComponents <- assembleNetworkComponents()
        if(nrow(netComponents[["vertex"]])>0) {
print(graphCfg[[gcPtr]][["vcfg"]])
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
    # Action triggered by a change in screen controls that require graph regeneration
    ##########################################################################################################

    observeEvent(c(input$cnUCDProxDist, input$cnSex, input$cnUCDDx, input$cnAge,
                   input$cnHASxLast, input$cnConceptFSN, input$cnRx, input$rxSubsume,
                   input$cnFindingSite, input$nedgemin, input$eopacity, input$vMassFactor,
                   input$vSizeFactor, input$vFontSize, input$nCluster, input$nearestHighlightDeg), {
print("DDDDDDDDDDDDDDDDDDDDDDDDDDDDD")
      if(exists("netData"))
        if(nrow(netData)>0) {

          # Modify current graph cfg
          graphCfgOp(op="upd")

          netComponents <- assembleNetworkComponents()

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
    # Interaction trigger
    # These are evaluated independently from remaining controls because multiple items in the group must be
    # selected (variables in set and variables to be connected to)
    ##########################################################################################################

    observeEvent(c(input$interactSet1, input$interactConn1), {

      if(exists("netData"))
        if(nrow(netData)>0 & (length(input$interactSet1)>1 & length(input$interactConn1)>0 |
                              length(input$interactSet2)>1 & length(input$interactConn2)>0)) {
          # Modify current graph cfg
          graphCfgOp(op="upd")
          netComponents <- assembleNetworkComponents()
print("ZXCZXCZCZXCZCZXCZX")
print(netComponents[["vertex"]])
print(netComponents[["edge"]])
          if(nrow(netComponents[["vertex"]])>0) {
            updateRadioButtons(session, "physics", selected=T)
#x <- composeNetwork(netComponents)
#print(x)

            output$g1 <- renderVisNetwork(composeNetwork(netComponents))
            #output$gTable <- DT::renderDataTable(composeGraphTable())
            updateRadioButtons(session=session, inputId="physics", selected=F)
          } else {
            output$g1 <- NULL
            #output$gTable <- NULL
          }
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
        visUpdateNodes(visNetworkProxy("g1"), data.frame("id"=vertex[,"id"], "fixed"=F))
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
    # Hide all vertices not connected to selected vertex and all edges attached to hidden vertices
    ##########################################################################################################

    observeEvent(input$shiftClick, {
      print("shiftClick")
      # Identify selected vertex
      v <- input$shiftClick[["nodes"]]
      if(length(v)>0) {
        v0 <- v[[1]][1]
        print(v0)
        vertex <- netComponents[["vertex"]]
        edge <- netComponents[["edge"]]
        # Identify all edges connected to selected vertex
        # From and to indicate node IDs
        # It is assumed that the ID of the selected node is returned in input$shiftClick[["nodes"]] (above), since
        # that is assumed to be the only data available to the UI once a graph has been rendered
        # Note that IDs were saved as numeric (in assembleNetComponents())
        ke <- which(edge[,"from"]==v0 | edge[,"to"]==v0)
        # Identify all vertices connected to selected vertex
        kv <- which(vertex[,"id"] %in% unlist(edge[ke,c("from", "to")]))
        # Hide vertices that are not connected to selected vertex
        vertex[,"hidden"] <- T
        vertex[kv,"hidden"] <- F
        vertex[,"physics"] <- F
        vertex[kv,"physics"] <- T
        output$g1 <- renderVisNetwork(composeNetwork(vertex=vertex, edge=edge[ke,],
                                                     nodeVar1=input$prtVar, nodeVar2="FSN",
                                                     nodeVar3=ifelse(!is.null(input$prescripConnector), "Prescription", NA),
                                                     nodeVar4=ifelse(!is.null(input$roleGroupConnector), "roleGroupFSN", NA),
                                                     nodeVar5=ifelse(!is.null(input$haConnector), "HASxLast", NA),
                                                     vColor1=vc1, vColor2=vc2, vColor3=vc3, vColor4=vc4, vColor5=vd5,
                                                     nCluster=input$nCluster, nearestHighlightDeg=input$nearestHighlightDeg))
        #updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
        #updateRadioButtons(session=session, inputId="physics", selected=F)
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Vertex alt-click event
    # Triggered by java script contained in the click element of the visEvents parameter of the graph
    # rendered by composeNetwork()
    # Verify that a vertex has been clicked (input$alt[["nodes"]] length one or greater)
    # Render new graph using selected node and any with a relationship
    ##########################################################################################################

    observeEvent(input$altClick, {
      print("altClick")
      # Identify selected vertex
      v <- input$altClick[["nodes"]]
      if(length(v)>0) {
        v0 <- v[[1]][1]
        # Record node's row position in vertex data frame
        altClickNode <<- c(altClickNode, v0)
        # Color selected vertex
        visUpdateNodes(visNetworkProxy("g1"), nodes=data.frame(id=netComponents[["vertex"]][v0,"id"], color=altClickColor))
        print(v0)
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Alt-click subnet event
    # Verify that vertices have been alt-clicked (altClickNode non-empty)
    # Render new graph using selected nodes
    ##########################################################################################################

    observeEvent(input$altClickSubnet, {
      print("altClickSubnet")
      # altClickNode vector contains IDs of nodes selected while holding the Alt key
      if(length(altClickNode)>0) {
        print(altClickNode)
        vertex <- netComponents[["vertex"]]
        edge <- netComponents[["edge"]]
        #print(vertex[altClickNode,])
        # Identify all edges that connect selected vertices
        # From and to indicate node IDs
        # It is assumed that the IDs selected nodes are saved in altClickNode
        # IDs were saved as numeric (in assembleNetComponents())
        # Note that an or in the following which() would select all nodes connected to any selected node
        ke <- which(edge[,"from"] %in% altClickNode & edge[,"to"] %in% altClickNode)
        #print(ke)
        # Identify all selected
        kv <- altClickNode #which(vertex[,"id"] %in% unlist(edge[ke,c("from", "to")]))
        # Hide vertices that are not connected to selected vertices
        vertex[,"hidden"] <- T
        vertex[kv,"hidden"] <- F
        vertex[,"physics"] <- F
        vertex[kv,"physics"] <- T
        output$g1 <- renderVisNetwork(composeNetwork(vertex=vertex, edge=edge[ke,],
                                                     nodeVar1=input$prtVar, nodeVar2="FSN",
                                                     nodeVar3=ifelse(!is.null(input$prescripConnector), "Prescription", NA),
                                                     nodeVar4=ifelse(!is.null(input$roleGroupConnector), "roleGroupFSN", NA),
                                                     nodeVar5=ifelse(!is.null(input$haConnector), "HASxLast", NA),
                                                     vColor1=vc1, vColor2=vc2, vColor3=vc3, vColor4=vc4, vColor=vc5,
                                                     nCluster=input$nCluster, nearestHighlightDeg=input$nearestHighlightDeg))
        #updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
        #updateRadioButtons(session=session, inputId="physics", selected=F)
        altClickNode <<- vector("integer")
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Alt-click descend event
    # Verify that vertices have been alt-clicked (altClickNode non-empty)
    # Generate new graph using children of selected participant and concept nodes
    # Append prescription and role group nodes using their current configuration parameter values
    ##########################################################################################################

    observeEvent(input$altClickDescend, {

      print("altClickDescend")

      if(length(altClickNode)>0) {

        # Capture on-screen parameter values to be relayed to graph query and construction functions
        prtVar <- input$prtVar
        nedgemin <- input$nedgemin
        eopacity <- input$eopacity
        vmassf <- input$vMassFactor
        vsizefactor <- input$vSizeFactor
        vfontsz <- c(input$vFontSize, 0.8*input$vFontSize)
        nCluster <- input$nCluster
        nearestHighlightDeg <- input$nearestHighlightDeg
        prescripConnector <- input$prescripConnector
        prescripSubsume <- input$prescripSubsume
        roleGroupConnector <- input$roleGroupConnector
        haConnector <- input$haConnector

        # altClickNode vector contains IDs of nodes selected while holding the Alt key
        print(altClickNode)
        vertex <- netComponents[["vertex"]]
        edge <- netComponents[["edge"]]
        print(vertex[altClickNode,])

        # Retrieve concept IDs from selected nodes - use current on screen concept if none selected
        conceptID <- vertex[which(vertex[,"varClass"]=="FSN" & vertex[,"id"] %in% altClickNode),"dbID"]
        if(length(conceptID)==0)
          conceptID <- conceptStack[csPtr,"sctid"]
        #print(conceptID)

        # Retrieve participant node var levels to filter - if no participant nodes selected then reuse
        # those appearing in current graph configuration (on assumption that that, possibly filtered
        # group is to be further studied) 
        prtVarFilter <- vertex[which(vertex[,"varClass"]==prtVar & vertex[,"id"] %in% altClickNode),"dbID"]
        if(length(prtVarFilter)==0)
          prtVarFilter <- vertex[which(vertex[,"varClass"]==prtVar), "dbID"]
        #print(prtVarFilter)

        # Retrieve selected prescriptions to filter
        prescripFilter <- vertex[which(vertex[,"varClass"]=="Prescription" & vertex[,"id"] %in% altClickNode),"dbID"]
        if(length(prescripFilter)==0)
          prescripFilter <- vertex[which(vertex[,"varClass"]=="Prescription"), "dbID"]

        # Retrieve selected roles to filter
        roleGroupFilter <- vertex[which(vertex[,"varClass"]=="roleGroupFSN" & vertex[,"id"] %in% altClickNode),"dbID"]
        if(length(roleGroupFilter)==0)
          roleGroupFilter <- vertex[which(vertex[,"varClass"]=="roleGroup"), "dbID"]

        # Retrieve selected hyperammonemia variables to filter
        haFilter <- vertex[which(vertex[,"varClass"]=="HASxLast" & vertex[,"id"] %in% altClickNode),"dbID"]
        if(length(haFilter)==0)
          haFilter <- vertex[which(vertex[,"varClass"]=="HASxLast"), "dbID"]
        #print(haFilter)

        prescripFilter <- NULL
        roleGroupFilter <- NULL
        haFilter <- NULL

        # Construct graph
        netComponents <<- queryAndConstructGraph(
                            nodeVar=prtVar, conceptID=conceptID,
                            prtVarFilter=prtVarFilter, prescripFilter=prescripFilter,
                            roleGroupFilter=roleGroupFilter, haFilter=haFilter,
                            prescripConnector=prescripConnector, prescripSubsume=prescripSubsume,
                            roleGroupConnector=roleGroupConnector, haConnector=haConnector,
                            vColor1=vc1, vColor2=vc2, vColor3=vc3, vColor4=vc4, vColor5=vc5,
                            nedgemin=nedgemin, eopacity=eopacity,
                            vMassFactor=vmassf, vSizeFactor=vsizefactor, vFontSize=vfontsz)

        # Render graph
        if(nrow(netComponents[["vertex"]])>0) {
          # Net regen is always done with physics enabled, but we want it to be disablead after regen
          # Direct disabling of physics (using visPhysics(enabled=F)) has no effect when called immediately after
          # renderVisNetwork(), but is effective when executed frimm within a shiny reactive function
          # So, although not ideal, force disable of physics by toggling the reaction control with physics par val
          updateRadioButtons(session, "physics", selected=T)
          output$g1 <- renderVisNetwork(
                         composeNetwork(
                           vertex=netComponents[["vertex"]],
                           edge=netComponents[["edge"]],
                           nodeVar1=prtVar,
                           nodeVar2="FSN",
                           nodeVar3=ifelse(!is.null(prescripConnector), "Prescription", NA),
                           nodeVar4=ifelse(!is.null(roleGroupConnector), "roleGroupFSN", NA),
                           nodeVar5=ifelse(!is.null(haConnector), "HASxLast", NA),
                           vColor1=vc1, vColor2=vc2, vColor3=vc3, vColor4=vc4, vColor5=vc5,
                           nCluster=nCluster, nearestHighlightDeg=nearestHighlightDeg))
          # Compose and render centrality table
          #output$gTable <- DT::renderDataTable(composeGraphTable())
          #updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
          updateRadioButtons(session=session, inputId="physics", selected=F)
        } else {
          output$g1 <- NULL
          #output$gTable <- NULL
        }

        altClickNode <<- vector("integer")

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