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
# Set static filter and appearance parameters
##########################################################################################################

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

# Font stroke (outline) color
fsc1 <- "#909090"

##########################################################################################################
# Load functions
##########################################################################################################

source("UCDFunctionsQuery.r", local=T, echo=F)
source("UCDFunctionsGraph.r", local=T, echo=F)

##########################################################################################################
# Connect to DB
##########################################################################################################

dbcon <- startGraph(c("http://localhost:7474/db/data/", "http://localhost:7479/db/data/")[1],
                    username="neo4j",
                    password=c("neo4j01", "Duke123!")[1])

##########################################################################################################
# Shiny server function
##########################################################################################################

shinyServer(
  function(input, output, session) {

    ##########################################################################################################
    # Configure global objects
    ##########################################################################################################

    # conceptStack contains the stack of concepts chosen by user
    # csPtr points to the position in the stack corresponding to the current select concept
    conceptStack <- data.frame(nodeLabel=character(), sctid=character(), FSN=character())
    csPtr <- 0

    # pConcept contains participant and conept data from which a network os composed (node and edge data)
    pConcept <- data.frame()

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

    }, ignoreInit=T)

    ##########################################################################################################
    # Render network action
    ##########################################################################################################

    observeEvent(input$renderGraph,{

      # Capture on-screen parameter values to be relayed to graph query and construction functions
      nodeVar <- input$nodeVar
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

      # Construct graph
      netComponents <<- queryAndConstructGraph(nodeVar=nodeVar, conceptID=conceptStack[csPtr,"sctid"],
                                               nodeVarFilter=NULL, prescripFilter=NULL, roleGroupFilter=NULL,
                                               haFilter=NULL,
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
        output$g1 <- renderVisNetwork(composeNetwork(vertex=netComponents[["vertex"]], edge=netComponents[["edge"]],
                                                     nodeVar1=nodeVar, nodeVar2="FSN",
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
    # Redraw edge event
    # Redraw by fixing vertex positions, stabilizing, then freeing vertex psitions
    ##########################################################################################################

    #observeEvent(input$redrawEdge, {
    #  print("redrawEdge")
    #  # Fix positions
    #  visUpdateNodes(visNetworkProxy("g1"), data.frame("id"=vertex[,"id"], "fixed"=T))
    #  # Stabilize
    #  visStabilize(visNetworkProxy("g1"))
    #  # Free positions
    #  updateTextInput(session=session, inputId="reactiveInst", value="vertexFixedOff")
    #}, ignoreInit=T)

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
                                                     nodeVar1=input$nodeVar, nodeVar2="FSN",
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
                                                     nodeVar1=input$nodeVar, nodeVar2="FSN",
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
        nodeVar <- input$nodeVar
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

        # Retrieve participant node var levels to filter - if no participnat nodes selected then reuse
        # those appearing in current graph configuration (on assumption that that, possibly filtered
        # group is to be further studied) 
        nodeVarFilter <- vertex[which(vertex[,"varClass"]==nodeVar & vertex[,"id"] %in% altClickNode),"dbID"]
        if(length(nodeVarFilter)==0)
          nodeVarFilter <- vertex[which(vertex[,"varClass"]==nodeVar), "dbID"]
        #print(nodeVarFilter)

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
                            nodeVar=nodeVar, conceptID=conceptID,
                            nodeVarFilter=nodeVarFilter, prescripFilter=prescripFilter,
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
                           nodeVar1=nodeVar,
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
    # Toggle prescriptions event
    ##########################################################################################################

    observeEvent(input$togglePrescription, {

      print("togglePrescription")

      # Capture on-screen parameter values to be relayed to graph query and construction functions
      nodeVar <- input$nodeVar
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

      # Test for prescription node existence
      # If nodes exist then trim them, otherwise append new ones
      vertex <- netComponents[["vertex"]]
      edge <- netComponents[["edge"]]
      kv <- which(vertex[,"varClass"]=="Prescription")

      if(length(kv)>0) {

        # Identify all edges that connect prescription vertices
        # From and to indicate node IDs
        ke <- which(edge[,"from"] %in% vertex[kv,"id"] | edge[,"to"] %in% vertex[kv,"id"])
        # Omit vertices and edges
        netComponents[["vertex"]] <<- vertex[-kv,]
        netComponents[["edge"]] <<- edge[-kv,]
        print(netComponents[[1]])
        # Render graph
        output$g1 <- renderVisNetwork(
                       composeNetwork(
                         vertex=netComponents[["vertex"]], edge=netComponents[["edge"]],
                           nodeVar1=input$nodeVar, nodeVar2="FSN",
                           nodeVar3=ifelse(!is.null(prescripConnector), "Prescription", NA),
                           nodeVar4=ifelse(!is.null(roleGroupConnector), "roleGroupFSN", NA),
                           nodeVar5=ifelse(!is.null(haConnector), "HASxLast", NA),
                           vColor1=vc1, vColor2=vc2, vColor3=vc3, vColor4=vc4,
                           nCluster=nCluster, nearestHighlightDeg=nearestHighlightDeg))
        # Compose and render centrality table
        #output$gTable <- DT::renderDataTable(composeGraphTable())
        #updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
        updateRadioButtons(session=session, inputId="physics", selected=F)

      } else if(!is.null(prescripConnector)) {

        # Append prescription nodes

        vertex <- netComponents[["vertex"]]

        # Retrieve concept IDs from selected nodes - use current on screen concept if none selected
        conceptID <- vertex[which(vertex[,"varClass"]=="FSN" & vertex[,"id"] %in% altClickNode),"dbID"]
        if(length(conceptID)==0)
          conceptID <- conceptStack[csPtr,"sctid"]

        # Retrieve participant node var levels to filter - if no participnat nodes selected then reuse
        # those appearing in current graph configuration (on assumption that that, possibly filtered
        # group is to be further studied) 
        nodeVarFilter <- vertex[which(vertex[,"varClass"]==nodeVar & vertex[,"id"] %in% altClickNode),"dbID"]
        if(length(nodeVarFilter)==0)
          nodeVarFilter <- vertex[which(vertex[,"varClass"]==nodeVar), "dbID"]

        # Append prescription nodes and edges
        netComponents <<- appendNetwork(
                            netComponents,
                            queryAndConstructGraphPrescrip(
                              nodeVar=ifelse(prescripConnector=="nodeVar", nodeVar, "FSN"),
                              conceptID=conceptID,
                              nodeVarFilter=nodeVarFilter,
                              prescripFilter=NULL,
                              subsume=prescripSubsume,
                              vColor1=ifelse(prescripConnector=="nodeVar", vc1, vc2),
                              vColor2=vc3,
                              nedgemin=nedgemin, eopacity=eopacity, vMassFactor=vmassf,
                              vSizeFactor=vsizefactor, vFontSize=vfontsz, elabVar=NA,
                              accMethod="PID"))

        # Render graph
        if(nrow(netComponents[["vertex"]])>0) {
          # Net regen is always done with physics enabled, but we want it to be disablead after regen
          # Direct disabling of physics (using visPhysics(enabled=F)) has no effect when called immediately after
          # renderVisNetwork(), but is effective when executed frimm within a shiny reactive function
          # So, although not ideal, force disable of physics by toggling the reaction control with physics par val
          updateRadioButtons(session, "physics", selected=T)
          output$g1 <- renderVisNetwork(
                         composeNetwork(
                           vertex=netComponents[["vertex"]], edge=netComponents[["edge"]],
                             nodeVar1=nodeVar,
                             nodeVar2="FSN",
                             nodeVar3=ifelse(!is.null(prescripConnector), "Prescription", NA),
                             nodeVar4=ifelse(!is.null(roleGroupConnector), "roleGroupFSN", NA),
                             nodeVar5=ifelse(!is.null(haConnector), "HASxLast", NA),
                             vColor1=vc1, vColor2=vc2, vColor3=vc3, vColor4=vc4,
                             nCluster=nCluster, nearestHighlightDeg=nearestHighlightDeg))
          # Compose and render centrality table
          #output$gTable <- DT::renderDataTable(composeGraphTable())
          #updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
          updateRadioButtons(session=session, inputId="physics", selected=F)
        }

      }

    })

    ##########################################################################################################
    # Restore graph after sub-netting
    ##########################################################################################################

    observeEvent(input$restoreVertex, {
      print("restoreVertex")
      output$g1 <- renderVisNetwork(
                     composeNetwork(
                       vertex=netComponents[["vertex"]], edge=netComponents[["edge"]],
                         nodeVar1=input$nodeVar,
                         nodeVar2="FSN",
                         nodeVar3=ifelse(!is.null(input$prescripConnector), "Prescription", NA),
                         nodeVar4=ifelse(!is.null(input$roleGroupConnector), "roleGroupFSN", NA),
                         nodeVar5=ifelse(!is.null(input$haConnector), "HASxLast", NA),
                         vColor1=vc1, vColor2=vc2, vColor3=vc3, vColor4=vc4,
                         nCluster=input$nCluster, nearestHighlightDeg=input$nearestHighlightDeg))
      #updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
      #updateRadioButtons(session=session, inputId="physics", selected=F)
      altClickNode <<- vector("integer")
    }, ignoreInit=T)

    ##########################################################################################################
    # Execution begins here
    ##########################################################################################################

    # Query SNOMED root node
    csPtr <- 1
    query <- "match(x) where x.FSN contains 'SNOMED CT Concept' and x.active='1'
              return   labels(x) as label, x.sctid as sctid, x.FSN as FSN limit 1"
    conceptStack[csPtr,] <- cypher(dbcon, query)[1,]
    output$currentRoot <- renderText(HTML(paste("<font color=blue>", conceptStack[csPtr,"FSN"], "</font>", sep="")))

    # Retrieve all nodes leading to the root node by ISA relationships
    # Filter list to nodes of interest
    conceptList <- queryISAConcept(conceptStack[csPtr,"sctid"], filterOpt="1")

    # Create global vector of nodes selected while holding the Alt key
    # These are used in sub-netting operations
    # Note that this must exist prior to clicking the Alt-clk buttons
    altClickNode <- vector("integer")

    # Update selection list with current value of NA to avoid triggering an immediate update event
    updateSelectInput(session, "conceptSel", choices=conceptList[,"FSN"], selected=NA)

  }
)