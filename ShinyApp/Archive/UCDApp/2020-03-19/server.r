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

# Vertex colors (one for concepts, one for groups, one for prescriptions)
vc1 <- "#66AAFF"
vc2 <- "#FFEE66"
vc3 <- "#88AAAA"

# Edge colors
ec1 <- "#080808"
ec2 <- "#C02020"

# Vertex and edge font sizes
vfsz <- 12
efsz <- 12

# Font stroke (outline) color
fsc1 <- "#909090"

# Vertex rendering scale factor
vsizefactor <- 1

##########################################################################################################
# Connect to DB
##########################################################################################################

dbcon <- startGraph("http://localhost:7474/db/data/", username="neo4j", password=c("Duke123!", "neo4j01")[2])

##########################################################################################################
# Function to assemble graph components (vertices and edges)
##########################################################################################################

assembleNetComponents <- function(pConcept, nodeVar1, nodeVar2, vColor1, vColor2, nedgemin, eopacity, vmassf, vfontsz) {

  #print(nodeVar1)
  #print(nodeVar2)
  #print(colnames(pConcept))

  # Compose graph data set, with participant groups and concepts as vertices and relationship of participant property and concept as edges

  if(nrow(pConcept)>0) {
    # Tabulate edges by node vars 1 and 2
    edat <- aggregate(1:nrow(pConcept), by=list(pConcept[,nodeVar1], pConcept[,nodeVar2]), length)
    colnames(edat) <- c("v1", "v2", "n")
    # edat[,"lab"] <- 
    #print(edat)
    # Tabulate vertices
    # Vertex set 1 (v1) for nodeVar1, set 2 (v2) for nodeVar2
    v1 <- aggregate(1:nrow(pConcept), by=list(pConcept[,nodeVar1]), length)
    colnames(v1) <- c("lab", "n")
    v2 <- aggregate(1:nrow(pConcept), by=list(pConcept[,nodeVar2]), length)
    colnames(v2) <- c("lab", "n")
    #print(v1)
    #print(v2)
    # Filter by edge count
    edat <- subset(edat, n>=nedgemin)
    if(nrow(edat)>0) {
      # Retain vertices on remaining edges
      v1 <- subset(v1, lab %in% edat[,"v1"])
      v2 <- subset(v2, lab %in% edat[,"v2"])
      edat[,"hovtext"] <- paste(edat[,"v1"], "; ", edat[,"v2"], "; n = ", edat[,"n"], sep="")
      # Assign vertex color by vertex set, edge color static
      vcolor <- c(rep(vColor1, nrow(v1)), rep(vColor2, nrow(v2)))
      vtcolor <- vcolor
      vtcolor[which(vcolor>"#CCCCCC")] <- "#404040"
      vtstroke <- fsc1
      vtstroke[which(vcolor>"#CCCCCC")] <- "#404040"
      ecolor <- ec1
      ehcolor <- ec2
      vertex0 <- data.frame("set"=c(rep(1, nrow(v1)), rep(2, nrow(v2))), "v"=c(v1[,"lab"], v2[,"lab"]),
                            "lab"=c(v1[,"lab"], v2[,"lab"]), "n"=c(v1[,"n"], v2[,"n"]),
                            "hovtext"=c(paste(v1[,"lab"], "; n = ", v1[,"n"], sep=""),
                                        paste(v2[,"lab"], "; n = ", v2[,"n"], sep="")))
    } else {
      vertex0 <- data.frame()
    }
  } else {
    vertex0 <- data.frame()
  }

  # Compose global vertex and edge sets
  if(nrow(vertex0)>0) {
    # Vertices
    vertex <- data.frame("id"=1:(nrow(vertex0)),
                         "fixed"=F,
                         "label"=vertex0[,"v"],
                         "color"=vcolor,
                         # Font size scaled to node observation frequency seems like agood idea, but introduces distracting variation
                         #"font"=list("color"=vtcolor, "size"=vfontsz[vertex0[,"set"]]*10*vertex0[,"n"]/max(vertex0[,"n"]), strokeWidth=1, "strokeColor"=fsc1),
                         "font"=list("color"=vtcolor, "size"=vfontsz[vertex0[,"set"]], strokeWidth=1, "strokeColor"=vtstroke),
                         "value"=vsizefactor*vertex0[,"n"]/max(vertex0[,"n"], na.rm=T),
                         "mass"=vmassf*(5+vertex0[,"n"]),
                         "title"=vertex0[,"hovtext"])
    # Include groups for legend configuration
    vertex[,"group"] <- c(nodeVar1, nodeVar2)[vertex0[,"set"]]
    #print(vertex)
    rownames(vertex) <- NULL
    # Compose vertex IDs (they are required for unambiguous identification in edge construction)
    vid <-setNames(vertex[,"id"], vertex[,"label"])
    #print(colnames(edat))
    # Compose edges
    if(nrow(edat)>0) {
      edge <- data.frame("from"=vid[edat[,"v1"]],
                         "to"=vid[edat[,"v2"]],
                         # Width
                         value=edat[,"n"],
                         #"label"=paste("n = ", edat[,"lab"], sep=""), 
                         # Hover text
                         "title"=edat[,"hovtext"],
                         "hoverWidth"=0,
                         "selectionWidth"=0,
                         "color"=list("color"=ecolor, "opacity"=eopacity, "highlight"=ehcolor),
                         # Font size scaled to node observation frequency seems like agood idea, but introduces distracting variation
                         #"font"=list("color"="white", "size"=vfontsz[1]*10*vertex0[match(vid[edat[,"v1"]], vertex[,"id"]),"n"]/max(vertex0[,"n"]), strokeWidth=1, "strokeColor"=fsc1),
                         "font"=list("color"="white", "size"=vfontsz[1], strokeWidth=1, "strokeColor"=fsc1),
                         #"length"=20,
                         "physics"=T,
                         "smooth"=T)
    } else {
      edge <- data.frame()
    }

  } else {
    vertex <- data.frame()
    edge <- data.frame()
  }

  print("net assembled")
  return(list("vertex"=vertex, "edge"=edge))

}

##########################################################################################################
# Function to append nodes and edges defined in netComponents2 to those in netComponents1
# Edges are composed by identifying node positions in appended set to those of edges defined in sets 1 and 2
# Properties of set 2 nodes in appended set are taken from set 2
##########################################################################################################

appendNetwork <- function(netComponents1, netComponents2) {

  # Append set 2 nodes to set 1
  # Limit to nodes that do not already appear in set 1
  vertex <- netComponents1[["vertex"]]
  n1 <- nrow(vertex)
  k <- which(!netComponents2[["vertex"]][,"label"] %in% vertex[,"label"])
  #print(k)
  if(length(k)>0) {
    vertex[(n1+1):(n1+length(k)),] <- netComponents2[["vertex"]][k,]
    rownames(vertex) <- NULL
    # Revise node IDs of appended vertices to position in appended set
    vertex[(n1+1):(n1+length(k)),"id"] <- (n1+1):(n1+length(k))
    #print(vertex)
  }

  # Append edges
  edge <- netComponents1[["edge"]]
  n2 <- nrow(netComponents2[["edge"]])
  if(n2>0) {
    n1 <- nrow(edge)
    edge[(n1+1):(n1+n2),] <- netComponents2[["edge"]]
    rownames(edge) <- NULL
    # Realign appended node IDs so that source corresponds to source in set 1 and destination corresponds
    # to those of newly appended nodes
    # Retrieve source and destination vertex labels
    vFrom <- netComponents2[["vertex"]][netComponents2[["edge"]][,"from"],"label"]
    vTo <- netComponents2[["vertex"]][netComponents2[["edge"]][,"to"],"label"]
    # Revise IDs of appended edge vertices using positions of source and dest labels in appended node set
    edge[(n1+1):(n1+n2),"from"] <- vertex[match(vFrom, vertex[,"label"]),"id"]
    edge[(n1+1):(n1+n2),"to"] <- vertex[match(vTo, vertex[,"label"]),"id"]
  }

  return(list("vertex"=vertex, "edge"=edge))

}

##########################################################################################################
# Function to compose graph using visNetwork() functions
# Notes:
# 1. Groups are character values assigned to vertex sets in assembleNetwork
# 2. Color parameters affect legend appearance and should agree with those used in vertex assembly
##########################################################################################################

composeNetwork <- function(vertex, edge, nodeVar1, nodeVar2, nodeVar3=NA, vColor1, vColor2, vColor3, nCluster, nearestHighlightDeg) {

  g <- visNetwork(vertex, edge) %>% 
         visGroups(groupname=nodeVar1, color=vColor1, font=list("color"=ifelse(vColor1<"#AAAAAA", "white", "#202020"), "size"=12)) %>%
         visGroups(groupname=nodeVar2, color=vColor2, font=list("color"=ifelse(vColor2<"#AAAAAA", "white", "#202020"), "size"=12))

  # Include third set of nodes, if specified
  if(!is.na(nodeVar3))
    g <- g %>% visGroups(groupname=nodeVar3, color=vColor3, font=list("color"=ifelse(vColor1<"#AAAAAA", "white", "#202020"), "size"=12))

  # Continue with composition
  g <- g %>%
         visLayout(randomSeed=1) %>%
         visLegend(useGroups=T, position="right", width=0.1, zoom=F) %>%
         visOptions(highlightNearest=list("enabled"=T, degree=nearestHighlightDeg, "hover"=T),
                    nodesIdSelection=T, selectedBy=list(variable="group", multiple=T), collapse=T) %>%
         visInteraction(hover=T, hoverConnectedEdges=T, navigationButtons=F) %>%
         #visHierarchicalLayout(direction="LR") %>%
         visPhysics(timestep=0.25, minVelocity=10, maxVelocity=50, 
                    barnesHut=list("avoidOverlap"=0.5, "springLength"=200, "springConstant"=0.5, "damping"=0.5),
                    repulsion=list("nodeDistance"=-500),
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

    conceptSelUpdate <- function(sctid=NA, FSN=NA) {}

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
        query <- paste(" match(x:ObjectConcept)-[:ISA]->(y:ObjectConcept)",
                       " where y.sctid='", conceptStack[csPtr,"sctid"], "' and x.active='1' and y.active='1'",
                       " return labels(x) as label, x.sctid as sctid, x.FSN as FSN",
                       " order by x.FSN", sep="")
        conceptList <<- cypher(dbcon, query)
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
        #print(conceptStack[csPtr,])
        output$currentRoot <- renderText(HTML(paste("<font color=blue>", conceptStack[csPtr,"FSN"], "</font>", sep="")))
        # Retrieve all nodes leading to the current root node by ISA relationships
        query <- paste(" match(x:ObjectConcept)-[:ISA]->(y:ObjectConcept)",
                       " where y.sctid='", conceptStack[csPtr,"sctid"], "' and x.active='1' and y.active='1'",
                       " return labels(x) as label, x.sctid as sctid, x.FSN as FSN",
                       " order by x.FSN", sep="")
        conceptList <<- cypher(dbcon, query)
        #print(conceptList)
        # Update selection list with current value of NA to avoid triggering an immediate update event
        updateSelectInput(session, "conceptSel", choices=conceptList[,"FSN"], selected=NA)
      }

    }, ignoreInit=T)

    ##########################################################################################################
    # Render network action
    ##########################################################################################################

    observeEvent(input$renderGraph,{

      # Retrieve node and edge data for participants connected to the current selected concept
      # Note the specification of directed edges although, in the db, they are limited to participant->concept
      # Also note the retrieval of unique combinations of participant and concept to avoid bias due to duplicate observations 
      query <- paste(" match(x:Participant)-[:P_SCT]->(y:ObjectConcept)-[:ISA]->(z:ObjectConcept)",
                     " where z.sctid='", conceptStack[csPtr,"sctid"], "' and y.active='1' and z.active='1'",
                     " return distinct",
                     " case when(labels(x)=['Participant'])then 'Participant'",
                     "             when(labels(x)=['Participant','UCD_Proximal'])then 'UCDProx'",
                     "        end as UCDProx,",
                     " x.ParticipantId as participantID, x.Sex as Sex, x.UCDDx as UCDDx,",
                     " case when(toInteger(x.OnsetAgeDays)<11)then '0-11'",
                     "      when(toInteger(x.OnsetAgeDays)<101)then '11-100'",
                     "      when(toInteger(x.OnsetAgeDays)<1001)then '101-1000'",
                     "      when(toInteger(x.OnsetAgeDays)<10001)then '1001-10000'",
                     "      when(toInteger(x.OnsetAgeDays) is not null)then '>10000'",
                     "      else null",
                     " end as onsetAgeDays, labels(y) as conceptLabel,",
                     " y.sctid as conceptID, y.FSN as FSN", sep="")
      #print(query)
      pConcept <- cypher(dbcon, query)
      #print(pConcept)

      # Assemble concept and participant nodes and edges
      netComponents <<- assembleNetComponents(pConcept, "FSN", input$nodeVar, vc1, vc2, input$nedgemin, input$eopacity, input$vMassFactor,
                                              c(0.8*input$vFontSize, input$vFontSize))
      #print(netComponents)

      # Include prescriptions, if requested
      if(input$prescripConnector != "none") {
        # Retrieve node and edge data for prescriptions connected to participants connected to the current selected concept
        # Note the specification of directed edges although, in the db, they are limited to participant->rx
        # Also, the participant node appears between P_RX and P_SCT because it has such relationships to RX and ObjectConcept
        # The query match(w:Participant)-[:P_RX]->(x:RXCUI)-[:P_SCT]->(y:ObjectConcept) returns null because RX has no relationships directed to ObjectConcept
        if(input$prescripSubsume) {
          query <- paste("match(v:RXCUI)-[:SUBSUMES]->(w:RXCUI)<-[:P_RX]-(x:Participant)-[:P_SCT]->(y:ObjectConcept)-[:ISA]->(z:ObjectConcept)",
                         "where z.sctid='", conceptStack[csPtr,"sctid"], "' and v.status='Active' and w.status='Active' and y.active='1' and z.active='1'", sep="")
        } else {
          query <- paste("match(w:RXCUI)<-[:P_RX]-(x:Participant)-[:P_SCT]->(y:ObjectConcept)-[:ISA]->(z:ObjectConcept)",
                         "where z.sctid='", conceptStack[csPtr,"sctid"], "' and w.status='Active' and y.active='1' and z.active='1'", sep="")
        }
        query <- paste(query,
                       " and toLower(w.category)='drug'",
                       " return distinct",
                       " case when(labels(x)=['Participant'])then 'Participant'",
                       "             when(labels(x)=['Participant','UCD_Proximal'])then 'UCDProx'",
                       "        end as UCDProx,",
                       " x.ParticipantId as participantID, x.Sex as Sex, x.UCDDx as UCDDx,",
                       " case when(toInteger(x.OnsetAgeDays)<11)then '0-11'",
                       "      when(toInteger(x.OnsetAgeDays)<101)then '11-100'",
                       "      when(toInteger(x.OnsetAgeDays)<1001)then '101-1000'",
                       "      when(toInteger(x.OnsetAgeDays)<10001)then '1001-10000'",
                       "      when(toInteger(x.OnsetAgeDays) is not null)then '>10000'",
                       "      else null",
                       " end as onsetAgeDays, y.sctid as conceptID, y.FSN as FSN, ",
                       ifelse(input$prescripSubsume, "v.name as Prescription", "w.name as Prescription"), sep="")
        print(query)
        pPrescrip <- cypher(dbcon, query)
        #print(pPrescrip)

        # Assemble prescription nodes with edges to specified set of connection nodes
        # Specify concept or participant node first, so that they align with the order assumed by appendPrescriptions()
        netComponentsP <- assembleNetComponents(pPrescrip, ifelse(input$prescripConnector=="concept", "FSN", input$nodeVar),
                                                "Prescription", vc1, vc3, input$nedgemin, input$eopacity, input$vMassFactor,
                                                c(input$vFontSize, 0.7*input$vFontSize))
        #print(netComponentsP)
        #print(netComponentsP[["vertex"]][,"label"])
        #print(colnames(netComponentsP[["vertex"]]))
        #print(colnames(netComponentsP[["edge"]]))

        # Append prescription nodes and edges to network
        netComponents <<- appendNetwork(netComponents, netComponentsP)

      }

      # Render graph
      if(nrow(netComponents[["vertex"]])>0) {
        # Net regen is always done with physics enabled, but we want it to be disablead after regen
        # Direct disabling of physics (using visPhysics(enabled=F)) has no effect when called immediately after
        # renderVisNetwork(), but is effective when executed frimm within a shiny reactive function
        # So, although not ideal, force disable of physics by toggling the reaction control with physics par val
        updateRadioButtons(session, "physics", selected=T)
        output$g1 <- renderVisNetwork(composeNetwork(vertex=netComponents[["vertex"]], edge=netComponents[["edge"]],
                                                     nodeVar1="FSN", nodeVar2=input$nodeVar, ifelse(input$prescripConnector!="none", "Prescription", NA),
                                                     vColor1=vc1, vColor2=vc2, vColor3=vc3,
                                                     nCluster=input$nCluster, nearestHighlightDeg=input$nearestHighlightDeg))
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
    # Verify that a vertex has been clicked
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
        ke <- which(edge[,"from"]==v0 | edge[,"to"]==v0)
        # Identify all vertices connected to selected vertex
        kv <- which(vertex[,"id"] %in% unlist(edge[ke,c("from", "to")]))
        # Hide vertices that are not connected to selected vertex
        vertex[,"hidden"] <- T
        vertex[kv,"hidden"] <- F
        vertex[,"physics"] <- F
        vertex[kv,"physics"] <- T
        output$g1 <- renderVisNetwork(composeNetwork(vertex=vertex, edge=edge[ke,],
                                                     nodeVar1="FSN", nodeVar2=input$nodeVar, ifelse(input$prescripConnector!="none", "Prescription", NA),
                                                     vColor1=vc1, vColor2=vc2, vColor3=vc3,
                                                     nCluster=input$nCluster, nearestHighlightDeg=input$nearestHighlightDeg))
        #updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
        #updateRadioButtons(session=session, inputId="physics", selected=F)
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Restore hidden vertices event
    ##########################################################################################################

    observeEvent(input$restoreVertex, {
      print("restoreVertex")
      output$g1 <- renderVisNetwork(composeNetwork(vertex=netComponents[["vertex"]], edge=netComponents[["edge"]],
                                                   nodeVar1="FSN", nodeVar2=input$nodeVar, ifelse(input$prescripConnector!="none", "Prescription", NA),
                                                   vColor1=vc1, vColor2=vc2, vColor3=vc3,
                                                   nCluster=input$nCluster, nearestHighlightDeg=input$nearestHighlightDeg))
      #updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
      #updateRadioButtons(session=session, inputId="physics", selected=F)
    }, ignoreInit=T)

    ##########################################################################################################
    # Execution begins here
    ##########################################################################################################

    # Query SNOMED root node
    csPtr <- 1
    query <- "match(x) where x.FSN contains 'SNOMED CT Concept' and x.active='1' return labels(x) as label, x.sctid as sctid, x.FSN as FSN limit 1"
    conceptStack[csPtr,] <- cypher(dbcon, query)[1,]
    #print(conceptStack)
    output$currentRoot <- renderText(HTML(paste("<font color=blue>", conceptStack[csPtr,"FSN"], "</font>", sep="")))

    # Retrieve all nodes leading to the current root node by ISA relationships
    query <- paste(" match(x:ObjectConcept)-[:ISA]-(y:ObjectConcept)",
                   " where y.sctid='", conceptStack[csPtr,"sctid"], "' and x.active='1' and y.active='1'",
                   " return labels(x) as label, x.sctid as sctid, x.FSN as FSN",
                   " order by x.FSN", sep="")
    conceptList <- cypher(dbcon, query)
    #print(conceptList)

    query <- paste(" match(x:ObjectConcept)-[:ISA]->(y:ObjectConcept)",
                   " where y.sctid='", conceptStack[csPtr,"sctid"], "' and x.active='1' and y.active='1'",
                   " return labels(x) as label, x.sctid as sctid, x.FSN as FSN",
                   " order by x.FSN", sep="")
    conceptList <- cypher(dbcon, query)
    #print(conceptList)


    # Update selection list with current value of NA to avoid triggering an immediate update event
    updateSelectInput(session, "conceptSel", choices=conceptList[,"FSN"], selected=NA)

  }
)