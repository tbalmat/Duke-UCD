#####################################################################################################
# Duke University UCD Shiny App, Apr 2020
# Graph functions
#####################################################################################################

##########################################################################################################
# Function to assemble graph components (vertices and edges)
##########################################################################################################

assembleNetComponents <- function(pConcept, nodeVar1, nodeVar2, vColor1, vColor2, nedgemin,
                                  eopacity, vmassf, vsizefactor, vfontsz, vfontcolor, elabVar, accMethod) {

  #print(nodeVar1)
  #print(nodeVar2)
  #print(colnames(pConcept))
  #print(pConcept)

  # Compose graph data set, with participant groups and concepts as vertices and relationship of
  # participant property and concept as edges
  # Parameters:
  # pConcept ...... a data frame with one row per nodeVar1, nodeVar2 combination
  # nodeVar1 ...... name of pConcept column from which to construct the first set of vertices
  # nodeVar2 ...... name of pConcept column from which to construct the second set of vertices
  # vColorX ....... color to assign vertex set x
  # nedgemin ...... restrict edges to those with computed n (as in label) >= this value
  # eopacity ...... edge opacity
  # vmassf ........ vertex mass factor, used in physics computations, low -> tighter graph
  # vsizefactor ... vertex rendering scale factor
  # vfontsize ..... vertex label font size (two position vector, one for each vertex set)
  # vfontcolor .... vertex label font color (two position vector, one for each vertex set)
  # elabVar ....... pConcept vector to include in edge labels
  # accMethod ..... method used to accumulate node and vertex observation frequencies
  #                 possible values are:
  #                 PID ............ count unique participant IDs for v1 frequencies
  #                                  count unique participant IDs for v2 frequencies
  #                                  count intersection of unique v1, v2 participant IDs for edge frequencies
  #                 V1PIDV2nEn ..... count unique participant IDs for v1 frequencies
  #                                  accumulate n by nodeVar2 values for v2 frequencies
  #                                  accumulate n by nodeVar1, nodeVar2 combinations for edge frequencies
  #                 V1PIDV2PIDEn ... count unique participant IDs for v1 frequencies
  #                                  count unique participant IDs for v2 frequencies
  #                                  count unique participant ID, nodeVar2 values for edge frequencies

  # Note the existence of column n in pConcept
  # This indicates the frequency of pairs of observations for each nodeVar1, nodeVar2 combination (row)

  if(nrow(pConcept)>0) {

    # Identify column that uniquely identifies (in the database) nodeVar1 and nodeVar2
    # IDs are assumed to be invariant for each value of nodeVarX (aggregate by parameter will have
    # a single level for each nodeVarX level)
    # IDs are used to uiquely identify a node, since it is possible to label distinct (ID) nodes equivalently
    if(nodeVar1=="FSN") {
      nodeIDvar1 <- "conceptID"
    } else if(nodeVar1=="Prescription") {
      nodeIDvar1 <- "prescriptionID"
    } else if(nodeVar1=="roleGroupFSN") {
      nodeIDvar1 <- "roleGroupID"
    } else {
      nodeIDvar1 <- nodeVar1
    }
    if(nodeVar2=="FSN") {
      nodeIDvar2 <- "conceptID"
    } else if(nodeVar2=="Prescription") {
      nodeIDvar2 <- "prescriptionID"
    } else if(nodeVar2=="roleGroupFSN") {
      nodeIDvar2 <- "roleGroupID"
    } else {
      nodeIDvar2 <- nodeVar2
    }

    # Tabulate vertices
    # Vertex set v1 corresponds to nodeVar1, set v2 to nodeVar2
    # If accMethod is unrecognized then frequencies for unique values of nodeVar1 and nodeVar2 are accumulated
    v1 <- aggregate(1:nrow(pConcept), by=list(rep(nodeVar1, nrow(pConcept)), pConcept[,nodeVar1], pConcept[,nodeIDvar1]),
                    function(k)
                      if(tolower(accMethod) %in% c("pid", "v1pidv2nen", "v1pidv2piden")) {
                        length(unique(pConcept[k,"participantID"]))
                      } else {
                        length
                      })
    colnames(v1) <- c("nodeVar", "lab", "dbID", "n")
    v2 <- aggregate(1:nrow(pConcept), by=list(rep(nodeVar2, nrow(pConcept)), pConcept[,nodeVar2], pConcept[,nodeIDvar2]),
                    function(k)
                      if(tolower(accMethod) %in% c("pid", "v1pidv2piden")) {
                        length(unique(pConcept[k,"participantID"]))
                      } else if(tolower(accMethod)=="v1pidv2nen") {
                        sum(pConcept[k,"n"])
                      } else {
                        length
                      })
    colnames(v2) <- c("nodeVar", "lab", "dbID", "n")
    #print(v1)
    #print(v2)

    # Tabulate edges by nodeVar1, nodeVar2 combinations
    # If accMethod is unrecognized then frequencies of unique nodeVar1, nodeVar2 values are accumulated
    if(!is.na(elabVar)) {
      agglist <- list(pConcept[,nodeVar1], pConcept[,elabVar], pConcept[,nodeVar2])
      cname <- c("v1", "edge", "v2", "n")
    } else {
      agglist <- list(pConcept[,nodeVar1], pConcept[,nodeVar2])
      cname <- c("v1", "v2", "n")
    }
    edat <- aggregate(1:nrow(pConcept), by=agglist,
                      function(k)
                        if(tolower(accMethod)=="pid") {
                          length(unique(pConcept[k,"participantID"]))
                        } else if(tolower(accMethod) %in% c("v1pidv2piden", "v1pidv2nen")) {
                          sum(pConcept[k,"n"])
                        } else {
                          length
                        })
    colnames(edat) <- cname
    #print(edat)
    # Filter edges by frequency
    edat <- subset(edat, n>=nedgemin)
    if(nrow(edat)>0) {
      # Retain vertices on remaining edges
      v1 <- subset(v1, lab %in% edat[,"v1"])
      v2 <- subset(v2, lab %in% edat[,"v2"])
      if(!is.na(elabVar)) {
        edat[,"hovtext"] <- paste(edat[,"v1"], "; ", edat[,"edge"], "; ", edat[,"v2"], "; n = ", edat[,"n"], sep="")
      } else {
        edat[,"hovtext"] <- paste(edat[,"v1"], "; ", edat[,"v2"], "; n = ", edat[,"n"], sep="")
      }
      vset <- c(rep(1, nrow(v1)), rep(2, nrow(v2)))
      vertex0 <- data.frame("set"=vset,
                            "v"=c(v1[,"lab"], v2[,"lab"]),
                            "vColor"=c(vColor1, vColor2)[vset],
                            "lab"=c(v1[,"lab"], v2[,"lab"]),
                            "vfontcolor"=vfontcolor[vset],
                            "vfontsz"=vfontsz[vset],
                            "vfstroke"=ifelse(vfontcolor[vset]<"#CCCCCC", fsc1, "#404040"),
                            "n"=c(v1[,"n"], v2[,"n"]),
                            "hovtext"=c(paste(v1[,"lab"], "; n = ", v1[,"n"], sep=""),
                                        paste(v2[,"lab"], "; n = ", v2[,"n"], sep="")),
                            "varClass"=c(v1[,"nodeVar"], v2[,"nodeVar"]),
                            "dbID"=c(v1[,"dbID"], v2[,"dbID"]))
    } else {
      vertex0 <- data.frame()
    }
  } else {
    vertex0 <- data.frame()
  }

  #print(vertex0)

  # Compose vertex and edge sets
  if(nrow(vertex0)>0) {
    # Vertices
    vertex <- data.frame("id"=1:(nrow(vertex0)),
                         "fixed"=F,
                         "label"=vertex0[,"v"],
                         "color"=vertex0[,"vColor"],
                         # Font size scaled to node observation frequency seems like agood idea, but introduces distracting variation
                         #"font"=list("color"=vtcolor, "size"=vfontsz[vertex0[,"set"]]*10*vertex0[,"n"]/max(vertex0[,"n"]), strokeWidth=1, "strokeColor"=fsc1),
                         "font"=list("color"=vertex0[,"vfontcolor"], "size"=vertex0[,"vfontsz"], strokeWidth=1, "strokeColor"=vertex0[,"vfstroke"]),
                         "value"=vsizefactor*vertex0[,"n"],
                         "mass"=vmassf*(5+vertex0[,"n"]),
                         "title"=vertex0[,"hovtext"],
                         "varClass"=vertex0[,"varClass"],
                         "dbID"=vertex0[,"dbID"])
    #print(vertex)
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
                         "color"=list("color"=ec1, "opacity"=eopacity, "highlight"=ec2),
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
  # Node IDs cannot be used for comparison because the vertex sets were constructed independently
  # Use node labels and hope there are no duplicates
  # Perhaps dbIDs would provide unambiguous identification
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
# Function to compose renderable graph using visNetwork() functions
# Notes:
# 1. Groups are character values assigned to vertex sets in assembleNetwork
# 2. Color parameters affect legend appearance and should agree with those used in vertex assembly
##########################################################################################################

composeNetwork <- function(vertex, edge, nodeVar1, nodeVar2, nodeVar3=NA, nodeVar4=NA, nodeVar5=NA,
                           vColor1, vColor2, vColor3, vColor4, vColor5, nCluster, nearestHighlightDeg) {

  g <- visNetwork(vertex, edge) %>% 
         visGroups(groupname=nodeVar1, color=vColor1, font=list("color"=ifelse(vColor1<"#AAAAAA", "white", "#202020"), "size"=12)) %>%
         visGroups(groupname=nodeVar2, color=vColor2, font=list("color"=ifelse(vColor2<"#AAAAAA", "white", "#202020"), "size"=12))

  # Include third set of nodes, if specified
  if(!is.na(nodeVar3))
    g <- g %>% visGroups(groupname=nodeVar3, color=vColor3, font=list("color"=ifelse(vColor3<"#AAAAAA", "white", "#202020"), "size"=12))

  # Include fourth set of nodes, if specified
  if(!is.na(nodeVar4))
    g <- g %>% visGroups(groupname=nodeVar4, color=vColor4, font=list("color"=ifelse(vColor4<"#AAAAAA", "white", "#202020"), "size"=12))

  # Include fifth set of nodes, if specified
  if(!is.na(nodeVar5))
    g <- g %>% visGroups(groupname=nodeVar5, color=vColor5, font=list("color"=ifelse(vColor5<"#AAAAAA", "white", "#202020"), "size"=12))

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
         # Trigger shiny events using mouse clicks with shift and ctl keys
         # Double click events fire two click events, so use shift-click for doubles
         # The following (java) instructions cause changes to input$shiftClick and input$ctlClick
         # See the corresponding event handlers for additional info
         visEvents(type="on", click="function(obj) {
                                       if(obj.event.srcEvent.shiftKey) {
                                         //alert('shift-click')
                                         Shiny.onInputChange('shiftClick', obj)
                                       } else if(obj.event.srcEvent.altKey) {
                                         //alert('alt-click')
                                         Shiny.onInputChange('altClick', obj)
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