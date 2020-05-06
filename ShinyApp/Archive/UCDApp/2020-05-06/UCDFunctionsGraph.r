#####################################################################################################
# Duke University UCD Shiny App, Apr 2020
# Graph functions
#####################################################################################################

##########################################################################################################
# Function to assemble network using current graph configuration
# Result is a list containing two data frames named "vertex" and "edge"
##########################################################################################################

assembleNetworkComponents <- function() {

  # Note the use of the global data frame netData as the source of observations
  if(nrow(netData)>0) {

    # Make a local copy of the variable connection list, since it may be modified here
    gconn <- graphCfg[[gcPtr]][["connect"]]

    # Evaluate interaction request(s)
    for(i in 1:2) {
      inx1 <- graphCfg[[gcPtr]][["interact"]][[paste("set", i, sep="")]]
      inx2 <- graphCfg[[gcPtr]][["interact"]][[paste("conn", i, sep="")]]
      # Require two or more variables in the interaction and at least one connected var
      if(length(inx1)>1 & length(inx2)>0) {
        # Retain positions of interaction set in vcfg
        kv <- match(inx1, vcfg[,"cID"])
        # Subsume Rx in interaction set, if requested
        k <- which(inx1=="Rx")
        if(length(k)>0)
          if(graphCfg[[gcPtr]][["connect"]][["rxSubsume"]]) {
            inx1[k] <- "rxSubsName"
          } else {
            inx1[k] <- "rxName"
          }
        # Append interaction set to variable configuration
        # Note that a local copy of vcfg is made with no effect on the global instance
        vID <- paste(inx1, collapse="_X_", sep="")
        vcfg <- rbind(vcfg, data.frame(# New var with name of all vars in set
                                       "vID"=vID,
                                       "dbID"=vID,
                                       "cID"=vID,
                                       # Concatenate presentation names for appearance
                                       "mID"=paste(vcfg[kv,"mID"], collapse=" -X- ", sep=""),
                                       # Average red, green, blue color components of vars in set
                                       "vColor"=rgb(matrix(apply(col2rgb(vcfg[kv,"vColor"]), 1, mean)/255, nrow=1)),
                                       # Average text sizes
                                       "tSize"=mean(vcfg[kv,"tSize"]),
                                       "tColor"="black",
                                       "elabVar"=NA,
                                       "accMethod"="PID"))
        # Include combined levels in observation data
        if(!vID %in% colnames(netData))
          netData[vID] <- apply(as.matrix(netData[,inx1]), 1, function(x) paste(x, collapse=" -X- ", sep=""))
        # Append connection vector to the connection list
        gconn[[vID]] <- inx2
      }
    }

    # Assemble pairs of node variable indices to be graphed
    # The first element in each pair is the index into vcfg[,"vID"] of one variable to be connected
    # The second element is the index into vcfg[,"vID"] of the other variable in the relationship  
    vpair <- do.call(rbind,
               apply(as.matrix(1:nrow(vcfg)), 1,
                 function(i)
                   if(length(gconn[[vcfg[i,"cID"]]])>0) {
                     return(data.frame(i, match(gconn[[vcfg[i,"cID"]]], vcfg[,"cID"])))
                   } else {
                     return(list())
                   }))

    if(ncol(vpair)>0) {

      # Reconfigure contextual vertex and edge properties
      if(graphCfg[[gcPtr]][["connect"]][["rxSubsume"]]) {
        vcfg[which(vcfg[,"vID"]=="Rx"), "vID"] <- "rxSubsName"
        vcfg[which(vcfg[,"dbID"]=="Rx"), "dbID"] <- "rxSubsID"
      } else {
        vcfg[which(vcfg[,"vID"]=="Rx"), "vID"] <- "rxName"
        vcfg[which(vcfg[,"dbID"]=="Rx"), "dbID"] <- "rxID"
      }
      vcfg[,"tSize"] <- graphCfg[[gcPtr]][["vfontsz"]]*vcfg[,"tSize"]

      # Save a copy of the vertex (variable) configuration to be used during rendering
      # Importantly, interaction variables may have been appended and these do not
      # appear in the global v configuration table
      graphCfgOp("vcfg", vcfg=vcfg)

      # Eliminate synonymous pairs (1->2 requested with 2->1) and any unison pairs 
      k <- which(vpair[,1]>vpair[,2])
      x <- vpair[k,1]
      vpair[k,1] <- vpair[k,2]
      vpair[k,2] <- x
      vpair <- vpair[which(!duplicated(vpair) & vpair[,1]!=vpair[,2]),]

      # Compose ordered vector of unique indices into vcfg for node construction
      kvar <- sort(unique(c(vpair[,1], vpair[,2])))

      # Tabulate vertices
      # The resulting list has one element per variable defined in vpair
      # If accMethod is unrecognized then frequencies for unique values of a variable are accumulated
      vdat <- do.call(rbind,
                lapply(kvar,
                  function(i)
                    setNames(aggregate(1:nrow(netData), by=list(rep(i, nrow(netData)), netData[,vcfg[i,"vID"]],
                                                                netData[,vcfg[i,"dbID"]]),
                               function(k)
                                 if(tolower(vcfg[i,"accMethod"]) %in% c("pid", "v1pidv2nen", "v1pidv2piden")) {
                                   length(unique(netData[k,"participantID"]))
                                 } else {
                                   length
                                 }), c("vi", "lab", "dbID", "n"))))

      # Tabulate edges, one edge for each combination of levels of node variables
      # The resulting list has one element per pair of variables defined in vpair
      # If accMethod is unrecognized then frequencies for unique values of variable pairs are accumulated
      edat <- do.call(rbind,
                lapply(1:nrow(vpair),
                  function(i) {
                    # Include additional label variable(s), if specified
                    # Concatenate row-wise column values where vcfg[,"elabVar"] is other than NA
                    if(!is.na(vcfg[vpair[i,1],"elabVar"])) {
                      elab <- netData[,vcfg[vpair[i,1],"elabVar"]]
                    } else {
                      elab <- rep("", nrow(netData))
                    }
                    if(!is.na(vcfg[vpair[i,2],"elabVar"]))
                      elab <- paste(elab, netData[,vcfg[vpair[i,2],"elabVar"]],
                                    sep=ifelse(!is.na(vcfg[vpair[i,1],"elabVar"]), "-", ""))
                    # Aggregate joint frequencies
                    setNames(aggregate(
                               1:nrow(netData),
                               by=list(rep(vpair[i,1], nrow(netData)), rep(vpair[i,2], nrow(netData)),
                                       netData[,vcfg[vpair[i,1],"vID"]], netData[,vcfg[vpair[i,2],"vID"]], elab),
                               function(k)
                                 if(tolower(vcfg[i,"accMethod"]) %in% c("pid", "v1pidv2nen", "v1pidv2piden")) {
                                   length(unique(netData[k,"participantID"]))
                                 } else {
                                   length
                                 }), c("vpairi", "vpairj", "v1", "v2", "elab", "n"))
                  }))

      # Filter edges, omitting those below minimum specified frequency
      edat <- subset(edat, n>=graphCfg[[gcPtr]]["nedgemin"])

      if(nrow(edat)>0) {

        # Compose vertex definitions for those that remain in edge relationships
        vertex <- do.call(rbind,
                    lapply(kvar,
                      function(k) {
                        kv <- which(vdat[,"vi"]==k &
                                    vdat[,"lab"] %in% c(edat[which(edat[,"vpairi"]==k),"v1"],
                                                        edat[which(edat[,"vpairj"]==k),"v2"])) 
                        data.frame("vi"=k,
                                   "fixed"=F,
                                   "label"=vdat[kv,"lab"],
                                   "color"=vcfg[k,"vColor"],
                                   "font"=list("color"=vcfg[k,"tColor"],
                                               "size"=vcfg[k,"tSize"],
                                               "strokeWidth"=1,
                                               "strokeColor"=ifelse(vcfg[k,"tColor"]<"#CCCCCC", fsc1, "#404040")),
                                   "value"=vdat[kv,"n"],
                                   "mass"=graphCfg[[gcPtr]][["vmassf"]]*(5+vdat[kv,"n"]),
                                   "title"=paste(vdat[kv,"lab"], "; n = ", vdat[kv,"n"], sep=""),
                                   # Identify the variable class that a node belongs to
                                   # This is used, later, during node subsetting and expanding (to further query observations)
                                   "varClass"=vcfg[k,"cID"],
                                   # Groups are used to identify a collection of nodes
                                   # These will also appear in the legend, so use verbose descriptors 
                                   "group"=vcfg[k,"mID"],
                                   "dbID"=vdat[kv,"dbID"])
                      }))
        vertex[,"id"] <- 1:nrow(vertex)

        # Compose edge definitions
        edge <- do.call(rbind,
                  lapply(1:nrow(edat),
                    function(i) {
                      kf <- which(vertex[,"vi"]==edat[i,"vpairi"] & vertex[,"label"]==edat[i,"v1"])
                      kt <- which(vertex[,"vi"]==edat[i,"vpairj"] & vertex[,"label"]==edat[i,"v2"])
                      data.frame("from"=vertex[kf,"id"],
                                 "to"=vertex[kt,"id"],
                                 # Width
                                 value=edat[i,"n"],
                                 #"label"=paste("n = ", edat[i,"lab"], sep=""), 
                                 # Hover text
                                 "title"=paste(edat[i,"v1"],
                                               ifelse(!edat[i,"elab"] %in% c("", "na"), paste("; ", edat[i,"elab"], sep=""), ""),
                                               "; ", edat[i,"v2"], "; n=", edat[i,"n"], sep=""),
                                 "hoverWidth"=0,
                                 "selectionWidth"=0,
                                 "color"=list("color"=ec1, "opacity"=graphCfg[[gcPtr]][["eopacity"]], "highlight"=ec2),
                                 # Font size scaled to node observation frequency seems like agood idea, but introduces distracting variation
                                 #"font"=list("color"="white", "size"=vfontsz[1]*10*vertex0[match(vid[edat[,"v1"]],
                                 #       vertex[,"id"]),"n"]/max(vertex0[,"n"]), strokeWidth=1, "strokeColor"=fsc1),
                                 "font"=list("color"="white", "size"=vcfg[edat[i,"vpairi"],"tSize"], strokeWidth=1, "strokeColor"=fsc1),
                                 #"length"=20,
                                 "physics"=T,
                                 "smooth"=T)
                    }))

        # Package the results
        netComponents <- list("vertex"=vertex, "edge"=edge)

      } else {

        netComponents <- list("vertex"=data.frame(), "edge"=data.frame())

      }

    } else {

      netComponents <- list("vertex"=data.frame(), "edge"=data.frame())

    }

  } else {

    netComponents <- list("vertex"=data.frame(), "edge"=data.frame())

  }

  print("net assembled")
  return(netComponents)

}

##########################################################################################################
# Function to compose renderable graph using visNetwork() functions
##########################################################################################################

composeNetwork <- function(netComponents) {

  # Parameters:
  # netComponents ..... list with "vertex" and "edge" elements, data frames containing vertex and edge
  #                     configurations in visNetwork format
print("KKKKKKKKKKKKKKKKKKKKKKKKKKKK")
print(graphCfg[[gcPtr]][["vcfg"]])
  # Compose legend text and color vectors
  # Color parameters affect legend appearance and should agree with groups specified in netComponents
  legendText <- graphCfg[[gcPtr]][["vcfg"]][unique(netComponents[["vertex"]][,"vi"]),"mID"]
  legendColor <- graphCfg[[gcPtr]][["vcfg"]][unique(netComponents[["vertex"]][,"vi"]),"vColor"]

  # Create graph
  g <- visNetwork(netComponents[["vertex"]], netComponents[["edge"]])

  # Append legend text and colors
  for(i in 1:length(legendText))
    g <- g %>% visGroups(groupname=legendText[i], color=legendColor[i],
                 font=list("color"=ifelse(mean(col2rgb(legendColor[i]))<175, "white", "#202020"), "size"=12))

  # Apply visualization features
  g <- g %>%
    visLayout(randomSeed=1) %>%
    visLegend(useGroups=T, position="right", width=0.1, zoom=F) %>%
    visOptions(highlightNearest=list("enabled"=T, degree=graphCfg[[gcPtr]][["nearestHighlightDeg"]], "hover"=T),
               #nodesIdSelection=T, selectedBy=list(variable="group", multiple=T),
               collapse=T) %>%
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
  if(graphCfg[[gcPtr]][["nCluster"]]>0)
    g <- g %>% visClusteringByHubsize(size=graphCfg[[gcPtr]][["nCluster"]])

  print("net composed")
  return(g)

}
