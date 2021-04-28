#####################################################################################################
# Duke University UCD Shiny App, Apr 2020
# Graph functions
#####################################################################################################

##########################################################################################################
# Function to assemble network using current graph configuration
# Result is a list containing two data frames named "vertex" and "edge"
# Vertices are limited to those with at least one edge to another vertex
##########################################################################################################

assembleNetworkComponents <- function() {

  if(nrow(netData)>0) {

    # Data validation
    #print(aggregate(1:nrow(netData), by=list(netData[,"UCDDx"], netData[,"HASxLast"]), function(k) length(unique(netData[k,"participantID"]))))

    # Force filter
    #print("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    #print(netData[1:10, "HASxLast"])
    #print(netData[1:10, "UCDDx"])
    #graphCfg[[gcPtr]][["filter"]] <- list("primary"=list("UCDDx"=c("ALD", "ASD")))
    #                                      #"interaction"=list(data.frame("HASxLast"="z", "UCDDx"="z")))

    #print("assemble.graphCfg")
    #print("GGGGGGGGGGGGGGGGGGGGGGGGGGGGGG")
    #print(str(graphCfg[[gcPtr]]))
    #print(graphCfg[[gcPtr]][["connect"]])
    #print(graphCfg[[gcPtr]][["filter"]])
    #print(graphCfg[[gcPtr]][["rxLeadCharFilter"]])
    #print("HHHHHHHHHHHHHHHHHHHHHHHHHHHHHH")

    # Copy graph connection cfg (for ease of script interpretation)
    vconn <- graphCfg[[gcPtr]][["connect"]]

    # Retrieve concept groups
    # These are used to compose joint-concept IDs and bypass concept joining when requested but
    # with only one concept present
    conceptOrder <- sort(unique(netData[,"conceptOrder"]))

    # Assemble pairs of primary (non-interaction) variables specified for connection
    # Vertex and edge data will be composed for them
    cPri <- do.call(rbind,
                    lapply(rownames(vcfg),
                      function(v)
                        if(length(vconn[[v]])>0) {
                          data.frame("v1"=v, "v2"=vconn[[v]])
                        } else {
                          data.frame()
                        }))

    # Copy interaction cfg
    cInteract <- graphCfg[[gcPtr]][["interact"]]

    # Proceed if any connections specified
    if(nrow(cPri)>0 | length(cInteract[["set1"]])>0 & length(cInteract[["conn1"]])>0 |
                      length(cInteract[["set2"]])>0 & length(cInteract[["conn2"]])>0) {

      # Compose set of non-interaction vertices to be constructed
      # This includes primary and vertices that interaction vertices are connected to
      if(nrow(cPri)>0) {
        # Eliminate synonymous connection pairs (1->2 requested with 2->1)
        # Arrange all pairs alphabetically
        k <- which(cPri[,"v1"]>cPri[,"v2"])
        x <- cPri[k,"v1"]
        cPri[k,"v1"] <- cPri[k,"v2"]
        cPri[k,"v2"] <- x
        # Eliminate duplicate pairs
        # Note that pairs with ID1=ID2 are retained so that intra-variable connections are
        # possible (to relate SNOMED concepts from different groups (order), for instance)
        cPri <- cPri[which(!duplicated(cPri)),]
        # Compose vector of primary and interaction variables for vertex creation
        vxPri <- unique(c(cPri[,"v1"], cPri[,"v2"], cInteract[["conn1"]], cInteract[["conn2"]]))
      } else {
        vxPri <- unique(c(cInteract[["conn1"]], cInteract[["conn2"]]))
      }

      # Collapse observations by omitting Rx when not specified in any vertex or connection cfg
      # Participants are, typically, associated with multiple Rx, causing the following joins to
      # produce many duplicates of observation segments (coneptID, FSN, etc.) that are not needed
      # Note the creation of a local copy of netData
      if(!"rxID" %in% c(vxPri, cInteract[["set1"]], cInteract[["set2"]])) {
        netData[,c("rxSubsID", "rxSubsName", "rxID", "rxName")] <- "na"
        netData <- netData[!duplicated(netData),]
      }

      # Configure joint-concepts when requested, multiple concepts specified, and concept appears in a connection specification
      # Note the creation or use of a local copy of netData
      if(vconn[["joinConcept"]]==T & "conceptID" %in% c(vxPri, cInteract[["set1"]], cInteract[["set2"]]) & length(conceptOrder)>1) {
        # Join concepts of successive order by participant ID
        # Retain IDs, FSNs, and finding sites for composition of vertex labels
        k <- which(netData[,"conceptOrder"]==conceptOrder[1])
        jc <- netData[k,]
        # Copy columns associated with concepts of first order
        jc <- cbind(jc, jc[,c("conceptID", "conceptFSN", "findingSiteID", "findingSiteFSN")])
        names(jc) <- c(names(netData), paste("jt", c("conceptID", "conceptFSN", "findingSiteID", "findingSiteFSN"), conceptOrder[1], sep=""))
        for(j in 2:length(conceptOrder)) {
          nm <- names(jc)
          k <- which(netData[,"conceptOrder"]==conceptOrder[j])
          # Join prior concepts with columns associated with jth order
          jc <- merge(jc, netData[k,c("participantID", "conceptID", "conceptFSN", "findingSiteID", "findingSiteFSN")],
                      by="participantID")
          names(jc) <- c(nm, paste("jt", c("conceptID", "conceptFSN", "findingSiteID", "findingSiteFSN"), conceptOrder[j], sep=""))
        }
        netData <- jc
        # Construct composite concept and finding site FSNs
        j <- paste("jtconceptFSN", conceptOrder, sep="")
        netData[,"conceptFSN"] <- unlist(lapply(1:nrow(netData), function(i) paste(netData[i,j], collapse=" -X- ", sep="")))
        j <- paste("jtfindingSiteFSN", conceptOrder, sep="")
        netData[,"findingSiteFSN"] <- unlist(lapply(1:nrow(netData), function(i) paste(netData[i,j], collapse=" -X- ", sep="")))
        #print(head(netData))
        rm(jc)
        joinConcept <- T
      } else {
        joinConcept <- F
      }

      # Collapse HA, if requested and HA appears in any connection
      # Note the creation or use of a local copy of netData
      if(vconn[["groupHA"]] & "HASxLast" %in% c(vxPri, cInteract[["set1"]], cInteract[["set2"]])) {
        k1 <- which(netData[,"HASxLast"]=="HA Events with or without Symptoms")
        k2 <- which(netData[,"HASxLast"] %in% c("Symptoms but no HA events", "No Reported HA Symptoms"))
        netData[k1,"HASxLast"] <- "HA Events"
        netData[k2,"HASxLast"] <- "No HA Events"
        netData[setdiff(1:nrow(netData), c(k1, k2)),"HASxLast"] <- "NA"
      }

      # Subsume Rx, if requested
      # Note the creation or use of a local copy of netData
      if(vconn[["rxSubsume"]] & "rxID" %in% c(vxPri, cInteract[["set1"]], cInteract[["set2"]])) {
        netData[,"rxID"] <- netData[,"rxSubsID"]
        netData[,"rxName"] <- netData[,"rxSubsName"]
      }

      # Extract filter for ease of script interpretation
      vfilter <- graphCfg[[gcPtr]][["filter"]]

      # Create a vertex ID counter to be incremented and applied for each new vertex
      nvid <- 0

      # Compose vertex data for each level of each variable specified in connection specifications
      # Three variable types are treated:  primary, joint-concept, and interaction
      # Compose a list with one element per variable type (primary, joint-concept, interaction),
      # each type element containing one element for each variable specified for connection,
      # each variable element containing one element per distinct value for the variable(s) in the
      # data, each level element containing the following elements:
      # 1. dbVar ..... vector of database variable(s) corresponding to the vertex level
      # 2. dbValue ... vector of database values corresponding to the variable(s) and level
      # 3. label ..... vector of labels, each positionally corresponding to elements of dbValue
      # 4. pid ....... unique participant IDs associated with dbValue in the respective variable
      # 6. vid ....... vertex ID, used to identify vertices for edge construction

      # Compose vertex data for primary, joint-concept, and interaction variables
      vdat <- lapply(c("primary", "jointConcept", "interaction"),
                function(vclass)

                  # Primary vars, including conceptID when not joined
                  if(vclass=="primary") {
                    # Construct vector of primary variables, omitting conceptID when joint-concepts are specified
                    # Joint-concept vertices are composed separately
                    if(joinConcept) {
                      vxPri0 <- setdiff(vxPri, "conceptID")
                    } else {
                      vxPri0 <- vxPri
                    }
                    if(length(vxPri0)>0) {
                      setNames(
                        lapply(vxPri0,
                          function(v) {
                            # Identify observations that satisfy filter specification
                            # The primary conceptID filter is named "conceptID"
                            # Primary filter is NULL when absent (length 0)
                            if(length(vfilter[["primary"]])>0) {
                              if(length(which(names(vfilter[["primary"]])==v))>0) {
                                k <- which(netData[,v] %in% vfilter[["primary"]][[v]])
                              } else {
                                k <- 1:nrow(netData)
                              }
                            } else {
                              k <- 1:nrow(netData)
                            }
                            # Filter for Rx leading characters, if specified
                            if(v=="rxID" & nchar(graphCfg[[gcPtr]][["rxLeadCharFilter"]])>0) {
                              a <- graphCfg[[gcPtr]][["rxLeadCharFilter"]]
                              k <- intersect(k, k[which(substring(netData[k,"rxName"], 1, nchar(a))==a)])
                            }
                            # Subset observations by variable level
                            # The result is a data frame where the "k" column is a list of observation IDs
                            lev <- aggregate(k, by=list(netData[k,v]), function(i) i)
                            names(lev) <- c("lev", "k")
                            # Iterate through each level and assemble vertex data, including vertex ID,
                            # vector of observation IDs, participant IDs, and concept order
                            setNames(lapply(1:nrow(lev),
                                       function(i) {
                                         # Assign vertex ID for visNetwork graph composition
                                         nvid <<- nvid+1
                                         list("dbVar"=v,
                                              "dbValue"=lev[i,"lev"],
                                              # Use first observation for label
                                              "label"=netData[lev[,"k"][[i]][1],vcfg[v,"labVar"]],
                                              "pid"=unique(netData[lev[,"k"][[i]],"participantID"]),
                                              "vid"=as.character(nvid),
                                              # Save concept order for conceptIDs
                                              # It is used to limit edges between concepts to those of different order
                                              "conceptOrder"=ifelse(v=="conceptID", netData[lev[,"k"][[i]][1],"conceptOrder"],0))
                                       }),
                                     lev[,"lev"])
                          }),
                        vxPri0)
                    } else {
                      list()
                    }

                  # Joint-concepts
                  } else if(vclass=="jointConcept") {
                    if(joinConcept) {
                      # Note that jointConcept==T => joint-concepts specified and concept appears in connection cfg
                      # One vertex will be generated for each combination of levels of each concept of each order
                      # Participants associated with each vertex are associated with each concept in the combination
                      # Edges will be drawn between vertices with non-null participant intersection, representing
                      # interaction-like relationships, where contrasts in the effect of A combined with B vs. A
                      # combined with C are presented
                      # Identify columns containing composite concept IDs
                      v <- paste("jt", "conceptID", conceptOrder, sep="")
                      # Filter observations using joint-concept filter, if present
                      if("jointConcept" %in% names(vfilter)) {
                        # Iterate through each joint filter and compose union of satisfying observations
                        # It is assumed that the order of concepts in both the filter and data agree (ascending conceptOrder)
                        kc <- unique(unlist(lapply(vfilter[["jointConcept"]], function(cid) which(all(netData[,v]==cid)))))
                      } else {
                        kc <- 1:nrow(netData)
                      }

                      # Filter observations using primary conceptID filters, if present
                      # This limits joint-concepts to those with at least one component concept appearing a primary filter
                      # It is assumed that there exist, at most, one coneptID element in the primary filter
                      if("conceptID" %in% names(vfilter[["primary"]]))
                        # Iterate through each concept filter value, compose union of satisfying observations, then
                        # intersect with joint-filtered observations
                        kc <- intersect(kc, unlist(lapply(vfilter[["primary"]][["conceptID"]], function(cid) which(any(netData[,v]==cid)))))
                      # Compose vertices
                      if(length(kc)>0) {
                        # Subset observations by variable level
                        # The result is a data frame where the "k" column is a list of observation IDs
                        lev <- aggregate(kc, by=lapply(v, function(j) netData[kc,j]), function(i) i)
                        names(lev) <- c(v, "k")
                        # Iterate through each level and assemble vertex data, including vector of concept IDs,
                        # vertex ID, vector of observation IDs, and vector of participant IDs
                        list("level"=
                          setNames(lapply(1:nrow(lev),
                                     function(i) {
                                       # Assign vertex ID for visNetwork graph composition
                                       nvid <<- nvid+1
                                       list("dbVar"=rep("conceptID", length(v)),
                                            "dbValue"=unlist(lev[i,v]),
                                            # Use first observation for label
                                            "label"=netData[lev[["k"]][[i]][1],vcfg["conceptID","labVar"]],
                                            "pid"=unique(netData[lev[,"k"][[i]],"participantID"]),
                                            "vid"=as.character(nvid))
                                     }),
                                   rep("jointConceptLevel", nrow(lev)))
                        )
                      } else {
                        list()
                      }
                    } else {
                      list()
                    }

                  # Interactions
                  } else if(vclass=="interaction") {
                    # Require at least two interacting vars and one connecting var for each set
                    cset <- which(c(length(cInteract[["set1"]])>1 & length(cInteract[["conn1"]])>0,
                                    length(cInteract[["set2"]])>1 & length(cInteract[["conn2"]])>0))
                    if(length(cset>0)) {
                      # Identify netData conceptID columns when joint-concepts specified
                      if(joinConcept) {
                        vc <- paste("jt", "conceptID", conceptOrder, sep="")
                      } else {
                        vc <- "conceptID"
                      }
                      # Apply interaction filters, if specified
                      # Note that this limits interactions strictly to those satisfying filter values (each level
                      # of each interaction filter variable must be satisfied) regardless of which variabes appear
                      # in interaction sets
                      if(length(vfilter[["interaction"]])>0) {
                        k <- unique(
                               unlist(lapply(1:length(vfilter[["interaction"]]),
                                        function(i) {
                                          # Interaction filters are single row data frames with netData vars as column names
                                          vfilt <- vfilter[["interaction"]][[i]]
                                          v <- colnames(vfilt)
                                          if(!joinConcept | !"conceptID" %in% v) {
                                            # Non-joint-concept
                                            # Identify observations with equality in all filter variables
                                            which(apply(as.matrix(1:nrow(netData)), 1,
                                                        function(j) all(netData[j,v]==vfilt[,v])))
                                          } else {
                                            # Joint-concept and conceptID in filter
                                            # Identify observations with equality in non-concept vars and
                                            # all specified filter concept values in joint-concept vars
                                            # It is assumed that concept order is identical in both the filter and netData 
                                            vnc <- setdiff(v, "conceptID")
                                            vcfilt <- setdiff(v, vnc)
                                            which(apply(as.matrix(1:nrow(netData)), 1,
                                                        function(j)
                                                          all(netData[j,vnc]==vfilt[,vnc]) &
                                                          all(netData[j,vc]==vfilt[,vcfilt])))
                                          }})))
                      } else {
                        k <- 1:nrow(netData)
                      }
                      # Iterate through interaction sets
                      # Create vertices for each combination of interaction variable level
                      setNames(
                        lapply(cset,
                          function(s) {
                            v <- cInteract[[paste("set", s, sep="")]]
                            # Filter observations using primary filters, if specified
                            # Retain observations with any interaction variable containing a corresponding filter value
                            for(v2 in v) {
                              vf <- vfilter[["primary"]][[v2]]
                              if(length(vf)>0)
                                if(v2!="conceptID") {
                                  k <- intersect(k, which(netData[,v2] %in% vf))
                                } else {
                                  k <- unlist(lapply(k, function(i) if(any(netData[i,vc] %in% vf)) {i} else {NULL}))
                                }
                            }
                            # Substitute joint-concept columns when concept in interaction and concepts are joined
                            if("conceptID" %in% v)
                              v <- c(setdiff(v, "conceptID"), vc)
                            # Subset observations by unique combinations of variable levels (values)
                            # The result is a data frame where the "k" column is a list of observation IDs
                            lev <- aggregate(k, by=lapply(v, function(w) netData[k,w]), function(i) i)
                            names(lev) <- c(v, "k")
                            # Iterate through each level and assemble vertex data, including vertex ID,
                            # vector of observation IDs, and participant IDs
                            setNames(lapply(1:nrow(lev),
                                       function(i) {
                                         # Assign vertex ID for visNetwork graph composition
                                         nvid <<- nvid+1
                                         # Replace joint-concept variables with "conceptID" for labeling
                                         v2 <- unlist(lapply(v, function(w) if(w %in% vc) {"conceptID"} else {w}))
                                         list("dbVar"=v2,
                                              "dbValue"=lev[i,v],
                                              # Assemble label using label variables corresponding to db values of first obs in subset
                                              "label"=paste(netData[lev[,"k"][[i]][1],vcfg[v2,"labVar"]], collapse=" -X- ", sep=""),
                                              "pid"=unique(netData[lev[,"k"][[i]],"participantID"]),
                                              "vid"=as.character(nvid))
                                       }),
                                     rep("level", nrow(lev)))
                          }),
                        paste("set", cset, sep="")
                      )
                    } else {
                      list()
                    }
                  } else {
                    list()
                  }
              )
      names(vdat) <- c("primary", "jointConcept", "interaction")
      #print(str(vdat))

      if(F) {
        print("VDAT-VDAT")
        print(str(vdat))
        print("VDAT-VDAT")
      }

      # Configure a function to create edge data for pairs of vertices
      # v1 and v2 are lists containing one element for each level of variables corresponding to vertex 1 and vertex 2
      # Each vertex ID (vid) of v1 is combined with each vertex ID of v2
      # Vertex ID pairs for edges
      edger <- function(v1, v2, diffOrder) {
                 # Note that the length of NULL list elements is 0
                 if(length(v1)>0 & length(v2)>0) {
                   do.call(rbind,
                           apply(as.matrix(1:length(v1)), 1,
                                 function(i1)
                                   do.call(rbind,
                                           apply(as.matrix(1:length(v2)), 1,
                                             function(i2) {
                                               # Limit edges between concepts of different order, if requested
                                               if(diffOrder) {
                                                 go <- v1[[i1]][["conceptOrder"]]!=v2[[i2]][["conceptOrder"]]
                                               } else {
                                                 go <- T
                                               }
                                               if(go) {
                                                 data.frame("vid1"=v1[[i1]][["vid"]],
                                                            "vid2"=v2[[i2]][["vid"]],
                                                            "nParticipant"=length(intersect(v1[[i1]][["pid"]], v2[[i2]][["pid"]])))
                                               } else {
                                                 data.frame()
                                               }
                                             }))))
                 } else {
                   data.frame()
                 }
               }

      # Compose edge data
      # Edges connect pairs of vertices as specified in the cPri matrix and interaction vectors
      # Iterate through each specified connection and create associated edges for referenced vertices
      # Edges are defined by pairs of vertex IDs (vid)
      edat <- rbind(
                # Primary variables and joint-concepts
                # Iterate through each connection pair
                if(nrow(cPri)>0) {
                  do.call(rbind,
                          apply(as.matrix(cPri), 1,
                                function(v) {
                                  # Identify concept and non-concepts in specified vars
                                  # Note that v is a length-two vwctor
                                  k1 <- which(v=="conceptID")
                                  k2 <- setdiff(1:2, k1)
                                  if(length(k1)==0) {
                                    # Neither var is a concept
                                    # Construct edges between vertices of each
                                    edger(vdat[["primary"]][[v[1]]], vdat[["primary"]][[v[2]]], diffOrder=F)
                                  } else if(length(k1)==1) {
                                    # One var in pair is a concept
                                    # Construct edges between vertices of primary or joint concepts and those of second var
                                    if(!joinConcept) {
                                      edger(vdat[["primary"]][["conceptID"]], vdat[["primary"]][[v[k2]]], diffOrder=F)
                                    } else {
                                      edger(vdat[["jointConcept"]][["level"]], vdat[["primary"]][[v[k2]]], diffOrder=F)
                                    }
                                  } else {
                                    # Both vars are concepts
                                    # Construct edges between vertices of each
                                    if(!joinConcept) {
                                      # Limit edges to concepts of different order
                                      edger(vdat[["primary"]][["conceptID"]], vdat[["primary"]][["conceptID"]], diffOrder=T)
                                    } else {
                                      edger(vdat[["jointConcept"]][["level"]], vdat[["jointConcept"]][["level"]], diffOrder=F)
                                    }
                                  }
                                }
                          )
                  )
                } else {
                  data.frame()
                },
                # Interactions
                # Iterate through both sets
                do.call(rbind,
                        apply(as.matrix(1:2), 1,
                              function(i) {
                                set <- paste("set", i, sep="")
                                conn <- paste("conn", i, sep="")
                                if(length(cInteract[[set]])>0 & length(cInteract[[conn]])>0) {
                                  # Iterate through all variables in the connection specification for the current set
                                  do.call(rbind,
                                          apply(as.matrix(cInteract[[conn]]), 1,
                                                function(vc) {
                                                  if(!joinConcept | vc!="conceptID") {
                                                    edger(vdat[["interaction"]][[set]], vdat[["primary"]][[vc]], diffOrder=F)
                                                  } else {
                                                    edger(vdat[["interaction"]][[set]], vdat[["jointConcept"]][["level"]], diffOrder=F)
                                                  }
                                                }))
                                } else {
                                  data.frame()
                                }
                              }
                        )
                )
              )

      if(T) {
        print("EDAT-EDAT")
        print(str(edat))
        print("EDAT-EDAT")
      }

      # Compose visNetwork vertex data frame
      # Iterate through all non-empty vertex types
      k <- which(unlist(lapply(1:length(vdat), function(i) length(vdat[[i]])))>0)
      vPri <- c(unlist(cPri), cInteract[["conn1"]], cInteract[["conn2"]])
      vertex <- do.call(rbind,
                  unlist(lapply(names(vdat)[k],
                           function(vtype)
                             unlist(
                               lapply(1:length(vdat[[vtype]]),
                                 function(i) {
                                   # Configure vertex features by variable type
                                   if(vtype=="primary") {
                                     vColor <- vcfg[names(vdat[[vtype]])[i],"vColor"]
                                     tColor <- vcfg[names(vdat[[vtype]])[i],"tColor"]
                                     tSize <- vcfg[names(vdat[[vtype]])[i],"tSize"] * graphCfg[[gcPtr]][["vfontsz"]]
                                     labLegend <- vcfg[names(vdat[[vtype]])[i],"labLegend"]
                                   } else if(vtype=="jointConcept") {
                                     vColor <- vcfg["conceptID","vColor"]
                                     tColor <- vcfg["conceptID","tColor"]
                                     tSize <- vcfg["conceptID","tSize"] * graphCfg[[gcPtr]][["vfontsz"]]
                                     labLegend <- vcfg["conceptID","labLegend"]
                                   } else if(vtype=="interaction") {
                                     # Use mean size and color values for interacting variables
                                     # Use variables from first element
                                     v <- vdat[[vtype]][[i]][[1]][["dbVar"]]
                                     # Average red, green, blue color components of vars in set
                                     vColor <- rgb(matrix(apply(col2rgb(vcfg[v,"vColor"]), 1, mean)/255, nrow=1))
                                     tColor <- rgb(matrix(apply(col2rgb(vcfg[v,"tColor"]), 1, mean)/255, nrow=1))
                                     tSize <- mean(vcfg[v,"tSize"]) * graphCfg[[gcPtr]][["vfontsz"]]
                                     labLegend <- paste(vcfg[v,"labLegend"], collapse=" -X- ", sep="")
                                   } else {
                                     vColor <- "black"
                                     tColor <- "black"
                                     tSize <- 6
                                     labLegend <- "na"
                                   }
                                   # Iterate through all vertices in the current type
                                   lapply(1:length(vdat[[vtype]][[i]]),
                                     function(j) {
                                       # Primary variable vertex data may have been generated for interaction filters
                                       # Render primary vertices only if specified in a primary connection or in the
                                       # connection vector of an interaction
                                       if(vtype=="primary") {
                                         vCreate <- (vdat[[vtype]][[i]][[j]][["dbVar"]] %in% vPri)
                                       } else {
                                         vCreate <- T
                                       }
                                       if(vCreate) {
                                         n <- length(unique(vdat[[vtype]][[i]][[j]][["pid"]]))
                                         data.frame(
                                           "id"=vdat[[vtype]][[i]][[j]][["vid"]],
                                           "fixed"=F,
                                           # Omit shape, since it disables node size scaling
                                           # Actually, circles always place text inside of node
                                           # Consult docs for more shape options and their effects
                                           #"shape"="circle",
                                           "label"=vdat[[vtype]][[i]][[j]][["label"]],
                                           "color"=vColor,
                                           "font"=list("color"=tColor,
                                                       "size"=tSize,
                                                       "vadjust"=-10,
                                                       "align"="left",
                                                       "strokeWidth"=1,
                                                       "strokeColor"=ifelse(tColor<"#CCCCCC", fsc1, "#404040")),
                                           "value"=n,
                                           "mass"=graphCfg[[gcPtr]][["vmassf"]]*(5+n),
                                           # Hover Text
                                           #"title"=paste(vdat[,"lab"], "; n=", vdat[kv,"n"], sep=""),
                                           "title"=paste("n=", n, sep=""),
                                           # Identify the variable class (primary, jointConcept, interaction) that a vertex belongs to
                                           # It is used to identify the type of filter passed when selecting a node
                                           "varClass"=vtype,
                                           # Assign a group
                                           # visNetwork uses this to relate nodes to legend entries (by color)
                                           "group"=labLegend,
                                           # Concept order is needed for filtering and expansion operations
                                           "conceptOrder"=ifelse(vtype=="primary" & names(vdat[[vtype]])[i]=="conceptID",
                                                                 vdat[[vtype]][[i]][[j]][["conceptOrder"]], 0))
                                       } else {
                                         data.frame()
                                       }
                                     })
                                 }),
                               recursive=F)
                         ),
                         recursive=F)
                )

      if(F) {
        print("VERTEX-VERTEX")
        print(str(vertex))
        print("VERTEX-VERTEX")
      }    

      # Compose vertex data set list
      # The ith element contains
      #   For primary vertices .... a vector of length one containing the data value corresponding to the ith row of the vertex
      #                             data frame (vector element name is that of the corresponding netData variable)
      #   For joint-concepts ...... a vector of conceptIDs corresponding to the ith row of the vertex data frame (elements named "conceptID")
      #   For interactions ........ a single row data frame with one coumn for each interaction variable (col names correspond to vars)
      # List elements are named with vertex IDs corresponding to those in the vertex data
      vid <- vector("character")
      vertexDataValue <- unlist(
                           lapply(1:length(vdat),
                                  function(i)
                                    if(length(vdat[[i]])>0) {
                                      vtype <- names(vdat)[i]
                                      # Iterate through each level of each vertex variable
                                      lapply(unlist(vdat[[i]], recursive=F),
                                             function(v) {
                                               # Save vertex ID for vertex data list element names
                                               vid <<- c(vid, v[["vid"]])
                                               setNames(v[["dbValue"]], v[["dbVar"]])
                                             })
                                    } else {
                                      NULL
                                    }),
                         recursive=F)
      # Omit NULL elements
      k <- na.omit(unlist(lapply(1:length(vertexDataValue), function(j) ifelse(!is.null(vertexDataValue[[j]]), j, NA))))
      if(length(k)>0) {
        vertexDataValue <- setNames(vertexDataValue[k], vid)
      } else {
        vertexDataValue <- list()
      }

      if(F) {
        print("VERTEX-DATA-VERTEX-DATA")
        print(str(vertexDataValue))
        print("VERTEX-DATA-VERTEX-DATA")
      }

      # Filter edges, omitting those with frequency 0 or below minimum specified frequency
      # Note that zeroes can arise when relating a variable to itself (observations may exist for
      # two distinct levels of the variable, but the participant ID sets for the levels may be disjoint)
      k <- which(edat[,"nParticipant"]>0 & edat[,"nParticipant"]>=graphCfg[[gcPtr]]["nedgemin"])

      # Compose visNetwork edge data frame
      edge <- do.call(rbind,
                lapply(k,
                  function(i) {
                    data.frame("from"=edat[i,"vid1"],
                               "to"=edat[i,"vid2"],
                               # Line weight
                               value=edat[i,"nParticipant"],
                               #"label"=paste("n = ", edat[i,"lab"], sep=""), 
                               # Hover text
                               "title"=paste("n=", edat[i,"nParticipant"], sep=""),
                               "hoverWidth"=0,
                               "selectionWidth"=0,
                               "color"=list("color"=ec1, "opacity"=graphCfg[[gcPtr]][["eopacity"]], "highlight"=ec2),
                               # Font size scaled to node observation frequency seems like a good idea, but introduces distracting variation
                               #"font"=list("color"="white", "size"=vfontsz[1]*10*vertex0[match(vid[edat[,"v1"]],
                               #       vertex[,"id"]),"n"]/max(vertex0[,"n"]), strokeWidth=1, "strokeColor"=fsc1),
                               "font"=list("color"="white",
                                           "size"=mean(vertex[which(vertex[,"id"] %in% edat[i,c("vid1", "vid2")]),"font.size"]),
                                           "strokeWidth"=1, "strokeColor"=fsc1),
                               #"length"=20,
                               "physics"=T,
                               "smooth"=T)
                  }))

      netComponents <- list("vertex"=vertex, "vertexDataValue"=vertexDataValue, "edge"=edge)

    } else {
      netComponents <- list("vertex"=data.frame(), "vertexDataValue"=list(), "edge"=data.frame())
    }

  } else {
    netComponents <- list("vertex"=data.frame(), "vertexDataValue"=list(), "edge"=data.frame())
  }

  return(netComponents)

}

##########################################################################################################
# Function to compose renderable graph using visNetwork() functions
##########################################################################################################

composeNetwork <- function(netComponents, renderGeometry, renderScaleX, renderScaleY) {

  # Parameters:
  # netComponents .... List with two elements:
  #                    "vertex" ..... data frame of visNetwork formatted vertices
  #                    "edge" ....... data frame of visNetwork formatted edges
  # renderGeometry ... Vertex coordinate assignment:  free, columnar, or radial
  # renderScaleX ..... Fixed coordinate x scale
  # renderScaleY ..... Fixed coordinate y scale
 
  # Assign fixed vertex coordinates
  nr <- nrow(netComponents[["vertex"]])
  if(nr>0)
    if(renderGeometry=="free") {
      netComponents[["vertex"]][,"fixed"] <- F
    } else if(renderGeometry=="columnar") {
      # Assign x-axis position by vertex variable
      # Group row indices by variable menu label
      x <- split(1:nr, netComponents[["vertex"]][,"group"])
      # Aggregate row counts and compute order by variable
      # They will be used to offset y positions for vertical centering
      nx <- data.frame("n"=unlist(lapply(x, length)))
      nx[order(nx),"order"] <- ifelse(nrow(nx)>2, nrow(nx):1, rep(1, nrow(nx)))
      nxmax <- max(nx[,"n"])
      # Compute y ordinate adjustment for each group (var class) 
      xy <- do.call(rbind, lapply(1:length(x),
                             function(i) {
                               # Compute y ordinate from position in ordered vector
                               y <- vector("integer", length(x[[i]]))
                               y[order(netComponents[["vertex"]][x[[i]],"label"])] <- 1:length(x[[i]]) +
                               # Add offset to vertically center group with respect to maximum sized group
                               (nxmax-nx[i,"n"])/2 +
                               # Nudge y adjustment by group to prevent edge overlap when number of nodes equal
                               # Nudge amount is 1/ngroups, increasing with decreasing n
                               (nx[i,"order"]-1)/length(x)
                               data.frame("k"=x[[i]],
                                          "x"=i*renderScaleX*100,
                                          "y"=y*renderScaleY*100)
                             }))
      # Assign new coordinates and render
      netComponents[["vertex"]][,c("x", "y")] <- xy[order(xy[,"k"]),c("x", "y")] 
      netComponents[["vertex"]][,"fixed"] <- T
    } else if(renderGeometry=="radial") {
      # Place nodes equi-angled on circumference of a unit circle then scale using specified x and y scale factors
      thd <- -8*atan(1)/nr
      # Assign angular positions in order of var classes and labels
      k <- order(netComponents[["vertex"]][,"varClass"], netComponents[["vertex"]][,"label"])
      # Assign x-y coordinates in counter-clock-wise units of thd
      netComponents[["vertex"]][k,"x"] <- cos((1:nr)*thd)*renderScaleX*100
      netComponents[["vertex"]][k,"y"] <- sin((1:nr)*thd)*renderScaleY*100
      netComponents[["vertex"]][,"fixed"] <- T
      #netComponents[["vertex"]][,"label"] <- HTML("<i>abcdefg</i>")
    }

  # Compose legend text and color vectors
  # Color parameters affect legend appearance and should agree with groups specified in netComponents
  legendText <- unique(netComponents[["vertex"]][,"group"])
  legendColor <- unique(netComponents[["vertex"]][,"color"])

  # Create graph
  g <- visNetwork(netComponents[["vertex"]], netComponents[["edge"]])

  # Append legend text and colors
  for(i in 1:length(legendText))
    g <- g %>% visGroups(groupname=legendText[i], color=legendColor[i],
                 font=list("color"=ifelse(mean(col2rgb(legendColor[i]))<175, "white", "#202020"), "size"=12))

  # Apply visualization features
  g <- g %>%
    visLayout(randomSeed=1, hierarchical=F) %>%
    visLegend(useGroups=T, position="right", width=0.1, zoom=F) %>%
    visOptions(highlightNearest=list("enabled"=T, degree=graphCfg[[gcPtr]][["nearestHighlightDeg"]], "hover"=T),
               # Include node selection controls
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

