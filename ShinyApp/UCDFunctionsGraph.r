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

    print("assemble.graphCfg")
    #print("GGGGGGGGGGGGGGGGGGGGGGGGGGGGGG")
    #print(graphCfg[[gcPtr]][["connect"]])
    #print(graphCfg[[gcPtr]][["filter"]])
    #print(graphCfg[[gcPtr]][["rxLeadCharFilter"]])
    #print("HHHHHHHHHHHHHHHHHHHHHHHHHHHHHH")

    # Copy graph cfg values (for ease of script interpretation)
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

    # Copy interaction sets
    cInteract <- graphCfg[[gcPtr]][["interact"]]

    # Proceed if any connections specified
    if(nrow(cPri)>0 | length(cInteract[["set1"]])>0 & length(cInteract[["conn1"]])>0 |
                      length(cInteract[["set2"]])>0 & length(cInteract[["conn2"]])>0) {

      # Eliminate synonymous connection pairs (1->2 requested with 2->1)
      # Arrange all pairs alphabetically
      if(nrow(cPri)>0) {
        k <- which(cPri[,"v1"]>cPri[,"v2"])
        x <- cPri[k,"v1"]
        cPri[k,"v1"] <- cPri[k,"v2"]
        cPri[k,"v2"] <- x
        # Eliminate duplicate pairs
        # Note that pairs with ID1=ID2 are retained so that inter-variable connections are
        # possible (to relate SNOMED concepts from different groups (order), for instance)
        cPri <- cPri[which(!duplicated(cPri)),]
        # Compose vector of primary variables for vertex creation
        # Concepts are excluded when being joined
        # Interaction vertices combine levels of primary variables and are created separately
        vxPri <- unique(c(cPri[,"v1"], cPri[,"v2"], unlist(cInteract)))
      } else {
        vxPri <- unique(unlist(cInteract))
      }

      # Configure join-concept flag (joint-concepts requested, multiple concepts specified, and
      # concept appears in a connection specification)
      # Omit concepts from individual variable vector when joining concepts
      # Vertex data for joint-concepts are created separately
      if(vconn[["joinConcept"]]==T & "conceptID" %in% vxPri & length(conceptOrder)>1) {
        vxPri <- setdiff(vxPri, "conceptID")
        joinConcept <- T
      } else {
        joinConcept <- F
      }

      # Collapse HA, if requested and HA appears in any connection
      # Note that a local copy of netData is made
      if(vconn[["groupHA"]] & "HASxLast" %in% c(vxPri, unlist(cInteract))) {
        k1 <- which(netData[,"HASxLast"]=="HA Events with or without Symptoms")
        k2 <- which(netData[,"HASxLast"] %in% c("Symptoms but no HA events", "No Reported HA Symptoms"))
        netData[k1,"HASxLast"] <- "HA Events"
        netData[k2,"HASxLast"] <- "No HA Events"
        netData[setdiff(1:nrow(netData), c(k1, k2)),"HASxLast"] <- "NA"
      }

      # Subsume Rx, if requested
      # Note the creation or use of a local copy of netData
      if(vconn[["rxSubsume"]] & "RxID" %in% c(vxPri, unlist(cInteract))) {
        netData[,"RxID"] <- netData[,"rxSubsID"]
        netData[,"RxName"] <- netData[,"rxSubsName"]
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
      # 4. obsID ..... observation IDs containing dbValue in the respective variable
      # 5. pid ....... unique participant IDs associated with dbValue in the respective variable
      # Note that obsID is needed to assemble interactions, later
      # Although pid could be assembled as needed using obsID, saving it here avoids redundant
      # observation scans later  

      # Compose vertex data for primary variables
      # Iterate through all primary variables
      if(length(vxPri)>0) {
        # Primary filter is NULL if absent (length 0)
        primaryFilter <- vfilter[["primary"]]
        vdat <- list("primary"=setNames(
                                 lapply(vxPri,
                                   function(v) {
                                     # Identify observations that satisfy filter specification
                                     # Note that the primary conceptID filter is named "conceptID"
                                     # The joint-concept filter is named "jointConcept" which should not
                                     # appear in vxPri
                                     if(length(primaryFilter)>0) {
                                       if(length(which(names(primaryFilter)==v))>0) {
                                         k <- which(netData[,v] %in% primaryFilter[[v]])
                                       } else {
                                         k <- 1:nrow(netData)
                                       }
                                     } else {
                                       k <- 1:nrow(netData)
                                     }
                                     # Subset observations by variable level
                                     # lev is a list of vectors, one for each level
                                     lev <- split(k, netData[k,v])
                                     # Iterate through each level and assemble vector of participant IDs
                                     setNames(lapply(1:length(lev),
                                                function(i) {
                                                  # Use data and label values from split values
                                                  # Assign vertex IDs (vid) for visNetwork graph composition
                                                  # Save order for concepts
                                                  nvid <<- nvid+1
                                                  list("dbVar"=v,
                                                       "dbValue"=names(lev)[i],
                                                       "label"=netData[lev[[i]][1],vcfg[v,"labVar"]],
                                                       "obsID"=lev[[i]],
                                                       "pid"=unique(netData[lev[[i]],"participantID"]),
                                                       "vid"=as.character(nvid),
                                                       "conceptOrder"=ifelse(v=="conceptID", netData[lev[[i]][1],"conceptOrder"],0))
                                                }),
                                              names(lev))
                                   }),
                                 vxPri))
      } else {
        vdat <- list("primary"=list())
      }

      # Compose vertex data for joint-concepts (if joint-concepts specified and concept appears in
      # primary or interaction connections)
      # One vertex will be generated for each combination of levels of each concept of each concept order
      # Concept order is supplied in the queried data, each user-specified concept having a distinct order
      # Here, order is used to group concepts
      # Given k groups then for each combination of k concepts to be joined, observations are chosen such that
      # each participant appearing on an observation with any one of the k concepts also appears on other
      # observations with the remaining k-1 concepts
      # The result is that all participants associated with a combination are associated with each concept in
      # the combination
      # This is somewhat like an interaction, in that contrasts in the effect of A when combined with B vs.
      # when A is combined with C are measurable (visualized)
      if(joinConcept) {
        # Copy concept filter, if present, for ease of script interpretation
        if("conceptID" %in% names(vfilter[["primary"]])) {
          priConceptFilter <- vfilter[["primary"]][["conceptID"]]
        } else {
          priConceptFilter <- vector("character")
        }
        # Compose participant ID list by joint concept level
        # If joint-concept filters are specified then generate vertex data for each, limiting participants
        # to those associated with each concept specified in the filter (one per concept order)
        # If no joint-concept filter is specified then:
        # 1. Limit observations to those specified in the primary concept filter, if specified
        # 2. Generate vertex data using all combinations of concepts of order i, order j, order k, ... 
        if("jointConcept" %in% names(vfilter)) {
          # Joint concept filter specified
          # Iterate through all joint combination filters
          vdat[["jointConcept"]][["level"]] <-
            setNames(
              lapply(1:length(vfilter[["jointConcept"]]),
                function(i) {
                  # SNOMED concepts appear in the subset of a single parent concept, so that concept
                  # order segregates concepts into parent groups
                  # It is assumed that the concept appearing in position j of the filter corresponds
                  # to conceptOrder[j]
                  # Identify observations containing concept specified in first position of joint
                  # filter and also present in primary filter (if one specified)
                  kc <- which(netData[,"conceptID"]==vfilter[["jointConcept"]][[i]][1] &
                              (length(priConceptFilter)==0 | netData[,"conceptID"] %in% priConceptFilter))
                  # Iterate through each concept in the current joint filter and limit indices to
                  # those with participant IDs associated with each successive concept in the joint
                  # filter (and also present in primary concept filter, if specified)
                  for(j in 2:length(vfilter[["jointConcept"]][[i]]))
                    kc <- which(netData[,"conceptID"]==vfilter[["jointConcept"]][[i]][j] &
                                (length(priConceptFilter)==0 | netData[,"conceptID"] %in% priConceptFilter) &
                                netData[,"participantID"] %in% netData[kc,"participantID"])
                  nvid <<- nvid+1
                  list("dbValue"=vfilter[["jointConcept"]][[i]],
                       # Compose label from first observations corresponding to dbValues
                       # match() returns the first observation for each concept in the joint group,
                       # The label variable for concepts is then supplied
                       "label"=paste(netData[match(vfilter[["jointConcept"]][[i]], netData[,"conceptID"]),vcfg["conceptID","labVar"]],
                                     collapse=" -X- ", sep=""),
                       "obsID"=kc,
                       "pid"=unique(netData[kc,"participantID"]),
                       "vid"=as.character(nvid))
                }),
              rep("jointConcetpLevel", length(vfilter[["jointConcept"]])))
        } else {
          # Compose combinations of concepts, with one from each order
          # Identify, for each combination, participants associated with each concept
          # Limit to concepts in primary concept filter, if one exists 
          if(length(priConceptFilter)>0) {
            kpc <- which(netData[,"conceptID"] %in% priConceptFilter)
          } else {
            kpc <- 1:nrow(netData)
          }
          # Compose combinations in stepwise concept order, beginning with concepts of order
          # conceptOrder[1] then appending columns for concepts of order conceptOrder[2], etc.
          # The result is a data frame with n-order columns, one for each group (order) of concepts
          # Each combination of concepts of order n are represented
          cset <- data.frame()
          for(j in 1:length(conceptOrder)) {
            # Combine existing combinations (previous orders) with all concepts of current order
            # Establish data frame with first order with elements
            # Expand with orders that have elements only
            x <- unique(netData[kpc[which(netData[kpc,"conceptOrder"]==conceptOrder[j])],"conceptID"])
            if(length(x)>0)
              if(nrow(cset)>0) {
                # Append the expanded (second) column returned by expand.grid()
                cset <- data.frame(cset, expand.grid(1:nrow(cset), x, stringsAsFactors=F)[,2])
              } else {
                cset <- data.frame(x)
            }
          }
          # Assign cset col names
          if(ncol(cset)>0)
            colnames(cset) <- paste("c", 1:ncol(cset), sep="")
          # Iterate through all concept sets, composing index vectors for observations
          # with participants associated with each concept in each set
          # Concept order and position in concept set are not used when searching for concepts, since each
          # concept is associated with a single order
          # Note that concept sets have been filtered to include only those satisfying the
          # primary concept filter, if specified
          if(nrow(cset)>0) {
            # Save to a temporary list so that NULL elements can be removed
            # Proceed only if multiple concept orders remain after filtering
            vtx <- lapply(1:nrow(cset),
                     function(i) {
                       # Identify observations with concepts in current set
                       kc <- which(netData[,"conceptID"] %in% cset[i,])
                       # Enumerate distinct concepts by participant
                       pid <- aggregate(kc, by=list(netData[kc,"participantID"]),
                                        function(m) length(unique(netData[m,"conceptID"])))
                       # Identify participants associated with each concept (n distinct concepts = number in set)
                       kp <- which(pid[,2]==ncol(cset))
                       if(length(kp)>0) {
                         nvid <<- nvid+1
                         list("dbValue"=as.character(cset[i,]),
                              # Compose label from first observations corresponding to dbValues
                              "label"=paste(netData[match(cset[i,], netData[,"conceptID"]),vcfg["conceptID","labVar"]],
                                            collapse=" -X- ", sep=""),
                              # Save observation IDs for participant/concept combinations where a participant is
                              # associated with each concept in the set
                              "obsID"=kc[which(netData[kc,"participantID"] %in% pid[kp,1])],
                              "pid"=pid[kp,1],
                              "vid"=as.character(nvid))
                       } else {
                         NULL
                       }
                     })
          } else {
            vtx <- list(NULL)
          }
          # Omit NULL elements
          k <- na.omit(unlist(lapply(1:length(vtx), function(j) ifelse(!is.null(vtx[[j]]), j, NA))))
          if(length(k)>0) {
            vdat[["jointConcept"]][["level"]] <- setNames(vtx[k], rep("jointConceptLevel", length(k)))
          } else {
            vdat[["jointConcept"]][["level"]] <- list()
          }
        }
      } else {
        vdat[["jointConcept"]] <- list()
      }

      # Compose vertex data for interactions
      if(length(cInteract[["set1"]])>0 & length(cInteract[["conn1"]])>0 |
         length(cInteract[["set2"]])>0 & length(cInteract[["conn2"]])>0) {
        # Copy concept filter, if present, for ease of script interpretation
        if("interaction" %in% names(vfilter)) {
          interactionFilter <- vfilter[["interaction"]]
        } else {
          interactionFilter <- vector("character")
        }
        # Iterate through both user-specified interaction sets, consisting of interacting and
        # connection variables
        # Note that no check or adjustment is made for duplicate interaction specifications,
        # where both interaction sets specify identical sets of variables
        # Save to a temporary list so that NULL elements can be removed
        vtx <- lapply(1:2,
                 function(iset) {
                   # Compose vector of variables in interaction
                   vset <- cInteract[[paste("set", iset, sep="")]]
                   # Proceed if at least two variables being interacted and at least one being connected
                   if(length(vset)>0 & length(cInteract[[paste("conn", iset, sep="")]])>0) {
                     # Assemble joint levels from combinations of each level of interacting variables
                     if(!joinConcept | !"conceptID" %in% vset) {
                       # Concepts absent or treated as a primary variable
                       # Assemble joint interaction levels from previously assembled primary vertices,
                       # since they contain indices to observations with filtered values
                       # Iterate through each variable in the interaction set
                       k <- 1:nrow(netData)
                       for(v in vset)
                         k <- intersect(k, unlist(lapply(1:length(vdat[["primary"]][[v]]),
                                                         function(j) vdat[["primary"]][[v]][[j]][["obsID"]])))
                       # Apply filters, if specified
                       if(length(k)>0 & length(interactionFilter)>0)
                         k <- unlist(lapply(1:length(interactionFilter),
                                       function(j)
                                         # Filters are database values in vectors with names
                                         # corresponding to interacting database variables
                                         # Test filter only if variables in filter specification
                                         # correspond to those in current vset (filters from
                                         # the other set are ignored)
                                         # Note that R will attempt to recycle the shorter of two
                                         # vectors being compared for equality, so use union-intersect
                                         # Also, if() evaluates scalars, selecting the first element of
                                         # vectors being compared (therefore, use all))
                                         if(all(sort(intersect(names(interactionFilter[[j]]), vset)) ==
                                                sort(union(names(interactionFilter[[j]]), vset)))) {
                                           # Note that ==() makes comparisons in row, col order and produces
                                           # a matrix of T/F values for each cell, but which() converts
                                           # an individual scalar values for each row (the first column)
                                           # Therefore, evaluate ==() for each row, requiring each column
                                           # position to be true
                                           k[which(unlist(lapply(k, function(m) all(netData[m,vset]==interactionFilter[[j]][vset]))))]
                                         } else {
                                           # Filter is ignored
                                           NULL
                                         }))
                       if(length(k)>0) {
                         # Compose sets of observation indices by variable level combination
                         lev <- split(k, netData[k,vset], drop=T, sep=" -X- ")
                         # Compose list containing levels, observation indices, and unique participant IDs
                         vtxi <- lapply(1:length(lev),
                                        function(j) {
                                          nvid <<- nvid+1
                                          # Parse database values from split list
                                          dbValue <- strsplit(names(lev)[j], " -X- ")[[1]]
                                          # Assemble label using label variables corresponding to db values of first obs in subset
                                          lab <- paste(netData[lev[[j]][1],vcfg[vset,"labVar"]], collapse=" -X- ", sep="")
                                          list("dbVar"=vset,
                                               # Parse variable values from split() levels
                                               "dbValue"=dbValue,
                                               "label"=lab,
                                               "obsID"=lev[[j]],
                                               "pid"=unique(netData[lev[[j]],"participantID"]),
                                               "vid"=as.character(nvid))
                                        })
                       } else {
                         NULL
                       }
                       # Omit NULL elements
                       k <- na.omit(unlist(lapply(1:length(vtxi), function(j) ifelse(!is.null(vtxi[[j]]), j, NA))))
                       if(length(k)>0) {
                         setNames(vtxi[k], rep(paste(vset, collapse=" -X- ", sep=""), length(k)))
                       } else {
                         list()
                       }
                     } else if(length(vdat[["jointConcept"]][["level"]])>0) {
                       # Joint-concepts exist and are specified in interaction set
                       # Note that length() returns 0 for non-existent elements
                       # Assemble interaction vertex data for the current variable set as follows:
                       # 1. Compose subset of variables excluding conceptID (vset0)
                       # 2. Assemble set of observation IDs that represent data satisfying primary
                       #    variable filters (this excludes interaction combinations that have
                       #    one or more component variable levels that have been excluded)
                       # 3. Assemble sets of observation indices for each combination of vset0 levels
                       # 4. Combine each vset0 combination with each joint-concept combination
                       # 4a. Iterate through each vset0 level combination
                       # 4b. Filter non-concept interaction variable combinations
                       # 4c. If observations exist after non-concept filtering then identify
                       #     joint-concepts that satisfy filters (since each observation contains
                       #     a single concept, joint-concepts involve multiple observations and
                       #     filtering is applied to previously composed joint-concept vertices
                       # 4d. Join filtered non-concept and joint-concept levels
                       # 
                       # Step 1. Subset non-concept variables
                       vset0 <- setdiff(vset, "conceptID")
                       # Step 2. Assemble observation indices to satisfy primary variable filters
                       # Iterate through each variable in the interaction set
                       # Intersect success index vectors with prior indices
                       kpr <- 1:nrow(netData)
                       for(v in vset0)
                         kpr <- intersect(kpr, unlist(lapply(1:length(vdat[["primary"]][[v]]),
                                                             function(j) vdat[["primary"]][[v]][[j]][["obsID"]])))
                       if(length(kpr)>0) {
                         # Step 3. Assemble sets of observation indices by (non-concept) variable combination
                         lev0 <- split(kpr, netData[kpr,vset0], drop=T, sep=" -X- ")
                         # Step 4. Iterate through all non-concept level combinations
                         # Combine non-concept and joint concept levels
                         # vset0 and joint-concept index sets include observations with participants
                         # associated with corresponding vset0 variable levels and all concepts in
                         # in the joint-concept combination
                         # The intersection of vset0 and join-concept indices limit observations
                         # to desired variable levels, but it is possible for, say, two different
                         # vset0 levels to correspond to observations represented by a given
                         # joint-concept index
                         # Therefore, for each vset0, joint-concept index intersection, retain
                         # observations for participants who are associated with each concept in the
                         # joint combination
                         # Step 4a. Iterate through each vset0 level combination
                         vtxi <- lapply(1:length(lev0),
                                        function(ilev0) {
                                          ks0 <- lev0[[ilev0]]
                                          # Filter
                                          if(length(interactionFilter)>0) {
                                            # Step 4b. Subset vset0 level observations using applicable filters
                                            ks0 <- unlist(lapply(1:length(interactionFilter),
                                                                 function(j)
                                                                   # Filters are database values in vectors with names
                                                                   # corresponding to interacting database variables
                                                                   # Test filter only if variables in filter specification
                                                                   # correspond to those in current vset (filters from
                                                                   # the other set are ignored)
                                                                   # Note that all variable names (including conceptID)
                                                                   # are used to determine if a filter applies, but
                                                                   # only non-concept variables are actually filtered here
                                                                   if(all(sort(intersect(names(interactionFilter[[j]]), vset)) ==
                                                                          sort(union(names(interactionFilter[[j]]), vset)))) {
                                                                     # Use all() to evaluate all columns (variables) of each row
                                                                     ks0[unlist(lapply(ks0, function(m) which(all(netData[m,vset0]==interactionFilter[[j]][vset0]))))]
                                                                   } else {
                                                                     # Filter is ignored
                                                                     NULL
                                                                   }))
                                            # Step 4c. Identify joint-concepts that satisfy applicable filters
                                            # Note that, to be applicable, filters must correspond to vset0 levels
                                            # Since a node containing an interaction involving joint-concepts was
                                            # selected for filtering (that's why we have a filter), joint-concepts
                                            # with the selected level must exist
                                            if(length(ks0)>0) {
                                              njc <- length(vdat[["jointConcept"]])
                                              # Iterate through each filter
                                              kjc <- unlist(lapply(1:length(interactionFilter),
                                                                   function(j1)
                                                                     # Evaluate only if filter applies to current variable set
                                                                     if(all(sort(intersect(names(interactionFilter[[j1]]), vset)) ==
                                                                            sort(union(names(interactionFilter[[j1]]), vset)))) {
                                                                       # Iterate through each joint-concept and indicate which
                                                                       # satisfy joint-concept portion of current filter
                                                                       lapply(1:njc,
                                                                              function(j2)
                                                                                if(all(vdat[["jointConcept"]][["level"]][[j2]][["dbValue"]]==
                                                                                       interactionFilter[[j1]][which(names(interactionFilter[[j1]])=="conceptID")])) {
                                                                                  j2
                                                                                } else {
                                                                                  # Joint-concept does not satisfy current filter - ignore
                                                                                  NULL
                                                                                })
                                                                     } else {
                                                                       # Ignore filter
                                                                       NULL
                                                                     }))
                                            } else {
                                              kjc <- NULL
                                            }
                                          } else {
                                            kjc <- 1:length(vdat[["jointConcept"]][["level"]])
                                          }
                                          # Step 4d. Generate vertex data for combinations of vset0 joint-concepts that satisfy filters
                                          if(length(ks0)>0 & length(kjc)>0) {
                                            # For each vset0 and joint-concept level combination, subset observations by intersecting
                                            # indices corresponding to vset0 levels with indices corresponding to the joint-concept
                                            # Since joint-concept observation IDs indicate participants associated with all concepts
                                            # in a joint combination, each observation in the vset0, joint-combination ID intersection
                                            # contains the necessary vset0 levels and one of the joint-concepts
                                            # The number of observations per participant is at least the number of concepts in the
                                            # joint combination
                                            vtxj <- lapply(kjc, function(j) {
                                                                  k <- intersect(ks0, vdat[["jointConcept"]][["level"]][[j]][["obsID"]])
                                                                  if(length(k)>0) {
                                                                    nvid <<- nvid+1
                                                                    dbVal0 <- strsplit(names(lev0)[ilev0], " -X- ")[[1]]
                                                                    dbValConcept <- vdat[["jointConcept"]][["level"]][[j]][["dbValue"]]
                                                                    # Compose vset0 portio of label from first observation in current ilev subset
                                                                    # Compose concept portion (one labe for each concept) from initial observations
                                                                    # corresponding to each concept ID in the joint set
                                                                    # Note that match() returns the first observation index for each concept
                                                                    lab <- paste(c(netData[lev0[[ilev0]][1],vcfg[vset0,"labVar"]],
                                                                                   netData[match(dbValConcept, netData[,"conceptID"]),vcfg["conceptID","labVar"]]),
                                                                                 collapse=" -X- ", sep="")
                                                                    list(# Include "conceptID" once for each concept in vars and values
                                                                         "dbVar"=c(vset0, rep("conceptID", length(dbValConcept))),
                                                                         "dbValue"=c(dbVal0, dbValConcept),
                                                                         "label"=lab,
                                                                         "obsID"=k,
                                                                         "pid"=unique(netData[k,"participantID"]),
                                                                         "vid"=as.character(nvid))
                                                                  } else {
                                                                    NULL
                                                                  }
                                                                })
                                            # Omit NULL entries
                                            k <- na.omit(unlist(lapply(1:length(vtxj), function(j) ifelse(!is.null(vtxj[[j]]), j, NA))))
                                            if(length(k)>0) {
                                              # Compose names from variables in interaction (conceptID appearing once for each joint-concept)
                                              nm <- unlist(lapply(k, function(j) paste(vtxj[[j]][["dbVar"]], collapse=" -X- ", sep="")))
                                              setNames(vtxj[k], nm)
                                            } else {
                                              NULL
                                            }
                                          } else {
                                            NULL
                                          }
                                        })
                         # Omit NULL entries
                         k <- na.omit(unlist(lapply(1:length(vtxi), function(j) ifelse(!is.null(vtxi[[j]]), j, NA))))
                         if(length(k)>0) {
                           unlist(vtxi[k], recursive=F)
                         } else {
                           NULL
                         }
                       } else {
                         NULL
                       }
                     } else {
                       NULL
                     }
                   } else {
                     NULL
                   }
                 })
        # Omit NULL elements at top level of return list (set 1 and set 2)
        k <- na.omit(unlist(lapply(1:length(vtx), function(j) ifelse(!is.null(vtx[[j]]), j, NA))))
        if(length(k)>0) {
          vdat[["interaction"]] <- setNames(vtx[k], c("set1", "set2")[k])
        } else {
          vdat[["interaction"]] <- list()
        }
      } else {
        vdat[["interaction"]] <- list()
      }

      if(F) {
        print("VDATVDAT")
        print(str(vdat))
        print("VDATVDAT")
      }

      # Configure a function to created edge data for pairs involving the three types of vertices
      edger <- function(v1, v2, reqDiffOrder) {
                 # Note that the length of NULL list elements is 0
                 if(length(v1)>0 & length(v2)>0) {
                   do.call(rbind,
                           apply(as.matrix(1:length(v1)), 1,
                                 function(i1)
                                   do.call(rbind,
                                           apply(as.matrix(1:length(v2)), 1,
                                             function(i2) {
                                               if(reqDiffOrder) {
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
      edat <- rbind(
                # Primary variables (including concepts when not joined)
                # Iterate through each connection pair
                if(nrow(cPri)>0) {
                  do.call(rbind,
                          apply(as.matrix(cPri), 1,
                                function(v)
                                  if(!"conceptID" %in% v) {
                                    edger(vdat[["primary"]][[v[1]]], vdat[["primary"]][[v[2]]], reqDiffOrder=F)
                                  } else if(v[1]=="conceptID" & v[2]!="conceptID") {
                                    if(!joinConcept) {
                                      edger(vdat[["primary"]][["conceptID"]], vdat[["primary"]][[v[2]]], reqDiffOrder=F)
                                    } else {
                                      edger(vdat[["jointConcept"]][["level"]], vdat[["primary"]][[v[2]]], reqDiffOrder=F)
                                    }
                                  } else if(v[1]!="conceptID" & v[2]=="conceptID") {
                                    if(!joinConcept) {
                                      edger(vdat[["primary"]][[v[1]]], vdat[["primary"]][["conceptID"]], reqDiffOrder=F)
                                    } else {
                                      edger(vdat[["primary"]][[v[1]]], vdat[["jointConcept"]][["level"]], reqDiffOrder=F)
                                    }
                                  } else if(!joinConcept) {
                                    edger(vdat[["jointConcept"]][["level"]], vdat[["jointConcept"]][["level"]], reqDiffOrder=F)
                                  } else {
                                    edger(vdat[["primary"]][["conceptID"]], vdat[["primary"]][["conceptID"]], reqDiffOrder=T)
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
                                                function(vc)
                                                  if(!joinConcept | vc!="conceptID") {
                                                    edger(vdat[["interaction"]][[set]], vdat[["primary"]][[vc]], reqDiffOrder=F)
                                                  } else {
                                                    edger(vdat[["interaction"]][[set]], vdat[["jointConcept"]][["level"]], reqDiffOrder=F)
                                                  }))
                                } else {
                                  data.frame()
                                }
                              }
                        )
                )
              )

      if(F) {
        print("EDATEDAT")
        print(edat)
        print("EDATEDAT")
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
        print("VERTEXVERTEX")
        print(vertex)
        print("VERTEXVERTEX")
      }    

      # Compose vertex data list
      # The ith element contains a named vector of data values corresponding to the ith row in the vertex data frame
      # Vector names are netData columns corresponding to the data values
      # For reference, list elements are named with vertex IDs that corresponf to those in the vertex data frame
      vid <- vector("character")
      k <- which(unlist(lapply(1:length(vdat), function(i) length(vdat[[i]])))>0)
      vertexDataValue <- unlist(unlist(
                           lapply(names(vdat)[k],
                                  function(vtype)
                                    lapply(1:length(vdat[[vtype]]),
                                           function(i)
                                             lapply(1:length(vdat[[vtype]][[i]]),
                                                    function(j) {
                                                      # Primary variable vertex data may have been generated for interaction filters
                                                      # Render primary vertices only if specified in a primary connection or in the
                                                      # connection vector of an interaction
                                                      if(vtype=="primary") {
                                                        vCreate <- (vdat[[vtype]][[i]][[j]][["dbVar"]] %in% c(unlist(cPri), cInteract[["conn1"]], cInteract[["conn2"]]))
                                                      } else {
                                                        vCreate <- T
                                                      }
                                                      if(vCreate) {
                                                        vid <<- c(vid, vdat[[vtype]][[i]][[j]][["vid"]])
                                                        if(vtype %in% c("primary", "interaction")) {
                                                          # Variable name(s) in dbVar element
                                                          setNames(vdat[[vtype]][[i]][[j]][["dbValue"]], vdat[[vtype]][[i]][[j]][["dbVar"]])
                                                        } else if(vtype=="jointConcept") {
                                                          # Use "conceptID" once for each member concept
                                                          setNames(vdat[[vtype]][[i]][[j]][["dbValue"]],
                                                                   rep("conceptID", length(vdat[[vtype]][[i]][[j]][["dbValue"]])))
                                                        } else {
                                                          NULL
                                                        }
                                                      } else {
                                                        NULL
                                                      }
                                                    }))),
                           recursive=F), recursive=F)
        # Omit NULL elements
        k <- na.omit(unlist(lapply(1:length(vertexDataValue), function(j) ifelse(!is.null(vertexDataValue[[j]]), j, NA))))
        if(length(k)>0) {
          vertexDataValue <- setNames(vertexDataValue[k], vid)
        } else {
          vdat[["interaction"]] <- list()
        }

      if(F) {
        print("VERTEXDATAVERTEXDATA")
        print(vertexDataValue)
        print("VERTEXDATAVERTEXDATA")
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

