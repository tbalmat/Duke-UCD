#####################################################################################################
# Duke University UCD Shiny App, Apr 2020
# Query functions
#####################################################################################################

##########################################################################################################
# Query participant and concept UCD observations using requested parameter values
# Return list of data frames containing vertex and edge configurations
##########################################################################################################

queryAndConstructGraphPC <- function(nodeVar1, nodeVar2, conceptID, nodeVarFilter, vColor1, vColor2,
                                     nedgemin, eopacity, vMassFactor, vSizeFactor, vFontSize, elabVar,
                                     accMethod) {
 
  # Parameters:
  # nodeVar1 ............. First node variable (taken from following query) - one set of vertices are
  #                        constructed from retrieved values of this var (typically participant variable)
  # nodeVar2 ............. Second node variable - one set of nodes constructed from corresponding queried
  #                        values (typically SNOMED FSN)
  # conceptID ............ SNOMED concepts to be queried - all concepts with an ISA relationship to
  #                        supplied concept IDs are returned by the following query and nodes are
  #                        constructed for them
  # nodeVarFilter ........ Limit nodeVar1 levels to those specified in this vector 
  # vColor1 .............. Color of nodeVar1 vertices
  # vColor2 .............. Color of nodeVar2 vertices
  # nedgemin ............. Limits edges to those with computed n (number of participants) >= this value
  # eopacity ............. Edge opacity
  # vMassFactor .......... Vertex mass factor (reduction causes greater repulsion)
  # vSizeFactor .......... Vertex size factor (does not appear to affect rendered vertex size)
  # vFontSize ............ Vertex label font size
  # elabVar .............. Variable (from query results) to use in constructing edge labels
  # accMethod ............ Node frequency (n) accumulatio method

  # Retrieve node and edge data for participants connected to the specified concept
  # Note the specification of directed edges although, in the db, they are limited to participant->concept
  # Also note the retrieval of unique combinations of participant and concept to avoid bias due to duplicate observations 
  # Although various types of concept nodes (ie, labels ["ObjectConcept", "Neuro", "Psych"], ["ObjectConcept", "Psych"])
  # are related to the terminating concept identified here by z.sctid (relationship type ISA), all (observed) ISA
  # relationships from participants to concept nodes with an ISA relation to the terminating concept are of type
  # ["ObjectConcept", "Psych", "PPsych", "PSCT"]
  # All relationships from participants to concepts are of type P_SCT
  query <- paste(" match(x:Participant)-[:P_SCT]->(y:ObjectConcept)-[r:ISA*]->(z:ObjectConcept)",
                 " where z.sctid in['", paste(conceptID, collapse="', '", sep=""), "']",
                 "       and y.active='1' and z.active='1'",
                 " with",
                 "   case when(labels(x)=['Participant'])then 'UCDDist'",
                 "        when(labels(x)=['Participant','UCD_Proximal'])then 'UCDProx'",
                 "   end as UCDProxDist,",
                 "   x.ParticipantId as participantID, x.Sex as Sex, x.UCDDx as UCDDx,",
                 "   case when(toInteger(x.OnsetAgeDays)<11)then '0-11'",
                 "        when(toInteger(x.OnsetAgeDays)<101)then '11-100'",
                 "        when(toInteger(x.OnsetAgeDays)<1001)then '101-1000'",
                 "        when(toInteger(x.OnsetAgeDays)<10001)then '1001-10000'",
                 "        when(toInteger(x.OnsetAgeDays) is not null)then '>10000'",
                 "        else null",
                 "   end as onsetAgeDays,",
                 "   startNode(last(r)) as concept",
                 # Filter participant var levels
                 ifelse(length(nodeVarFilter)>0,
                        paste(" where x.", nodeVar1, " in['",
                              paste(nodeVarFilter, collapse="', '", sep=""), "']", sep=""),
                        ""), 
                 " return  UCDProxDist, participantID, Sex, UCDDx, onsetAgeDays,",
                 "         concept.sctid as conceptID, labels(concept) as conceptLabel,",
                 "         concept.FSN as FSN, count(1) as n", sep="")
  #print(query)
  pConcept <- cypher(dbcon, query)
  #print(pConcept)

  if(!is.null(pConcept)>0) {
    # Relabel null values
    pConcept[which(is.na(pConcept[,"Sex"])),"Sex"] <- "na"
    pConcept[which(is.na(pConcept[,"UCDDx"])),"UCDDx"] <- "na"
    pConcept[which(is.na(pConcept[,"onsetAgeDays"])),"onsetAgeDays"] <- "na"
    # Assemble concept and participant nodes and edges
    # Return in two element list with names "vertex" and "edge"
    return(assembleNetComponents(pConcept=pConcept,
                                 nodeVar1=nodeVar1,
                                 nodeVar2=nodeVar2,
                                 vColor1=vColor1,
                                 vColor2=vColor2,
                                 nedgemin=nedgemin,
                                 eopacity=eopacity,
                                 vmassf=vMassFactor,
                                 vsizefactor=vSizeFactor,
                                 vfontsz=c(vFontSize, 0.8*vFontSize),
                                 # Adjust font color light for dark vertex color, dark for light
                                 vfontcolor=c(ifelse(vColor1<"#CCCCCC", vColor1, "#404040"),
                                              ifelse(vColor2<"#CCCCCC", vColor2, "#404040")),
                                 elabVar=elabVar,
                                 accMethod=accMethod))
  } else {
    return(list("vertex"=data.frame(), "edge"=data.frame()))
  }

}

##########################################################################################################
# Query prescription and (participant or concept) UCD observations using requested parameter values
# Return list of data frames containing vertex and edge configurations
# Parameter subsume (T/F) enables prescriptions to be combined into parent nodes based on RxNorm relationships
##########################################################################################################

queryAndConstructGraphPrescrip <- function(nodeVar, conceptID, nodeVarFilter, prescripFilter, subsume,
                                           vColor1, vColor2, nedgemin, eopacity, vMassFactor, vSizeFactor,
                                           vFontSize, elabVar, accMethod) {

  # Retrieve node and edge data for prescriptions connected to participants connected to the specified concept
  # Note the specification of directed edges although, in the db, they are limited to participant->rx
  # Also, the participant node appears between P_RX and P_SCT because it has such relationships to RX and ObjectConcept
  # The query match(w:Participant)-[:P_RX]->(x:RXCUI)-[:P_SCT]->(y:ObjectConcept) returns null because RX has no
  # relationships directed to ObjectConcept
  # All relationships from participants to prescriptions are of type P_RX
  if(subsume) {
    query <- paste(" match(v:RXCUI)-[:SUBSUMES]->(w:RXCUI)<-[:P_RX]-(x:Participant)-[:P_SCT]->",
                                                "(y:ObjectConcept)-[r:ISA*]->(z:ObjectConcept)",
                   " where z.sctid in['", paste(conceptID, collapse="', '", sep=""), "']",
                   "       and v.status='Active' and w.status='Active' and y.active='1' and z.active='1'",
                   # Filter prescriptions, if any specified
                   ifelse(length(prescripFilter)>0,
                          paste(" and v.id in['", paste(prescripFilter, collapse="', '", sep=""), "']", sep=""),
                          ""), sep="")
  } else {
    query <- paste(" match(w:RXCUI)<-[:P_RX]-(x:Participant)-[:P_SCT]->(y:ObjectConcept)-[r:ISA*]->(z:ObjectConcept)",
                   " where z.sctid in['", paste(conceptID, collapse="', '", sep=""), "']",
                   "       and w.status='Active' and y.active='1' and z.active='1'",
                   # Filter prescriptions, if any specified
                   ifelse(length(prescripFilter)>0,
                          paste(" and w.id in['", paste(prescripFilter, collapse="', '", sep=""), "']", sep=""),
                          ""), sep="")
  }
  query <- paste(query,
                 " and toLower(w.category)='drug'",
                 " with",
                 "   case when(labels(x)=['Participant'])then 'UCDDist'",
                 "        when(labels(x)=['Participant','UCD_Proximal'])then 'UCDProx'",
                 "   end as UCDProxDist,",
                 "   x.ParticipantId as participantID, x.Sex as Sex, x.UCDDx as UCDDx,",
                 "   case when(toInteger(x.OnsetAgeDays)<11)then '0-11'",
                 "        when(toInteger(x.OnsetAgeDays)<101)then '11-100'",
                 "        when(toInteger(x.OnsetAgeDays)<1001)then '101-1000'",
                 "        when(toInteger(x.OnsetAgeDays)<10001)then '1001-10000'",
                 "        when(toInteger(x.OnsetAgeDays) is not null)then '>10000'",
                 "        else null",
                 "   end as onsetAgeDays,",
                 "   startNode(last(r)) as concept, ",
                     ifelse(subsume, "v.id as prescriptionID, v.name as Prescription,",
                                     "w.id as prescriptionID, w.name as Prescription,"),
                 "   1 as n",
                 # Filter nodeVar levels when edges to participant var nodes being generated (not concept FSN)
                 ifelse(nodeVar!="FSN" & length(nodeVarFilter)>0,
                        paste(" where ", nodeVar, " in['", paste(nodeVarFilter, collapse="', '", sep=""), "']", sep=""),
                        ""),
                 " return  distinct UCDProxDist, participantID, Sex, UCDDx, onsetAgeDays,",
                 "         concept.sctid as conceptID, concept.FSN as FSN,",
                 "         prescriptionID, Prescription, n", sep="")

  pPrescrip <- cypher(dbcon, query)
  #print(pPrescrip)

  if(!is.null(pPrescrip)>0) {
    # Relabel null values
    pPrescrip[which(is.na(pPrescrip[,"Sex"])),"Sex"] <- "na"
    pPrescrip[which(is.na(pPrescrip[,"UCDDx"])),"UCDDx"] <- "na"
    pPrescrip[which(is.na(pPrescrip[,"onsetAgeDays"])),"onsetAgeDays"] <- "na"
    # Assemble and return graph with specified nodeVar and prescription nodes and edges
    return(assembleNetComponents(pConcept=pPrescrip,
                                 nodeVar1=nodeVar,
                                 nodeVar2="Prescription",
                                 vColor1=vColor1,
                                 vColor2=vColor2,
                                 nedgemin=nedgemin,
                                 eopacity=eopacity,
                                 vmassf=vMassFactor,
                                 vsizefactor=vSizeFactor,
                                 vfontsz=c(vFontSize, 0.7*vFontSize),
                                 # Adjust font color light for dark vertex color, dark for light
                                 vfontcolor=c(ifelse(vColor1<"#CCCCCC", vColor1, "#404040"),
                                              ifelse(vColor2<"#CCCCCC", vColor2, "#404040")),
                                 elabVar=elabVar,
                                 accMethod=accMethod))
  } else {
    return(list("vertex"=data.frame(), "edge"=data.frame()))
  }

}

##########################################################################################################
# Query role group and (participant or concept) UCD observations using requested parameter values
# Return list of data frames containing vertex and edge configurations
##########################################################################################################

queryAndConstructGraphRoleGroup <- function(nodeVar, conceptID, nodeVarFilter, roleGroupFilter,
                                            vColor1, vColor2, nedgemin, eopacity, vMassFactor,
                                            vSizeFactor, vFontSize, elabVar, accMethod) {

  query <- paste(" match(x:Participant)-[:P_SCT]->(y:ObjectConcept)-[r:ISA*]->(z:ObjectConcept)",
                 " where z.sctid in['", paste(conceptID, collapse="', '", sep=""), "']",
                 "       and y.active='1' and z.active='1'",
                 " with x, y, r",
                 " match(y)-[role1:HAS_ROLE_GROUP]->(rg)-[role2:FINDING_SITE]->(rgc:ObjectConcept)",
                 " where role2.active='1' and rgc.active='1'",
                 # Filter role groups, if any specified
                 ifelse(length(roleGroupFilter)>0,
                        paste(" and rgc.sctid in['", paste(roleGroupFilter, collapse="', '", sep=""), "']", sep=""),
                        ""),
                 " with",
                 "   case when(labels(x)=['Participant'])then 'UCDDist'",
                 "        when(labels(x)=['Participant','UCD_Proximal'])then 'UCDProx'",
                 "   end as UCDProxDist,",
                 "   x.ParticipantId as participantID, x.Sex as Sex, x.UCDDx as UCDDx,",
                 "   case when(toInteger(x.OnsetAgeDays)<11)then '0-11'",
                 "        when(toInteger(x.OnsetAgeDays)<101)then '11-100'",
                 "        when(toInteger(x.OnsetAgeDays)<1001)then '101-1000'",
                 "        when(toInteger(x.OnsetAgeDays)<10001)then '1001-10000'",
                 "        when(toInteger(x.OnsetAgeDays) is not null)then '>10000'",
                 "        else null",
                 "   end as onsetAgeDays,",
                 "   startNode(last(r)) as concept,",
                 "   type(role2) as role, rgc, y",
                 # Filter nodeVar levels when edges to participant var nodes being generated (not concept FSN)
                 ifelse(nodeVar!="FSN" & length(nodeVarFilter)>0,
                        paste(" where ", nodeVar, " in['", paste(nodeVarFilter, collapse="', '", sep=""), "']", sep=""),
                        ""),
                 " return  UCDProxDist, participantID, Sex, UCDDx, onsetAgeDays,",
                 "         concept.sctid as conceptID, labels(concept) as conceptLabel,",
                 "         concept.FSN as FSN, role, rgc.sctid as roleGroupID, rgc.FSN as roleGroupFSN,",
                 "         count(1) as n", sep="")

  pRoleGroup <- cypher(dbcon, query)
  #print(pRoleGroup)

  if(!is.null(pRoleGroup)>0) {
    # Relabel null values
    pRoleGroup[which(is.na(pRoleGroup[,"Sex"])),"Sex"] <- "na"
    pRoleGroup[which(is.na(pRoleGroup[,"UCDDx"])),"UCDDx"] <- "na"
    pRoleGroup[which(is.na(pRoleGroup[,"onsetAgeDays"])),"onsetAgeDays"] <- "na"
    # Assemble and return graph with specified nodeVar and role group nodes and edges
    return(assembleNetComponents(pConcept=pRoleGroup,
                                 nodeVar1=nodeVar,
                                 nodeVar2="roleGroupFSN",
                                 vColor1=vColor1,
                                 vColor2=vColor2,
                                 nedgemin=nedgemin,
                                 eopacity=eopacity,
                                 vmassf=vMassFactor,
                                 vsizefactor=vSizeFactor,
                                 vfontsz=c(vFontSize, 0.7*vFontSize),
                                 # Adjust font color light for dark vertex color, dark for light
                                 vfontcolor=c(ifelse(vColor1<"#CCCCCC", vColor1, "#404040"),
                                              ifelse(vColor2<"#CCCCCC", vColor2, "#404040")),
                                 elabVar=elabVar,
                                 accMethod=accMethod))
  } else {
    return(list("vertex"=data.frame(), "edge"=data.frame()))
  }

}

##########################################################################################################
# Query hyperammonemia and (participant or concept) UCD observations using requested parameter values
# Return list of data frames containing vertex and edge configurations
##########################################################################################################

queryAndConstructGraphHA <- function(nodeVar, conceptID, nodeVarFilter, haFilter,
                                     vColor1, vColor2, nedgemin, eopacity, vMassFactor,
                                     vSizeFactor, vFontSize, elabVar, accMethod) {

  query <- paste(" match(x:Participant)-[:P_SCT]->(y:ObjectConcept)-[r:ISA*]->(z:ObjectConcept)",
                 " where z.sctid in['", paste(conceptID, collapse="', '", sep=""), "']",
                 "       and y.active='1' and z.active='1'",
                 " with x, y, r",
                 " match(y)-[role1:HAS_ROLE_GROUP]->(rg)-[role2:FINDING_SITE]->(rgc:ObjectConcept)",
                 " where role2.active='1' and rgc.active='1'",
                 # Filter HA levels, if any specified
                 ifelse(length(haFilter)>0,
                        paste(" and x.HASxLast in['", paste(haFilter, collapse="', '", sep=""), "']", sep=""),
                        ""),
                 " with",
                 "   case when(labels(x)=['Participant'])then 'UCDDist'",
                 "        when(labels(x)=['Participant','UCD_Proximal'])then 'UCDProx'",
                 "   end as UCDProxDist,",
                 "   x.ParticipantId as participantID, x.Sex as Sex, x.UCDDx as UCDDx,",
                 "   case when(toInteger(x.OnsetAgeDays)<11)then '0-11'",
                 "        when(toInteger(x.OnsetAgeDays)<101)then '11-100'",
                 "        when(toInteger(x.OnsetAgeDays)<1001)then '101-1000'",
                 "        when(toInteger(x.OnsetAgeDays)<10001)then '1001-10000'",
                 "        when(toInteger(x.OnsetAgeDays) is not null)then '>10000'",
                 "        else null",
                 "   end as onsetAgeDays,",
                 "   x.HASxLast as HASxLast,",
                 "   startNode(last(r)) as concept,",
                 "   type(role2) as role, rgc, y",
                 # Filter nodeVar levels
                 # nodeVar should contain either FSC (HA edges to concept) or a participant var (HA to that var)
                 ifelse(nodeVar!="FSN" & length(nodeVarFilter)>0,
                        paste(" where ", nodeVar, " in['", paste(nodeVarFilter, collapse="', '", sep=""), "']", sep=""),
                        ""),
                 " return  UCDProxDist, participantID, Sex, UCDDx, onsetAgeDays,",
                 "         concept.sctid as conceptID, labels(concept) as conceptLabel,",
                 "         concept.FSN as FSN, HASxLast, role, rgc.sctid as roleGroupID,",
                 "         rgc.FSN as roleGroupFSN, count(1) as n", sep="")

  #print(nodeVar)
  #print(nodeVarFilter)
  #print(haFilter)
  #print(query)
  pHA <- cypher(dbcon, query)
  #print(pHA)

  if(!is.null(pHA)>0) {
    # Relabel null values
    pHA[which(is.na(pHA[,"Sex"])),"Sex"] <- "na"
    pHA[which(is.na(pHA[,"UCDDx"])),"UCDDx"] <- "na"
    pHA[which(is.na(pHA[,"onsetAgeDays"])),"onsetAgeDays"] <- "na"
    pHA[which(is.na(pHA[,"HASxLast"])),"HASxLast"] <- "na"
    # Assemble and return graph with specified nodeVar and role group nodes and edges
    return(assembleNetComponents(pConcept=pHA,
                                 nodeVar1=nodeVar,
                                 nodeVar2="HASxLast",
                                 vColor1=vColor1,
                                 vColor2=vColor2,
                                 nedgemin=nedgemin,
                                 eopacity=eopacity,
                                 vmassf=vMassFactor,
                                 vsizefactor=vSizeFactor,
                                 vfontsz=c(vFontSize, 0.7*vFontSize),
                                 # Adjust font color light for dark vertex color, dark for light
                                 vfontcolor=c(ifelse(vColor1<"#CCCCCC", vColor1, "#404040"),
                                              ifelse(vColor2<"#CCCCCC", vColor2, "#404040")),
                                 elabVar=elabVar,
                                 accMethod=accMethod))
  } else {
    return(list("vertex"=data.frame(), "edge"=data.frame()))
  }

}

##########################################################################################################
# Query SNOMED concepts with an ISA relationship to a node with specific sct ID
##########################################################################################################

queryISAConcept <- function(sctid, filterOpt=NA) {

  # Retrieve all nodes leading to the specified node by ISA relationships
  query <- paste(" match(x:ObjectConcept)-[:ISA]->(y:ObjectConcept)",
                 " where y.sctid='", sctid, "' and x.active='1' and y.active='1'",
                 " return labels(x) as label, x.sctid as sctid, x.FSN as FSN",
                 " order by x.FSN", sep="")
  conceptList <- cypher(dbcon, query)

  # Filter concepts, if requested
  # This, global, approach should be refined based on some sort of user-defined filtering
  if(!is.na(filterOpt)) {
    return(conceptList[which(conceptList[,"FSN"] %in%
                             c("Clinical finding (finding)",
                               "Disease (disorder)",
                               "Mental disorder (disorder)",
                               "Neurological finding (finding)",
                               "Metabolic disease (disorder)",
                               "Motor nervous system finding (finding)",
                               "Disorder of nervous system (disorder)")),])
  } else {
    return(conceptList)
  }

}

##########################################################################################################
# Query UCD observations corresponding to participant variables, SNOMED concepts, prescriptions, and
# role groups as specified in function parameter values
# Return list of data frames containing vertex and edge configurations
##########################################################################################################

queryAndConstructGraph <- function(nodeVar, conceptID, nodeVarFilter, prescripFilter, roleGroupFilter,
                                   haFilter, prescripConnector, prescripSubsume, roleGroupConnector,
                                   haConnector, vColor1, vColor2, vColor3, vColor4, vColor5,
                                   nedgemin, eopacity, vMassFactor, vSizeFactor, vFontSize) {

  # Parameters:
  # nodeVar .............. Participant node variable - one set of vertices are constructed from participant
  #                        values of this var
  # conceptID ............ Vector of SNOMED concept IDs - one set of verices is constructed from concept
  #                        nodes in the DB with ISA relationships to supplied concepts
  # nodeVarFilter ........ Vector of participant var values (levels of nodeVar) for which to construct nodes
  #                        (empty = all) 
  # prescripFilter ....... Vector of prescription DB IDs for which to construct prescription nodes (empty = all) 
  # roleGroupFilter ...... Vector of role group DB IDs for which to construct role nodes (empty = all)
  # haFilter ............. Vector of HASxLast levels for whic to construct HA nodes
  # prescripConnector .... Instructs which set of nodes (participant or concept) prescriptions nodes are to be
  #                        connected to ("nodeVar" specifies participant nodes, "FSN" specifies concept nodes)
  # prescripSubsume ...... T = combine prescriptions into common parent nodes, F = do not combine 
  # roleGroupConnector ... Instructs which set of nodes (participant or concept) role group nodes are to be
  #                        connected to ("nodeVar" specifies participant nodes, "FSN" specifies concept nodes)
  # haConnector .......... Instructs which set of nodes (participant or concept) HA nodes are to be
  #                        connected to ("nodeVar" specifies participant nodes, "FSN" specifies concept nodes)
  # vColor1 .............. Color of participant vertices
  # vColor2 .............. Color of concept vertices
  # vColor3 .............. Color of prescription vertices
  # vColor4 .............. Color of role group vertices
  # vColor5 .............. Color of HA vertices
  # nedgemin ............. Limits edges to those with computed n (number of participants) >= this value
  # eopacity ............. Edge opacity
  # vMassFactor .......... Vertex mass factor (reduction causes greater repulsion)
  # vSizeFactor .......... Vertex size factor (does not appear to affect rendered vertex size)
  # vFontSize ............ Vertex label font size

  # Construct initial graph components, for participants and concepts
  # First set of vertices from specified participant variable, second set is always fully specified name of concepts
  netComponents <- queryAndConstructGraphPC(nodeVar1=nodeVar, nodeVar2="FSN", conceptID=conceptID, nodeVarFilter=nodeVarFilter,
                                            vColor1=vColor1, vColor2=vColor2, nedgemin=nedgemin, eopacity=eopacity,
                                            vMassFactor=vMassFactor, vSizeFactor=vSizeFactor, vFontSize=vFontSize,
                                            elabVar=NA, accMethod="PID")

  # Append prescription nodes, if requested
  # First set of vertices from specified participant variable or concept, second set prescriptions
  if(!is.null(prescripConnector)) 
    netComponents <- appendNetwork(
                       netComponents,
                       queryAndConstructGraphPrescrip(
                         nodeVar=ifelse(prescripConnector=="nodeVar", nodeVar, "FSN"),
                         conceptID=conceptID,
                         nodeVarFilter=nodeVarFilter,
                         prescripFilter=prescripFilter,
                         subsume=prescripSubsume,
                         vColor1=ifelse(prescripConnector=="nodeVar", vColor1, vColor2),
                         vColor2=vColor3,
                         nedgemin=nedgemin, eopacity=eopacity, vMassFactor=vMassFactor,
                         vSizeFactor=vSizeFactor, vFontSize=vFontSize, elabVar=NA,
                         accMethod="PID"))

  # Append role group nodes, if requested
  # First set of vertices from specified participant variable or concept, second set role groups
  # Include role descriptors in edge labels
  if(!is.null(roleGroupConnector)) 
    netComponents <- appendNetwork(
                       netComponents,
                       queryAndConstructGraphRoleGroup(
                         nodeVar=ifelse(roleGroupConnector=="nodeVar", nodeVar, "FSN"),
                         conceptID=conceptID,
                         nodeVarFilter=nodeVarFilter,
                         roleGroupFilter=roleGroupFilter,
                         vColor1=ifelse(roleGroupConnector=="nodeVar", vColor1, vColor2),
                         vColor2=vColor4,
                         nedgemin=nedgemin, eopacity=eopacity, vMassFactor=vMassFactor,
                         vSizeFactor=vSizeFactor, vFontSize=vFontSize, elabVar="role",
                         accMethod="PID"))

  # Append HA nodes, if requested
  # First set of vertices from specified participant variable or concept, second set HASxLast
  if(!is.null(haConnector)) 
    netComponents <- appendNetwork(
                       netComponents,
                       queryAndConstructGraphHA(nodeVar=ifelse(haConnector=="nodeVar", nodeVar, "FSN"),
                       conceptID=conceptID,
                       nodeVarFilter=nodeVarFilter,
                       haFilter=haFilter,
                       vColor1=ifelse(haConnector=="nodeVar", vColor1, vColor2),
                       vColor2=vColor5,
                       nedgemin=nedgemin, eopacity=eopacity, vMassFactor=vMassFactor,
                       vSizeFactor=vSizeFactor, vFontSize=vFontSize, elabVar="role",
                       accMethod="PID"))

  # Return two element list containing vertices and edges
  return(netComponents)

}