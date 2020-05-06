#####################################################################################################
# Duke University UCD Shiny App, Apr 2020
# Query functions
#####################################################################################################

##########################################################################################################
# Query SNOMED concepts with an ISA relationship to a node with specific sctID
# Specifying sctID of 0 returns values for the root SNOMEDCT concept
##########################################################################################################

queryISAConcept <- function(sctid, filterOpt=NA) {

  if(sctid!=0) {

    # Retrieve all nodes leading to the specified node by ISA relationships
    query <- paste(" match(x:ObjectConcept)-[:ISA]->(y:ObjectConcept)",
                   " where y.sctid='", sctid, "' and x.active='1' and y.active='1'",
                   " return labels(x) as label, x.sctid as sctid, x.FSN as FSN",
                   " order by x.FSN", sep="")
    conceptList <- cypher(db, query)

    # Filter concepts, if requested
    # This, global, approach should be refined based on some sort of user-defined filtering
    if(!is.na(filterOpt))
      conceptList <- conceptList[
                       which(conceptList[,"FSN"] %in%
                               c("Clinical finding (finding)",
                                 "Disease (disorder)",
                                 "Mental disorder (disorder)",
                                 "Neurological finding (finding)",
                                 "Metabolic disease (disorder)",
                                 "Motor nervous system finding (finding)",
                                 "Disorder of nervous system (disorder)")),]

  } else {

    query <- "match(x) where x.FSN contains 'SNOMED CT Concept' and x.active='1'
              return   labels(x) as label, x.sctid as sctid, x.FSN as FSN limit 1"
    conceptList <- cypher(db, query)[1,]

  }

    return(conceptList)

}

##########################################################################################################
# Query UCD participant variables, SNOMED concepts, prescriptions, and finding sites
##########################################################################################################

queryNetworkData <- function(filter) {

  # Parameters:
  # filter ......... a list with:
  #                  1. elements named according to variables to be filtered (UCDProxDist, Sex, Age,
  #                     UCDDx, prescripID, findingSiteID) 
  #                  2. a vector of levels (values of participant vars) or IDs associated with element name

  # Result is a data frame with one row per participant var, concept, prescription, and finding site
  # Although values within columns are not unique, all combinations of levels of two variables exist
  # in various rows
  # Unique values within columns constitute nodes for the corresponding var
  # Unique pairs of values within two columns constitute edges between corresponding vars

  # Path notes:

  # Participant -> Concept:
  # Note the specification of directed edges although, in the db, they are limited to participant->concept
  # Also note the retrieval of unique combinations of participant and concept to avoid bias due to duplicate observations 
  # Although various types of concept nodes (ie, labels ["ObjectConcept", "Neuro", "Psych"], ["ObjectConcept", "Psych"])
  # are related to the terminating concept identified here by z.sctid (relationship type ISA), all observed ISA
  # relationships from participants to concept nodes with an ISA relation to the terminating concept are of type
  # ["ObjectConcept", "Psych", "PPsych", "PSCT"]
  # All observed relationships from participants to concepts are of type P_SCT
  # The specification [rcn:ISA*], below, identifies all paths from a participant to a concept of interest
  # Later, startNode(last(rcn)) returns the leading node in the last relationship of the rcn path
  # Effectively, all paths from a participant to the requested concept (parameter of this function) produce
  # one result row labeled with the immediate child of the requested concept
  # Distinct combinations of participant, concept are returned

  # Participant -> Prescription:
  # Note the specification of directed edges although, in the db, they are limited to participant->rx
  # Both prescription and subsuming presecription are returned so that a graph can be constructed at
  # detailed prescription or parent-grouped levels
  # Not all participants have prescriptions and not all prescriptions are subsumed
  # Therefore, optional matches (left joins) are used to retrieve prescriptions
  # If a prescription is not subsumed by another then the prescription is returned as its subsuming RX

  # Concept -> finding site:
  # Role groups are used to identify finding sites
  # The relation HAS_ROLE_GROUP is used to locate role group nodes (rg), the only type of node
  # that occur in these relationships
  # Then a relationship type of FINDING_SITE is used to locate a concept node containing the
  # ID and description of the actual site
  # Since many SNOMED concepts do not have associated finding sites (in the DB), left joins are used

  # Compose concept filter string

  i <- which(names(filter)=="conceptID")
  if(length(i)>0) {
    filtConcept <- paste(" and sct2.sctid in['", paste(filter[[i[1]]], collapse="', '", sep=""), "']", sep="")
  } else {
    filtConcept <- ""
  }

  # Compose filter strings for remaining variables
  filtVar <- ""
  for(i in which(names(filter)!="conceptID"))
    filtVar <- paste(filtVar, " and ", names(filter)[i], " in['", paste(filter[[i]], collapse="', '", sep=""), "']", sep="")

  # Compose query
  query <- paste(# Retrieve pairs of participants and SNOMED concepts",
                 " match  (prt:Participant)-[:P_SCT]->(sct0:ObjectConcept)-[rsct:ISA*]->(sct2:ObjectConcept)",
                 " where  1=1", filtConcept,
                 "        and sct0.active='1' and sct2.active='1'",
                 # Retrieve leading node in final path terminating at sct0
                 # This accepts paths from any concept with a path (regardless of length) to sct0 and
                 # aggregates all of them to the immediate child of sct0 
                 " with   prt, sct0, startNode(last(rsct)) as sct1",
                 " where  sct1.active='1'",
                 # Left join to prescriptions (note that the where clause is applied within the join,
                 # such that nulls are returned in rx0 and rx1 nodes when no prescriptions exist for a prt
                 # or when staus values are not 'active')
                 " optional match(prt)-[:P_RX]->(rx0:RXCUI)",
                 " where  toLower(rx0.status)='active'",
                 " optional match(rx0)<-[:SUBSUMES]-(rx1:RXCUI)",
                 " where  toLower(rx1.status)='active'",
                 " with   prt, sct0, sct1, rx0, rx1",
                 # Left join to finding sites from first node in path from participant to terminating concept node
                 " optional match(sct0)-[role1:HAS_ROLE_GROUP]->(rg)-[role2:FINDING_SITE]->(fst:ObjectConcept)",
                 " where  role2.active='1' and fst.active='1'",
                 " with distinct",
                 "   prt.ParticipantId as participantID,",
                 "   case when(labels(prt)=['Participant'])then 'UCDDist'",
                 "        when(labels(prt)=['Participant','UCD_Proximal'])then 'UCDProx'",
                 "        else 'na'",
                 "   end as UCDProxDist,",
                 "   prt.Sex as Sex, prt.UCDDx as UCDDx, prt.HASxLast as HASxLast,",
                 "   case when(toInteger(prt.OnsetAgeDays)<11)then '0-11'",
                 "        when(toInteger(prt.OnsetAgeDays)<101)then '11-100'",
                 "        when(toInteger(prt.OnsetAgeDays)<1001)then '101-1000'",
                 "        when(toInteger(prt.OnsetAgeDays)<10001)then '1001-10000'",
                 "        when(toInteger(prt.OnsetAgeDays) is not null)then '>10000'",
                 "        else null",
                 "   end as onsetAgeDays,",
                 "   sct1.sctid as conceptID, sct1.FSN as conceptFSN,",
                 #   Substitute rx for subsuming rx when not subsumed",
                 "   coalesce(rx1.id, rx0.id) as rxSubsID, coalesce(rx1.name, rx0.name) as rxSubsName,",
                 "   rx0.id as rxID, rx0.name as rxName,",
                 "   fst.sctid as findingSiteID, fst.FSN as findingSiteFSN, type(role2) as fsRole",
                 " where 1=1", filtVar,
                 " return",
                 "   participantID, coalesce(UCDProxDist, 'na') as UCDProxDist,",
                 "   coalesce(Sex, 'na') as Sex, coalesce(HASxLast, 'na') as HASxLast,",
                 "   coalesce(UCDDx, 'na') as UCDDx, coalesce(onsetAgeDays, 'na') as onsetAgeDays,",
                 "   conceptID, conceptFSN,",
                 "   coalesce(rxSubsID, 'na') as rxSubsID, coalesce(rxSubsName, 'na') as rxSubsName,",
                 "   coalesce(rxID, 'na') as rxID, coalesce(rxName, 'na') as rxName,",
                 "   coalesce(findingSiteID, 'na') as findingSiteID, coalesce(findingSiteFSN, 'na') as findingSiteFSN,",
                 "   coalesce(fsRole, 'na') as fsRole", sep="")

  #print(query)
  return(cypher(db, query))

}