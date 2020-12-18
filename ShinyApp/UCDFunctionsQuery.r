###########################################################################################################
# Duke University UCD Shiny App, Apr 2020
# Query functions
###########################################################################################################

# For use with Neo4j version 4

###########################################################################################################
# Notes on querying Neo4j from within R
#
# R Package RNeo4j functions properly with Neo4j version 3.5 but, due to changes in http services in
# version 4, cannot make database connections
# RNeo4j is no longer maintained (last activity appears to be in 2016) and has been removed from CRAN
#
# R package neo4r has been experimented with, but response time is slow, perhaps due to its sole
# row set return format of tibbles (packaging results in a tibble may be time consuming)
# Tabular results are required here, so that tibble formatting is an unnecessary use of time
#
# Query functions implemented use direct API calls using Neo4j's http interface
# Once a database instance is running (typically on port 7474) queries are passed to it using the
# PUT() function from the httr package
# Results are returned in a json structure, which is converted to a list using the fromJSON() function
# of the rjson package
# The resulting list contains one element per row and column in the result set
# Row and column list elements are transformed into a data frame using columnwise lapply() operations
########################################################################################################## 

##########################################################################################################
# Neo4j query function implemented using PUT operation
# Parameter query contains a properly formatted Cypher query
# Return value is a list with two elements:
#   status .... "", if no errors reported by Neo4j service
#               text contained in the error element of json structure returned by Neo4j, if error occurs
#   data ...... data frame containing Cypher query results, if no error
#               0-row, 0-column data frame, if PUT() or Neo4j report an error
##########################################################################################################

neo4jQuery <- function(neo4jPortDB, neo4jUID, neo4jPW, query) {

  # Establish connection and begin transaction
  # All queries, even those limited to record selection, are executed within a transaction
  # A post to the database/db url returns a transaction ID to which the query will be targeted
  # The transaction has a brief lifespan, so one is needed prior to execution of each query
  # The leading portion of the target url appears in the [["headers"]][["location"]] element of
  # the returned list
  # The complete target url is the leading portion followed by "/commit"
  neo <- try(POST(paste(neo4jPort, "/db/", neo4jDB, "/tx", sep=""), authenticate(neo4jUID, neo4jPW)), silent=T)

  # Evaluate port connection success
  if(class(neo)!="try-error") {

    # Evaluate database connection success
    if(class(neo)=="response") {
      if(neo[["status_code"]]==201) {

        # Extract transaction target url
        nurl <- paste(neo[["headers"]][["location"]], "/commit", sep="")

        # Execute query
        # Brackets [] enclosing statement are required (otherwise "Unable to deserialize request ..." error)
        qjson <- POST(nurl, authenticate(neo4jUID, neo4jPW),
                      body=paste('{ "statements" : [{ "statement" : "', gsub("\n", " ", query), '" }] }', sep=""),
                      content_type_json(), encode="raw")

        # Parse json elements into a list
        qlist <- fromJSON(rawToChar(qjson[["content"]]), simplify=F)

        # Evaluate error status returned by Neo4j
        if(length(qlist[["errors"]])==0) {
          # Retrieve column names
          cnames <- unlist(qlist[["results"]][[1]][["columns"]])
          # Parse data rows one column at a time
          # Convert null database values to NA, since nulls are ignored by R, causing row counts of zero
          # that cannot be combined with non-zero row counts
          qdat <- do.call(cbind,
                          lapply(1:length(cnames),
                            function(j)
                              unlist(lapply(1:length(qlist[["results"]][[1]][["data"]]),
                                       function(i)
                                         ifelse(!is.null(qlist[["results"]][[1]][["data"]][[i]][["row"]][[j]]),
                                                qlist[["results"]][[1]][["data"]][[i]][["row"]][[j]],
                                                NA)))))
          colnames(qdat) <- cnames
          return(list("status"="", "data"=as.data.frame(qdat)))

        } else {
          # Return Neo4j error text
          return(list("status"=paste(unlist(qlist[["errors"]]), collapse="; ", sep=""), "data"=data.frame()))
        }
      } else {
        # Return http response
        # Note that "errors" or "content" elements may not exist in neo, depending on the error
        return(list(#"status"=neo,
                    "status"=paste(unlist(fromJSON(rawToChar(neo[["content"]]), simplify=F)), collapse="; ", sep=""),
                    "data"=data.frame()))
      }
    } else {
      return(list("status"="Unrecognized object returned by Neo4j server", "data"=data.frame()))
    }
  } else {
    # Report port connection error
    return(list("status"=neo[1], "data"=data.frame()))
  }

}

##########################################################################################################
# Query SNOMED concepts with an ISA relationship to a node with specific sctID
# Specifying sctID of 0 returns values for the root SNOMEDCT concept
##########################################################################################################

queryISAConcept <- function(sctid) {

  if(sctid!=0) {

    # Retrieve all nodes leading to the specified node by ISA relationships
    # Note the concatenation of labels, since Cypher returns a list of labels when multiples exist
    # Cypher substring positions are 0-based
    query <- paste(" match(x:ObjectConcept)-[r:ISA]->(y:ObjectConcept)",
                   " where y.sctid='", sctid, "' and x.active='1' and y.active='1' and r.active='1'",
                   " with reduce(a='', b in labels(x)|a+', '+b) as label, x.sctid as sctid, x.FSN as FSN",
                   " return substring(label, 2, size(label)-2) as label, sctid, FSN",
                   " order by FSN", sep="")
  } else {
    query <- "match(x) where x.FSN contains 'SNOMED CT Concept' and x.active='1'
              with     reduce(a='', b in labels(x)|a+', '+b) as label, x.sctid as sctid, x.FSN as FSN
              return   label, sctid, FSN limit 1"
  }
  conceptMbr <- neo4jQuery(neo4jPortDB, neo4jUID, neo4jPW, query)
  if(conceptMbr[["status"]]=="") {
    if(!is.na(conceptMbr[["data"]][1,1])) {
      return(data.frame("label"=conceptMbr[["data"]][,"label"],
                        "sctid"=conceptMbr[["data"]][,"sctid"],
                        "FSN"=paste(conceptMbr[["data"]][,"FSN"], " ", conceptMbr[["data"]][,"sctid"], sep="")))
    } else {
      showNotification("SNOMED CT concept has no sub-members", type="error")
      return(NULL)
    }
  } else {
    showNotification(conceptMbr[["status"]], type="error")
    return(NULL)
  }

}

##########################################################################################################
# Query UCD participant variables, SNOMED concepts, prescriptions, and finding sites
##########################################################################################################

queryNetworkData <- function() {

  # Query observations, filtered using the "query" element of the current global graph configuration

  # The filter is a named list, where names indicate the database variable to be filtered and
  # the corresponding elements are:
  #   1. for concept, a list containing elements "style," "values," and "op," where
  #      the style element contains either "ID" or "contains"
  #        Style "ID" instructs to compose a where clause containing "and sctid in('x', 'y', 'z', ...)"
  #          where 'x', 'y', 'z', ... are concept IDs taken from the values element, a vector
  #        Style "contains" instructs to parse elements of the values element, a semi-colon delimited
  #          string of terms into "a,", "b,", ... , "c" and to compose a where clause containing
  #          "(and x.FSN contains 'a' * x.FSN contains 'b' * ... * x.FSN contains 'c')" where
  #          x is the ObjectConcept node referenced in the Cypher query and op is the value contained
  #          in the "op" element (either "and" or "or")
  #          NOTE that style "contains" can cause artificial weighting of concepts by return multiple paths
  #          for a single concept
  #          Consider concepts a, b, and c, where c is a child of b and b is a child of a, both
  #          a and b have "disorder" in their FSN, and a participant exists with a P_SCT relationship to
  #          c.  Since c is on paths to both b and a, it will be accumulated once to individual nodes
  #          for concepts a and b.  Since the analytical paradign of this project is to accumulate
  #          participant/concept combinations to specified terminating SNOMEDCT concepts, this style
  #          should be used with caution.  Further, the number of paths in the data, combined with
  #          the method of row set collection (all participants, concepts in satisfying paths, Rx, and
  #          finding site combinations are returned), generally results in a large, expansive query
  #          that does not complete efficiently, unless very restrictive search terms are specified.
  #        Style "lead" instructs to parse elements of the values element, a semi-colon delimited
  #          string of terms into "a,", "b,", ... , "c" and to compose a where clause containing
  #          "(and left(x.FSN, length('a'))='a' * left(x.FSN, length('b'))='b' * ... * left(x.FSN, length('c')) contains 'c')"
  #          NOTE that, because text searches are limited to leading text, this style remedies the
  #          duplicate weighting problem mentioned above, for the "contains" style, provided that
  #          sufficient, distuinguishing search terms are specified
  #          For instance, specifying "Mood disorder" omits conept "Mild mood disorder," which is a
  #          member of "Mood disorder."  However it also retains "Mood disorder of a depressed type."
  #          It is recommended to supply as much distinguishing text in search terms as is possible.
  #        Style "exact" instructs to parse elements into "a," "b," and "c" then compose the where
  #          clause "and x.FSN in['a', 'b', 'c']
  # 
  #   2. for remaining variables, a vector of database values corresponding to levels to be included

  # If a variable is not included in the query filter names, then no filtering is done to that variable
  # Note that a variable name can be repeated, but this will cause a null set to be returned if the 
  # intersection of all vectors for any one variable is null (two vectors c("a", "b") and c("c", "d")
  # for variable x generates a where clause such as where x in('a', 'b') and x in('c', 'd') which is null) 

  # Note that concept IDs appearing in the query filter are always used to select children nodes with
  # an ISA relationship to the specified concept
  # Multiple concepts and variable filters can be specified

  # The result is a data frame with one row per participant var, concept, prescription, and finding site
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

  # Compose search term filter strings
  # Limit to database variables, which omits, for instance, interaction variables
  # Note that all observed variables in the UCD database are encoded as character strings, even those
  # with numeric appearance (hence the use of delimiting apostrophes here)
  filtConcept <- ""
  conceptOrder <- ""
  filtVar <- ""
  for(i in 1:length(graphCfg[[gcPtr]][["query"]])) {
    vid <- names(graphCfg[[gcPtr]][["query"]])[i]
    qi <- graphCfg[[gcPtr]][["query"]][[i]]
    if(vid=="concept") {
      if(tolower(qi[["style"]])=="id") {
        # Compose "and sctid in(...)" where clause
        filtConcept <- paste(" and sct2.sctid in['", paste(qi[["values"]], collapse="', '", sep=""), "']", sep="")
        # Compose statement to order sub-concepts (returned by query) in order of their parent (specified in
        # conceptID vector)
        conceptOrder <- paste(
                          "case sct2.sctid ",
                          paste(
                            paste("when('", qi[["values"]], "')then ", qi[["conceptOrder"]], " ", sep=""),
                            collapse=""),
                          " end", sep="")
      } else if(tolower(qi[["style"]])=="exact") {
        # Compose "and (FSN='a' and/or FSN='b' ...)" clause
        # Insert and/or operation between filter values
        filtConcept <- paste(
                         " and (",
                         paste(paste(" sct2.lowcaseFSN='", tolower(qi[["values"]]), "' ", sep=""),
                               collapse=tolower(qi[["op"]]), sep=""),
                         ")")
        # Compose concept order case statement
        # Note that an FSN from a single observation may satisfy multiple search terms
        # The first search term satisfied in the composed case statement assigns concept order
        # Beware of joining concepts of high order (more than three), since the labels become too
        # long to be legible on the rendered graph
        conceptOrder <- paste(
                          "case ",
                          paste("when(sct2.lowcaseFSN='", tolower(qi[["values"]]), "')then ", qi[["conceptOrder"]],
                                collapse=" ", sep=""),
                          " end ", sep="")
      } else if(tolower(qi[["style"]])=="lead") {
        # Compose "and (left(FSN, length('a'))='a' and/or left(FSN, length('b'))='b' ...)" clause
        # Combine char length with and/or operation between filter values
        filtConcept <- paste(
                         " and (",
                         paste(paste(" sct2.lowcaseFSN starts with '", tolower(qi[["values"]]), "' ", sep=""),
                               collapse=tolower(qi[["op"]]), sep=""),
                         ")")
        # Compose concept order case statement
        # Note that an FSN from a single observation may satisy multiple search terms
        # The first search term satisfied in the composed case statement assigns concept order
        # Beware of joining concepts of high order (three or so), since the labels become too
        # long to be legible on the rendered graph
        conceptOrder <- paste(
                          "case ",
                          paste("when(sct2.lowcaseFSN starts with '", tolower(qi[["values"]]), "')then ",
                                qi[["conceptOrder"]], collapse=" ", sep=""),
                          " end ", sep="")
      } else if(tolower(qi[["style"]])=="contains") {
        # Compose "and (FSN contains 'a' and/or FSN contains 'b' ...)" clause
        # Insert and/or operation between filter values
        filtConcept <- paste(
                         " and (",
                         paste(paste(" sct2.lowcaseFSN contains '", tolower(qi[["values"]]), "' ", sep=""),
                               collapse=tolower(qi[["op"]]), sep=""),
                         ")")
        # Compose concept order case statement
        # Note that an FSN from a single observation may satisy multiple search terms
        # The first search term satisfied in the composed case statement assigns concept order
        # Beware of joining concepts of high order (three or so), since the labels become too
        # long to be legible on the rendered graph
        conceptOrder <- paste(
                          "case ",
                          paste("when(sct2.lowcaseFSN contains '", tolower(qi[["values"]]), "')then ",
                                qi[["conceptOrder"]], collapse=" ", sep=""),
                          " end ", sep="")
      } else {
        filtConcept <- ""
        conceptOrder <- "1"
      }
    } else {
      # Non-concept variable
      # Compose vid in['id1', 'id2', ...] clause
      filtVar <- paste(filtVar, " and ", vid, " in['", paste(qi, collapse="', '", sep=""), "']", sep="")
    }
  }

  # Compose query
  query <- paste(# Retrieve participant / SNOMED concept pairs
                 # Note that P_SCT relationships have no keys (fields) and, therefore, no status
                 # All are assumed to be active
                 # Participants have an EligibilityStatus field, but no 'active' field
                 # Two types of participant->concept path are explored:
                 #   1. those with a P_SCT relationship from a participant to a concept that has a path
                 #      consisting of ISA relationships to a concept specified in filtConcept and
                 #   2. those with a single P_SCT relationship from a participant directly to a concept
                 #      specified in filtConcept
                 # Type 1 paths result in selecting concepts with a length-one path to a terminating concept
                 # Type 2 paths result in selecting the terminating concept itself
                 # This strategy overcomes an ambiguity that arises when the path from participant to
                 # terminating concept is a single P_SCT relationship
                 # If concept selection is limited to those immediately prior to the terminating concept in
                 # sequence of ISA relationships then participant->P_SCT->terminating concept paths are
                 # excluded and participants "disappear" as a concept is selected for drill-down exploration
                 # Consider concepts A, B, and C where C-ISA->B-ISA->A, where -ISA-> indicates an ISA
                 # relationship; participants p1 and p2, both with property D; and p1-SCT->B, p2-SCT->C,
                 # where -SCT-> indicates a P_SCT relationship
                 # When a graph displays A and D with a connecting edge, both p1 and p2 are included in
                 # participant counts for the edge, indicating relationships between D and some children of A
                 # If B is selected for expansion, the relationship p2->B is retained (because of C-ISA->B)
                 # but, unless p1-SCT->B is retained, p1->B disappears and participant counts appear inconsistent
                 # To maintain consistency of participant counts as concepts are expanded, the two path types
                 # are combined, all paths terminating in a chosen concept, but some leading to children of
                 # that concept, some for which the concept has no children (for certain participants)
                 # The call{} function of Neo4j was added in version 4 and is needed to execute a union
                 # of participant->concept paths
                 " call {",
                 "   match  (prt:Participant)-[:P_SCT]->(sct0:ObjectConcept)-[rsct:ISA*]->(sct2:ObjectConcept)",
                 "   where  1=1", filtConcept,
                 "          and sct0.active='1' and sct2.active='1'",
                 # Due to [rsct:ISA*] having indeterminate length, we must evaluate all relationships in the path
                 "          and all(r2 in rsct where r2.active='1')",
                 # Retrieve leading node (sct1) in final path terminating at sct2
                 # This accepts paths from any concept with a path (regardless of length) to sct2 and
                 # aggregates all of them to the immediate child 
                 "  return  prt, sct0, startNode(last(rsct)) as sct1, sct2.FSN as parentConceptFSN,",
                 conceptOrder, " as conceptOrder",
                 "  union",
                 "  match  (prt:Participant)-[:P_SCT]->(sct2:ObjectConcept)",
                 "  where  1=1", filtConcept,
                 "  return prt, sct2 as sct0, sct2 as sct1, sct2.FSN as parentConceptFSN,",
                 conceptOrder, " as conceptOrder",
                 " }",
                 " with   prt, sct0, sct1, conceptOrder, parentConceptFSN",
                 " where  sct1.active='1'",
                 # Left join to prescriptions (note that the where clause is applied within the join,
                 # such that nulls are returned in rx0 and rx1 nodes when no prescriptions exist for a prt
                 # or when staus values are not 'active')
                 # P_SCT relationships have no keys (fields) and, therefore, no status
                 # All are assumed to be active
                 " optional match(prt)-[:P_RX]->(rx0:RXCUI)",
                 " where  toLower(rx0.status)='active'",
                 # SUBSUMES relationships have no keys (fields) and, therefore, no status
                 # All are assumed to be active
                 " optional match(rx0)<-[:SUBSUMES]-(rx1:RXCUI)",
                 " where    toLower(rx1.status)='active'",
                 " with     prt, sct0, sct1, conceptOrder, parentConceptFSN, rx0, rx1",
                 # Left join to finding sites from first node in path from participant to terminating concept node
                 # This returns detailed finding sites that are children of the top level nodes returned (sct1),
                 # since nodes that are higher in the hierarchy tend to have vague finding sites
                 # Finding sites are reachable through role groups only (no observed finding sites have a direct
                 # relationship from a concept
                 # HAS_ROLE_GROUP relationships have no keys (fields) and, therefore, no status
                 # All are assumed to be active
                 " optional match(sct0)-[role1:HAS_ROLE_GROUP]->(rg)-[role2:FINDING_SITE]->(fst:ObjectConcept)",
                 " where  role2.active='1' and fst.active='1'",
                 " with     distinct",
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
                 "   sct1.sctid as conceptID, sct1.FSN as conceptFSN, conceptOrder, parentConceptFSN,",
                 #   Use rx for subsuming rx when not subsumed (optional subsuming match returns null)
                 "   coalesce(rx1.id, rx0.id) as rxSubsID, coalesce(rx1.name, rx0.name) as rxSubsName,",
                 "   rx0.id as rxID, rx0.name as rxName,",
                 "   fst.sctid as findingSiteID, fst.FSN as findingSiteFSN, type(role2) as fsRole",
                 " where 1=1", filtVar,
                 " return",
                 "   participantID, coalesce(UCDProxDist, 'na') as UCDProxDist,",
                 "   coalesce(Sex, 'na') as Sex, coalesce(HASxLast, 'na') as HASxLast,",
                 "   coalesce(UCDDx, 'na') as UCDDx, coalesce(onsetAgeDays, 'na') as onsetAgeDays,",
                 "   conceptID,",
                 #"   case when(conceptOrder>0)then '('+conceptOrder+') ' else '' end + conceptFSN as conceptFSN,",
                 "   '('+conceptOrder+') ' + conceptFSN as conceptFSN,",
                 "   conceptOrder, parentConceptFSN,",
                 "   coalesce(rxSubsID, 'na') as rxSubsID, coalesce(rxSubsName, 'na') as rxSubsName,",
                 "   coalesce(rxID, 'na') as rxID, coalesce(rxName, 'na') as rxName,",
                 "   coalesce(findingSiteID, 'na') as findingSiteID, coalesce(findingSiteFSN, 'na') as findingSiteFSN,",
                 "   coalesce(fsRole, 'na') as fsRole", sep="")

  print(query)
  x <- try(neo4jQuery(neo4jPortDB, neo4jUID, neo4jPW, query), silent=T)
  if(x[["status"]]=="") {
    return(x[["data"]])
  } else {
    showNotification(x[["status"]], type="error")
    return(NULL)
  }

}

##########################################################################################################
# Query paths from FSN1 to FSN2 using active ISA relationships
# Use supplied FSNs as leading search text
# FSN2=="" implies SNOMEDCT root
# A data frame is returned with columns sctid and FSN
##########################################################################################################

queryPath <- function(FSN1, FSN2) {

  if(FSN2=="")
    FSN2 <- "snomed ct concept"
  if(nchar(FSN1)>0 & nchar(FSN2)>0) {
    query <- paste(" match(x:ObjectConcept)-[r:ISA*]->(y:ObjectConcept)",
                   " where x.active='1'",
                   "       and left(x.lowcaseFSN, ", nchar(FSN1), ")='", tolower(FSN1), "'",
                   #"       and y.active='1'",
                   "       and left(y.lowcaseFSN, ", nchar(FSN2), ")='", tolower(FSN2), "'",
                   "       and reduce(x=0, y in r | x + case when(y.active='0' or endNode(y).active='0')then 1 else 0 end)=0",
                   " return reduce(x='', y in r | endNode(y).FSN + '[' + endNode(y).sctid + ']; ' + x) + ",
                   "        x.FSN + '[' + x.sctid + ']' as path",
                   sep="")
    x <- try(neo4jQuery(neo4jPortDB, neo4jUID, neo4jPW, query), silent=T)
    if(x[["status"]]=="") {
      if(nrow(x[["data"]])>0) {
        path <- x[["data"]][,"path"]
      } else {
        path <- "No path returned"
      }
    } else {
      showNotification(x[["status"]], type="error")
      path <- ""
    }
  } else {
    path <- ""
  }

  return(path)

}