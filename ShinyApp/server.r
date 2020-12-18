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
#library(RNeo4j)
library(httr)
library(rjson)
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
shiftClickColor <- "#FF00FF"

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
          # Database and label variables - query result columns containing names and labels of covariates
          "dbVar"=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptID", "rxID", "findingSiteID"),
          "labVar"=c("UCDProxDist", "Sex", "UCDDx", "onsetAgeDays", "HASxLast", "conceptFSN", "rxID", "findingSiteFSN"),
          # Legend text
          "labLegend"=c("UCD Proximal/Distal", "Sex", "UCD Dx", "Onset Age in Days", "HA Sympt Last", "SNOMED Concept", "Rx", "Finding Site"),
          # Color, size, additional label variable for edges, accumulation method (unique participant ID frequencies)
          "vColor"=c(vc1, rgb(matrix(col2rgb(vc1)/255*0.85, nrow=1)), rgb(matrix(col2rgb(vc1)/255*0.7, nrow=1)),
                     rgb(matrix(col2rgb(vc1)/255*0.55, nrow=1)), vc5, vc2, vc3, vc4),
          "tSize"=c(1, 1, 1, 1, 1, 1, 1, 1),
          "tColor"=c(pfc, pfc, pfc, pfc, vc5, vc2, vc3, vc4),
          "elabVar"=c(NA, NA, NA, NA, NA, NA, NA, "fsRole"))
rownames(vcfg) <- vcfg[,"dbVar"]

# Reactive value list (used to copy values of reactive variables prior to being modified
reactVal <- list()

##########################################################################################################
# Load functions
##########################################################################################################

source("UCDFunctionsQuery.r", local=T, echo=F)
source("UCDFunctionsGraph.r", local=T, echo=F)

##########################################################################################################
# Create global variables
##########################################################################################################

# Database connections elements
neo4jPort <- c("http://localhost:7474", "http://localhost:7479")[1]
neo4jUID <- "neo4j"
neo4jPW <- c("neo4j01", "Duke123!")[1]
neo4jDB <- "ucddb-2018-12-23.db"

# Connect to Neo4j
cn <- try(GET(url=neo4jPort, authenticate(neo4jUID, neo4jPW)), silent=F)
if(class(cn)=="try-error") {
  showNotification(cn[1], type="error")
} else if(class(cn)=="response") {
  if(cn[["status_code"]]!=200)
    showNotification(paste(unlist(fromJSON(rawToChar(cn[["content"]]), simplify=F)), collapse="; ", sep=""), type="error")
} else {
  showNotification("Unrecognized reponse from Neo4j during initial connection attempt", type="error")
}

# Create a graph configuration stack and a pointer to the current configuration
# The stack is descended into and ascended out of as graph nodes are selected and expanded
# Parameter values stored on the stack are used to requery the DB and construct a graph instance
graphCfg <- list()
gcPtr <- 0

# Interaction flag
interactionRendered <- F

##########################################################################################################
# Shiny server function
##########################################################################################################

shinyServer(
  function(input, output, session) {

    ##########################################################################################################
    # Configure objects that are shared within the Shiny environment 
    ##########################################################################################################

    # exploreConceptStack contains the stack of concepts chosen by user
    # csPtr points to the position in the stack corresponding to the current select concept
    exploreConceptStack <- data.frame(nodeLabel=character(), sctid=character(), FSN=character())
    csPtr <- 0

    # Create a data frame of selected node indicators (using shift-click)
    # These are used in sub-netting operations
    # Note that this must exist prior to using shift-click and alt-click (select and deselect)
    shiftClickNode <- data.frame("nodeID"=integer(), "color"=character())

    ##########################################################################################################
    # Concept member choice list event
    # Append selected concept to selection stack
    # Update choice list with members of selected concept (descend one level in concept hierarchy)
    ##########################################################################################################

    observeEvent(input$exploreChoices,{

      if(substr(input$exploreChoices, 1, 2)=="<-") {
        # Retreat one position in concept stack and trim current concept from dispayed path
        if(csPtr>1) {
          csPtr <<- csPtr-1
          output$exploreCurrRootPath <- renderText(HTML(
                                          paste("<font color=blue>",
                                            paste(exploreConceptStack[1:csPtr,"FSN"], collapse="; ", sep=""),
                                            "</font>", sep="")))
          # Retrieve all nodes leading to the current node (in current stack pos) by ISA relationships
          # Prepend with return to parent option, if not at root SNOMED node (which has no parent)
          if(csPtr>1) {
            exploreRootMbrs <<- rbind(data.frame(
                                        "label"="", "sctid"="0",
                                        "FSN"=paste("<- ", exploreConceptStack[csPtr,"FSN"], sep="")),
                                      queryISAConcept(exploreConceptStack[csPtr,"sctid"]))
          } else {
            exploreRootMbrs <<- queryISAConcept(exploreConceptStack[csPtr,"sctid"])
          }
          # Update selection list with current value of NA to avoid triggering an immediate update event
          updateSelectInput(session, "exploreChoices", choices=exploreRootMbrs[,"FSN"], selected=NA)
        }
      } else {
        # Select concept and refresh choices
        # Identify position of selected concept in current concept list
        k <- which(exploreRootMbrs[,"FSN"]==input$exploreChoices)
        if(length(k)>0) {
          # Advance concept stack position and save selected concept
          csPtr <<- csPtr+1
          exploreConceptStack[csPtr,] <<- exploreRootMbrs[k[1],]
          rownames(exploreConceptStack) <- NULL
          # Update current root node
          output$exploreCurrRootPath <- renderText(HTML(paste("<font color=blue>", paste(exploreConceptStack[1:csPtr,"FSN"], collapse="; ", sep=""), "</font>", sep="")))
          # Retrieve all nodes leading to the current node (in current stack pos) by ISA relationships
          exploreRootMbrs <<- rbind(data.frame(
                                      "label"="", "sctid"="0",
                                      "FSN"=paste("<- ", exploreConceptStack[csPtr,"FSN"], sep="")),
                                    queryISAConcept(exploreConceptStack[csPtr,"sctid"]))
          # Update selection list with current value of NA to avoid triggering an immediate update event
          updateSelectInput(session, "exploreChoices", choices=exploreRootMbrs[,"FSN"], selected=NA)
        }
      }

    }, ignoreInit=T)

    ##########################################################################################################
    # Return to parent concept action (return to previous concept in stack)
    ##########################################################################################################

    observeEvent(input$retParentConcept,{

      if(csPtr>1) {
        csPtr <<- csPtr-1
        #output$exploreCurrRootPath <- renderText(HTML(paste("<font color=blue>", exploreConceptStack[csPtr,"FSN"], "</font>", sep="")))
        output$exploreCurrRootPath <- renderText(HTML(paste("<font color=blue>", paste(exploreConceptStack[1:csPtr,"FSN"], collapse="; ", sep=""), "</font>", sep="")))
        # Retrieve all nodes leading to the current node (in current stack pos) by ISA relationships
        exploreRootMbrs <<- queryISAConcept(exploreConceptStack[csPtr,"sctid"])
        # Update selection list with current value of NA to avoid triggering an immediate update event
        updateSelectInput(session, "exploreChoices", choices=exploreRootMbrs[,"FSN"], selected=NA)
      }

    }, ignoreInit=T)

    ##########################################################################################################
    # Concept selection event (to be included in query)
    # Append current (root) concept to selected concept data frame
    ##########################################################################################################

    observeEvent(input$conceptSelect,{

      # Append current (root) concept to selected concept data frame
      # Include concept description in displayed list
      #print(exploreConceptStack)
      queryConcept <<- rbind(queryConcept, data.frame("ID"=exploreConceptStack[csPtr,"sctid"], "FSN"=exploreConceptStack[csPtr,"FSN"]))
      output$queryConceptFSN <- renderText(paste(
                                            paste("(", 1:nrow(queryConcept), ") ", queryConcept[,"FSN"], sep=""),
                                           collapse="<br>", sep=""))
      updateActionButton(session, "queryConcepts", HTML("query &nbsp;&nbsp; <font color='white'><i>NEEDS REFRESH!</i></font>"))

    }, ignoreInit=T)

    ##########################################################################################################
    # Clear selected concepts event
    ##########################################################################################################

    observeEvent(input$conceptSelectClear,{

      queryConcept <<- data.frame("ID"=character(), "FSN"=character())
      output$queryConceptFSN <- renderText("")
      updateActionButton(session, "queryConcepts", HTML("query"))

    }, ignoreInit=T)

    ##########################################################################################################
    # Query concept ID and FSN update action
    # Label query button with "query required" flag
    ##########################################################################################################

    observeEvent(c(input$queryConceptID, input$queryFSNKeyword), {

      updateActionButton(session, "queryConcepts", HTML("query &nbsp;&nbsp; <font color='white'><i>NEEDS REFRESH!</i></font>"))

    }, ignoreInit=T)

    ##########################################################################################################
    # Query concept action
    ##########################################################################################################

    observeEvent(input$queryConcepts,{

      # Motor Dysfunction (path Clinical finding, Clinical history, Finding of movement)
      # Tremor (path path Clinical finding, Clinical history, Finding of movement, Involuntary movement)
      #queryConcept <- data.frame("ID"=c("52559000", "26079004"), FSN="")

      # Motor dysfunction and anxiety
      #queryConcept <- data.frame("ID"=c("52559000", "48694002", FSN=""))

      # Motor dysfunction and developmental mental disorder
      #queryConcept <- data.frame("ID"=c("52559000", "129104009", FSN=""))

      # Anxiety disorder, mood disorder
      #queryConcept <- data.frame("ID"=c("197480006", "46206005", FSN=""))

      # Use explicitly supplied concept IDs, if present
      # Omit spaces and parse using semi-colon delimiters
      if(nchar(input$queryConceptID)>0)
        queryConcept <- data.frame("ID"=strsplit(gsub(" ", "", input$queryConceptID), ";")[[1]], FSN="")

      # Execute only if concepts selected or search key word(s) supplied 
      if(nrow(queryConcept)>0 | nchar(input$queryFSNKeyword)>0) {
        # Initialize graph configuration stack
        # Note that this event always establishes a new graph environment
        # Only one of ID or keyword filters are applied, ID takes precedence
        if(nrow(queryConcept)>0) {
          graphCfgOp("op"="init", query=list("concept"=list("style"="ID", "values"=queryConcept[,"ID"],
                                             "op"="", "conceptOrder"=1:nrow(queryConcept))))
        } else {
          # Parse semi-colon delimited search strings
          # Omit double spaces and spaces after semicolons
          a <- input$queryFSNKeyword
          while(regexpr("; ", a)[[1]]>0 | regexpr("  ", a)[[1]]>0)
            a <- gsub("; ", ";", gsub("  ", " ", a))
          # Parse filter values
          b <- strsplit(a, ";")[[1]]
          graphCfgOp("op"="init", query=list("concept"=list("style"=input$queryFSNKeywordStyle,
                                                            "values"=b,
                                                            "op"=input$queryFSNKeywordOp,
                                                            "conceptOrder"=1:length(b))))
        }

        # Query observations using the currently selected concept
        # Note the placement of results into a global data frame, since various user triggered actions
        # utilize previously queried data
        netData <<- queryNetworkData()

        if(class(netData)=="data.frame") {
          if(nrow(netData)>0) {
            obsExist <- T
          } else {
            obsExist <- F
          }
        } else {
          obsExist <- F
        }

        if(obsExist) {

          netComponents <<- assembleNetworkComponents()
          if(nrow(netComponents[["vertex"]])>0) {

            # Net regen is always done with physics enabled, but we want it to be disabled after regen
            # Direct disabling of physics (using visPhysics(enabled=F)) has no effect when called immediately after
            # renderVisNetwork(), but is effective when executed from within a shiny reactive function
            # So, although not ideal, force disable of physics by toggling the reaction control with physics par val
            updateRadioButtons(session, "physics", selected=T)
            output$g1 <- renderVisNetwork(composeNetwork(netComponents, input$renderGeometry, input$renderScaleX, input$renderScaleY))
     
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
          showNotification("No data exist for specified SNOMED CT concept(s) or related covariates", type="error")

        }

      } else {

        netData <<- data.frame()
        output$g1 <- NULL
        #output$gTable <- NULL

      }

      updateActionButton(session, "queryConcepts", label="query")

    }, ignoreInit=T)      

    ##########################################################################################################
    # Maintain graph configuration stack
    # Initialize, push/remove current configuration onto/from stack
    # Record currently selected concept IDs, participant var filter values, prescription filters, etc.
    # Note the global declaration to that the function accessible from outside the shiynyServer() env
    ##########################################################################################################

    graphCfgOp <<- function(op, query=NULL, filter=NULL, filterMode=NULL, vcfg=NULL) {

      # Parameters:
      # op ........... "init" ..... initialize graph configuration stack (one element with curr cfg)
      #                "add" ...... push the current on-screen configuration onto the graph configuration
      #                             stack and advance pointer
      #                "upd" ...... update configuration with current on-screen values
      #                "remove" ... remove the current configuration and retreat stack pointer
      #                "vcfg" ..... update the vcfg element of graphCfg
      # query ........ List of query instruction sets, one set for each variable to be treated
      #                See notes in the queryNetworkData() function for an explanation
      # filter ....... List of variable value filters, one for each variable to be filtered
      #                For primary variables (non-joined concepts and non-interactions),
      #                element names correspond to the database variable to filter
      #                For primary variables, each element is a vector of values to include
      #                For joint-concepts and interactions, each element is list with each element
      #                containing a vector, where each vector specifies a set of database values
      #                to be included (vertices to be created for)
      #                Joint-concept vectors need not be named (all correspond to concepts)
      #                Interaction filter vector names positionally correspond to database
      #                variables to be evaluated (data column with name = pos i of filter vector
      #                names is filtered by value in pos i of filter vector 
      # filterMode ... "filter", "expand", "nbhood1" as used in assembleNetworkComponents()
      # vcfg ......... Data frame of vertex configuration values, a modified version of the global
      #                vcfg data frame that reflects current data set and appearance configuration
      #                These values generally modify defaults and account for additional variables
      #                (such as for interactions) and are used to construct and render the current graph
      #                It is assumed that gcPtr > 0

      if(op %in% c("init", "add", "upd")) {

        # Use current values for elements common to init, add, upd operations
        gcfg <- list("query"=query,
                     "filter"=filter,
                     "filterMode"=filterMode,
                     "rxLeadCharFilter"=tolower(gsub(" ", "", input$rxLeadCharFilter)),
                     "connect"=list("UCDProxDist"=input$cnUCDProxDist,
                                    "Sex"=input$cnSex,
                                    "UCDDx"=input$cnUCDDx,
                                    "onsetAgeDays"=input$cnAge,
                                    "HASxLast"=input$cnHASxLast,
                                    "groupHA"=input$groupHA,
                                    "conceptID"=input$cnConceptID,
                                    "joinConcept"=input$joinConcept,
                                    "rxID"=input$cnRx,
                                    "rxSubsume"=input$rxSubsume,
                                    "findingSiteID"=input$cnFindingSite),
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
                     "nearestHighlightDeg"=input$nearestHighlightDeg,
                     "vcfg"=vcfg)

        # Assign elements specific to requested operation
        if("op"=="init" | gcPtr<1) {
          graphCfg <<- list()
          gcPtr <<- 1
        } else if(op %in% c("add", "upd") & gcPtr>0) {
          # Copy query and filters from current cfg if unspecified
          if(is.null(query))
            gcfg[["query"]] <- graphCfg[[gcPtr]][["query"]]
          if(is.null(filter))
            gcfg[["filter"]] <- graphCfg[[gcPtr]][["filter"]]
          if(is.null(filterMode))
            gcfg[["filterMode"]] <- graphCfg[[gcPtr]][["filterMode"]]
          if(op=="add")
            gcPtr <<- gcPtr+1
        }

        # Save new configuration
        if(gcPtr>0)
          graphCfg[[gcPtr]] <<- gcfg

      } else if(op=="remove" & gcPtr>1) {

        # Remove current cfg from stack (truncate stack)
        gcPtr <<- gcPtr-1
        graphCfg <<- graphCfg[1:gcPtr]
        # Restore screen values
        # Suggestion:  isolate to avoid triggering reactive events
        # Connect vars (checkbox groups)
        v <- data.frame("cfg"=c("UCDProxDist", "Sex", "UCDDx", "osetAgeDays", "HASxLast", "conceptFSN",
                                "Rx", "findingSiteFSN"),
                        "scr"=c("cnUCDProxDist", "cnSex", "cnUCDDx", "cnAge", "cnHASxLast", "cnConceptFSN",
                                "cnRx", "cnFindingSite"))
        for(i in 1:nrow(v))
          if(!is.null(graphCfg[[gcPtr]][["connect"]][[v[i,"cfg"]]])) {
            updateCheckboxGroupInput(session, v[i,"scr"], selected=graphCfg[[gcPtr]][["connect"]][[v[i,"cfg"]]])
          } else {
            updateCheckboxGroupInput(session, v[i,"scr"], selected=character())
          }
        # Connect vars (checkboxes)
        v <- data.frame("cfg"=c("rxSubsume", "groupHA", "joinConcept"), "scr"=c("rxSubsume", "groupHA", "joinConcept"))
        for(i in 1:nrow(v))
          if(!is.null(graphCfg[[gcPtr]][["connect"]][[v[i,"cfg"]]])) {
            updateCheckboxInput(session, v[i,"scr"], value=graphCfg[[gcPtr]][["connect"]][[v[i,"cfg"]]])
          } else {
            updateCheckboxInput(session, v[i,"scr"], value=F)
          }
        # Interactions
        v <- data.frame("cfg"=c("set1", "conn1", "set2", "conn2"),
                        "scr"=c("interactSet1", "interactConn1", "interactSet2", "interactConn2"))
        for(i in 1:nrow(v))
          if(!is.null(graphCfg[[gcPtr]][["interact"]][[v[i,"cfg"]]])) {
            updateCheckboxGroupInput(session, v[i,"scr"], selected=graphCfg[[gcPtr]][["interact"]][[v[i,"cfg"]]])
          } else {
            updateCheckboxGroupInput(session, v[i,"scr"], selected=character())
          }
        # Sliders
        v <- data.frame("cfg"=c("nedgemin", "eopacity", "vmassf", "vsizefactor", "vfontsz", "nCluster",
                                "nearestHighlightDeg", "renderScaleX", "renderScaleY"),
                        "scr"=c("nedgemin", "eopacity", "vMassFactor", "vSizeFactor", "vFontSize", "nCluster",
                                "nearestHighlightDeg", "renderScaleX", "renderScaleY"))
        for(i in 1:nrow(v))
          if(!is.null(graphCfg[[gcPtr]][[v[i,"cfg"]]]))
            updateSliderInput(session, v[i,"scr"], value=graphCfg[[gcPtr]][[v[i,"cfg"]]])
        # Radio buttons
        v <- data.frame("cfg"=c("physics", "renderGeometry"), "scr"=c("physics", "renderGeometry"))
        for(i in 1:nrow(v))
          if(!is.null(graphCfg[[gcPtr]][[v[i,"cfg"]]]))
            updateRadioButtons(session, v[i,"scr"], selected=graphCfg[[gcPtr]][[v[i,"cfg"]]])
        # Text fields
        if(nchar(graphCfg[[gcPtr]][["rxLeadCharFilter"]])>0) {
          updateTextInput(session, "rxLeadCharFilter", value=graphCfg[[gcPtr]][["rxLeadCharFilter"]])
        } else {
          updateTextInput(session, "rxLeadCharFilter", value=NA)
        }

      } else if(op=="vcfg" & gcPtr>0) {

        # Save the supplied vcfg data frame in the current cfg
        graphCfg[[gcPtr]][["vcfg"]] <<- vcfg

      }

      #print("graphCfgOp.op")
      #print(op)
      #print("graphCfgOp.query")
      #print(graphCfg[[gcPtr]][["query"]])
      #print("graphCfgOp.filter")
      #print(graphCfg[[gcPtr]][["filter"]])
      #print("graphCfgOp.filterMode")
      #print(graphCfg[[gcPtr]][["filterMode"]])

    }

    ##########################################################################################################
    # Record reactive values prior to update
    # These are useful in evaluating current with prior values 
    ##########################################################################################################

    onFlush(session=session, once=FALSE,
      function() {
        isolate(reactVal[["interactSet1"]] <<- input$interactSet1)
        isolate(reactVal[["interactConn1"]] <<- input$interactConn1)
        isolate(reactVal[["interactSet2"]] <<- input$interactSet2)
        isolate(reactVal[["interactConn2"]] <<- input$interactConn2)
      }
    )  

    ##########################################################################################################
    # Action triggered by a change in controls that require graph regeneration
    ##########################################################################################################

    observeEvent(c(input$cnUCDProxDist, input$cnSex, input$cnUCDDx, input$cnAge,
                   input$cnHASxLast, input$groupHA, input$cnConceptID, input$joinConcept,
                   input$cnRx, input$rxSubsume, input$cnFindingSite, input$nedgemin,
                   input$eopacity, input$vMassFactor, input$vSizeFactor, input$vFontSize,
                   input$nCluster, input$nearestHighlightDeg), {

      if(exists("netData"))
        if(nrow(netData)>0) {
          # Modify current graph cfg
          graphCfgOp(op="upd")
          netComponents <<- assembleNetworkComponents()
          if(nrow(netComponents[["vertex"]])>0) {
            updateRadioButtons(session, "physics", selected=T)
            output$g1 <- renderVisNetwork(composeNetwork(netComponents, input$renderGeometry, input$renderScaleX, input$renderScaleY))
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

    ##########################################################################################################
    # Action triggered by a change in FSN path finder field
    ##########################################################################################################

    observeEvent(input$conceptFinderText, {
      output$conceptFinderPath <- renderText(paste(queryPath(input$conceptFinderText, ""), collapse="<br><br>", sep=""))
    }, ignoreInit=T)

    ##########################################################################################################
    # Interaction cfg trigger
    # These are evaluated independently from remaining controls because multiple items in the group must be
    # selected (variables in set and variables to be connected to)
    ##########################################################################################################

    observeEvent(c(input$interactSet1, input$interactConn1, input$interactSet2, input$interactConn2), {
      if(exists("netData")) {
        # Execute if sufficient number of variables specified either in current reactive values or prior to
        # a change in any of the interaction cfg values (so that, if interactions were included prior to change,
        # but are not to be included at present, then they will be removed and will not be included again until
        # a sufficient number of variables is spepcified)
        # Note that list elements are omitted when an attempt to assign NULL is made (prior value is NULL)
        # However, reference to nonexistent list elements (by name) return NULL and the length of a NULL vector is 0
        if(nrow(netData)>0 & (length(input$interactSet1)>1 & length(input$interactConn1)>0 |
                              length(input$interactSet2)>1 & length(input$interactConn2)>0 |
                              length(reactVal[["interactSet1"]])>1 & length(reactVal[["interactConn1"]])>0 |
                              length(reactVal[["interactSet2"]])>1 & length(reactVal[["interactConn2"]])>0)) {
          # Modify current graph cfg
          graphCfgOp(op="upd")
          netComponents <<- assembleNetworkComponents()
          if(nrow(netComponents[["vertex"]])>0) {
            updateRadioButtons(session, "physics", selected=T)
            output$g1 <- renderVisNetwork(composeNetwork(netComponents, input$renderGeometry, input$renderScaleX, input$renderScaleY))
            #output$gTable <- DT::renderDataTable(composeGraphTable())
            updateRadioButtons(session=session, inputId="physics", selected=F)
          } else {
            output$g1 <- NULL
            #output$gTable <- NULL
          }
        }
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Render graph with specified geometry and scale
    ##########################################################################################################

    observeEvent(c(input$renderGeometry, input$renderScaleX, input$renderScaleY), {
      print("renderGeometry")
      updateRadioButtons(session, "physics", selected=T)
      output$g1 <- renderVisNetwork(composeNetwork(netComponents, input$renderGeometry, input$renderScaleX, input$renderScaleY))
      updateRadioButtons(session, "physics", selected=F)
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
        visUpdateNodes(visNetworkProxy("g1"), data.frame("id"=netComponents[["vertex"]][,"id"], "fixed"=F))
        output$g1 <- renderVisNetwork(composeNetwork(netComponents, input$renderGeometry, input$renderScaleX, input$renderScaleY))
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
    # Include or remove node from set of current selected nodes
    ##########################################################################################################

    observeEvent(input$shiftClick, {
      #print("shiftClick")
      # Identify selected vertex (value of id column in vertex data frame)
      v <- input$shiftClick[["nodes"]]
      if(length(v)>0) {
        v <- v[[1]][1]
        print(paste("vertex shift-selected:  ", v, sep=""))
        # Determine whether node is to be included or removed from selection set
        k <- which(shiftClickNode[,"nodeID"]==v)
        if(length(k)>0) {
          # Node exists in selection set, so restore color then remove it
          visUpdateNodes(visNetworkProxy("g1"), nodes=data.frame("id"=v[k], "color"=shiftClickNode[k,"color"]))
          shiftClickNode <<- shiftClickNode[-k,]
        } else {
          # Record node ID and current color
          shiftClickNode[nrow(shiftClickNode)+1,] <<- c(v, netComponents[["vertex"]][match(v, netComponents[["vertex"]][,"id"]),"color"])
          # Apply selection color to node
          visUpdateNodes(visNetworkProxy("g1"), nodes=data.frame("id"=v, "color"=shiftClickColor))
        }
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Vertex alt-click event
    # Triggered by java script contained in the click element of the visEvents parameter of the graph
    # rendered by composeNetwork()
    ##########################################################################################################

    observeEvent(input$altClick, {
      #print("altClick")
      # Identify selected vertex
      v <- input$altClick[["nodes"]]
      if(length(v)>0)
        print(paste("vertex alt-selected:  ", v[[1]][1], sep=""))
    }, ignoreInit=T)

    ##########################################################################################################
    # Return to previous graph
    # Decrement graph configuration stack pointer, truncate stack, and regenerate graph using cfg at new pointer
    ##########################################################################################################

    observeEvent(input$nodeRestorePrevious, {
      print("graphRestorePrevious")
      if(gcPtr>1) {
        # Retain current filter value to trigger requery after graph cfg restore, if needed
        filterMode0 <- graphCfg[[gcPtr]][["filterMode"]]
        graphCfgOp("remove")
        # Requery if filter mode was "expand," since subsetting may have occurred
        if(filterMode0=="expand")
          netData <<- queryNetworkData()
        if(nrow(netData)>0) {
          netComponents <<- assembleNetworkComponents()
          if(nrow(netComponents[["vertex"]])>0) {
            updateRadioButtons(session, "physics", selected=T)
            output$g1 <- renderVisNetwork(composeNetwork(netComponents, input$renderGeometry, input$renderScaleX, input$renderScaleY))
            #output$gTable <- DT::renderDataTable(composeGraphTable())
            updateRadioButtons(session=session, inputId="physics", selected=F)
          } else {
            output$g1 <- NULL
            #output$gTable <- NULL
          }
        } else {
          output$g1 <- NULL
          #output$gTable <- NULL
        }
      # Clear selected node indices
      shiftClickNode <<- shiftClickNode[F,]
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Create a vertex filter list for subsetting
    # Result is a list formatted for use by assembleNetwork() to limit graph to specified nodes (specific
    # levels of variables to be rendered)
    ##########################################################################################################

    subsetNodeFilterCompose <- function(nodeID) {

      # Parameters:
      # nodeID ........... Vector of node IDs (from netComponents[["vertex"]]) to be treated

      if(length(nodeID)>0) {
        filter <- list()
        # Identify vertex data rows for specified nodes
        kvx <- match(nodeID, netComponents[["vertex"]][,"id"])
        # Generate primary variable filter
        # Identify variables in primary class
        kc <- kvx[which(netComponents[["vertex"]][kvx,"varClass"]=="primary")]
        if(length(kc)>0) {
          # Compose sets of vertices by database variable
          # Database variables appear in the names attribute of the vertex data vectors
          # vertexDataValue elements positionally correspond to rows in the vertex data frame (i<->i)
          ivar <- split(kc, unlist(lapply(kc, function(kv) names(netComponents[["vertexDataValue"]][[kv]]))))
          filter[["primary"]] <- lapply(1:length(ivar),
                                   function(i)
                                     # Retrieve database values for current variable
                                     unlist(lapply(ivar[[i]], function(j) netComponents[["vertexDataValue"]][j])))
          names(filter[["primary"]]) <- names(ivar)
          # Retain current filter specifications for variables not represented in specified nodes
          if(!is.null(graphCfg[[gcPtr]][["filter"]])) {
            # Index current filter elements not in specified node variables
            k4 <- which(!names(graphCfg[[gcPtr]][["filter"]][["primary"]]) %in% names(filter[["primary"]]))
            if(length(k4)>0)
              filter[["primary"]] <- c(filter[["primary"]], graphCfg[[gcPtr]][["filter"]][["primary"]][k])
          }
        }
        # Generate joint concept filter
        # Identify variables in jointConcept class
        kc <- kvx[which(netComponents[["vertex"]][kvx,"varClass"]=="jointConcept")]
        if(length(kc)>0) {
          # Copy vectors of joint-concept IDs from vertex data to jointConcept filter element
          filter[["jointConcept"]] <- lapply(kc, function(i) netComponents[["vertexDataValue"]][[i]])
        }
        # Generate interaction filter
        # Identify variables in interaction class
        kc <- kvx[which(netComponents[["vertex"]][kvx,"varClass"]=="interaction")]
        if(length(kc)>0) {
          # Copy vectors of interaction data values from vertex data to jointConcept filter element
          filter[["interaction"]] <- lapply(kc, function(i) netComponents[["vertexDataValue"]][[i]])
        }
        filter
      } else {
        NULL
      }
    }

    ##########################################################################################################
    # Filter nodes
    # Render new graph limited to selected nodes
    ##########################################################################################################

    filterNodes <- function() {
      # Push a new graph cfg with additional filter for selected nodes
      # Use current query cfg for reference, but do not requery
      # The current Rx filter is saved by graphCfgOp()
      graphCfgOp(op="add",
                 query=graphCfg[[gcPtr]][["query"]],
                 # Include filter list
                 filter=subsetNodeFilterCompose(shiftClickNode[,"nodeID"]),
                 filterMode="filter")
      # Render graph
      netComponents <<- assembleNetworkComponents()
      if(nrow(netComponents[["vertex"]])>0) {
        updateRadioButtons(session, "physics", selected=T)
        output$g1 <- renderVisNetwork(composeNetwork(netComponents, input$renderGeometry, input$renderScaleX, input$renderScaleY))
        #output$gTable <- DT::renderDataTable(composeGraphTable())
        #updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
        updateRadioButtons(session=session, inputId="physics", selected=F)
      } else {
        output$g1 <- NULL
        #output$gTable <- NULL
      }
    }

    ##########################################################################################################
    # Node subnet event
    # Verify that vertices have been selected (shiftClickNode non-empty) or if Rx char filter has changed
    # Render new graph limited to selected nodes
    ##########################################################################################################

    observeEvent(input$nodeSubnet, {
      print(paste("node subnet:  ", paste(shiftClickNode[,"nodeID"], collapse=", ", sep=""), sep=""))
      # shiftClickNode data frame contains IDs (row positions in vertex DF) of selected nodes
      if(nrow(shiftClickNode)>0 | input$rxLeadCharFilter!=graphCfg[[gcPtr]][["rxLeadCharFilter"]]) {
        # Filter nodes by selection or leading Rx characters
        # Note that the current Rx filter is saved as a function of filterNodes()
        filterNodes()
        # Clear selected node indices
        shiftClickNode <<- shiftClickNode[F,]
      }
    }, ignoreInit=T)

    ##########################################################################################################
    # Node neighborhood subnet event
    # Verify that vertices have been selected (shiftClickNode non-empty) or new Rx filter specified
    # Render new graph limited to selected nodes along with nodes with an edge to selected nodes
    ##########################################################################################################

    observeEvent(input$nodeNeighborhood1, {

      print(paste("node neighborhood1:  ", paste(shiftClickNode[,"nodeID"], collapse=", ", sep=""), sep=""))
      # shiftClickNode data frame contains IDs (row positions in vertex DF) of selected nodes
      if(nrow(shiftClickNode)>0 | input$rxLeadCharFilter!=graphCfg[[gcPtr]][["rxLeadCharFilter"]]) {
        # Filter nodes by selection or leading Rx characters
        # Note that the current Rx filter is saved as a function of filterNodes()
        filterNodes()
        # Clear selected node indices
        shiftClickNode <<- shiftClickNode[F,]
      }

    }, ignoreInit=T)

    ##########################################################################################################
    # Node expand event
    # Verify that vertices have been selected (shiftClickNode non-empty) or if Rx lead char filter has changed
    # Generate new graph using children of selected nodes, if any, otherwise the selected nodes
    # Advance graph configuration stack then assemble and compose graph using current cfg
    ##########################################################################################################

    observeEvent(input$nodeExpand, {

      print(paste("node expand:  ", paste(shiftClickNode[,"nodeID"], collapse=", ", sep=""), sep=""))

      # Construct vectors of DB values for querying (concepts) and filtering (non-concept variables)
      # At present, non-concepts are non-hierarchical, so there are no sub-levels in which to descend and expand
      # If concept(s) selected and (concepts not being joined or only one concept specified)
      #   Query sub-nodes of specified concepts
      #   Filter using non-concept specified nodes
      # Else
      #   Filter using all specified nodes
      # Filter by Rx leading chars, if Rx filter different from filter in current current graph cfg
      if(nrow(shiftClickNode)>0) {
        # Retrieve vertex row indices for selected nodes
        knode <- match(shiftClickNode[,"nodeID"], netComponents[["vertex"]][,"id"])
        # Test for conceptID as a primary variable (prompts for expansion, not filtering)
        kcp <- intersect(knode[which(netComponents[["vertex"]][knode,"varClass"]=="primary")],
                         unlist(lapply(knode,
                                       function(i)
                                         if(names(netComponents[["vertexDataValue"]][[i]])[1]=="conceptID") {
                                           i
                                         } else {
                                           NA
                                         })))
        if(length(kcp)>0) {
          # Concept(s) selected and conceptID is a primary, not joined, variable
          # Compose query instruction for selected concepts, so that a graph of chidren concepts produced
          # Include concept order from previous query (from the parents of selected nodes) 
          # This will maintain continuity of concept orders (numbers in parentheses preceding concept descriptions),
          # so that as concept nodes are subset into children sets, their order corresponds to the initial
          # concepts selected (using the on-screen concept selection fields)
          query <- list("concept"=list("style"="ID",
                                       "values"=unlist(lapply(kcp, function(i) netComponents[["vertexDataValue"]][[kcp]])),
                                       "op"="",
                                       "conceptOrder"=netComponents[["vertex"]][kcp,"conceptOrder"]))
          # Compose filter for non-concept variables
          filter <- subsetNodeFilterCompose(shiftClickNode[,"nodeID"])
          # Eliminate concepts from primary variable filter, since the query will limit concepts
          filter[["primary"]] <- filter[["primary"]][which(names(filter[["primary"]])!="conceptID")]
          filterMode <- "expand"
        } else {
          # Concepts not selected or they are joined
          # Retain current query for reference (nodes are not expanded, so query is not performed)
          # Compose filter for joined concept and non-concept variables        
          query <- graphCfg[[gcPtr]][["query"]]
          filter <- subsetNodeFilterCompose(shiftClickNode[,"nodeID"])
          filterMode <- "filter"
        }

        # Push a new graph cfg with query and filter configuration
        # Note that the leading Rx text filter is saved during graphCfgOp()
        graphCfgOp(op="add", query=query, filter=filter, filterMode=filterMode)
        # Requery, if expanding concept nodes
        if(filterMode=="expand") {
          netData <<- queryNetworkData()
          # Verify existence of subnet node data
          if(class(netData)=="data.frame") {
            if(nrow(netData)==0)
              showNotification("No data exist for specified SNOMED CT concept(s) or related covariates", type="error")
          } else {
            netData <<- data.frame()
            showNotification("No data exist for specified SNOMED CT concept(s) or related covariates", type="error")
          }
        }
        # Filter nodes and edges then render graph
        netComponents <<- assembleNetworkComponents()
        if(nrow(netComponents[["vertex"]])>0) {
          updateRadioButtons(session, "physics", selected=T)
          output$g1 <- renderVisNetwork(composeNetwork(netComponents, input$renderGeometry, input$renderScaleX, input$renderScaleY))
          #output$gTable <- DT::renderDataTable(composeGraphTable())
          #updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
          updateRadioButtons(session=session, inputId="physics", selected=F)
        } else {
          output$g1 <- NULL
          #output$gTable <- NULL
        }
      } else if(input$rxLeadCharFilter!=graphCfg[[gcPtr]][["rxLeadCharFilter"]]) {
        # Filter nodes by leading Rx characters
        # Note that the current Rx filter is saved by filterNodes()
        # The filtered graph is also rendered by filterNodes()
        filterNodes()
      }
      # Clear selected node indices
      shiftClickNode <<- shiftClickNode[F,]

    }, ignoreInit=T)

    ##########################################################################################################
    # Execution begins here
    ##########################################################################################################

    # Query SNOMED root node
    # Initialize pointer to current concept in stack
    csPtr <- 1
    exploreConceptStack[csPtr,] <- queryISAConcept(0)[1,]
    output$exploreCurrRootPath <- renderText(HTML(paste("<font color=blue>", exploreConceptStack[csPtr,"FSN"], "</font>", sep="")))

    # Retrieve all nodes leading to the root concept node by ISA relationships
    exploreRootMbrs <- queryISAConcept(exploreConceptStack[csPtr,"sctid"])

    # Update concept selection list with current value of NA to avoid triggering an immediate update event
    updateSelectInput(session, "exploreChoices", choices=exploreRootMbrs[,"FSN"], selected=NA)

    # Create empty data frame to contain concept IDs and descriptions for concepts to be queried
    queryConcept <- data.frame("ID"=character(), "FSN"=character())

  }
)