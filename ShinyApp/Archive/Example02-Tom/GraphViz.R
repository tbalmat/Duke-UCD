
library(shiny)
library(RNeo4j)
library(visNetwork)
library(igraph)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("Neo4J Visualization"),

    fluidRow(
        column(6, h4("Layout with Kamada Kawai"),
               HTML("<br>"),
               radioButtons("UCDDx1", "UCDDx", choices=c("All", "null", "ALD", "ARG", "ASD", "CITR", "CPS1", "HHH", "NAGS", "OTC"), inline=T, selected="All"),
               HTML("<br>"),
               sliderInput("StartAgeDaysRange1", "Start Age Range (1,000 days)", min=0, max=30, step=1, value=c(0, 30)),
               HTML("<br>"),
               fluidRow(column(width=5, sliderInput("ObsLimit1", "Observation Return Limit (0 = All)", min=0, max=1000, step=20, value=100)),
                        column(width=7, radioButtons("OrderBy1", "Order By", choices=c("UCDDxAgeDays", "OnsetAgeDays", "RegisterAgeDays"), inline=T, selected="UCDDxAgeDays"))),
               HTML("<br>"),
               visNetworkOutput('network1', height="500px")),
        column(6,h4("Clustering based on Louvain method"),
               HTML("<br>"),
               radioButtons("UCDDx2", "UCDDx", choices=c("All", "null", "ALD", "ARG", "ASD", "CITR", "CPS1", "HHH", "NAGS", "OTC"), inline=T, selected="All"),
               HTML("<br>"),
               sliderInput("StartAgeDaysRange2", "Start Age Range (1,000 days)", min=0, max=30, step=1, value=c(0, 30)),
               HTML("<br>"),
               fluidRow(column(width=5, sliderInput("ObsLimit2", "Observation Return Limit (0 = All)", min=0, max=1000, step=20, value=100)),
                        column(width=7, radioButtons("OrderBy2", "Order By", choices=c("UCDDxAgeDays", "OnsetAgeDays", "RegisterAgeDays"), inline=T, selected="UCDDxAgeDays"))),
               HTML("<br>"),
               visNetworkOutput('network2', height="500px"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$network1 <- renderVisNetwork({
        graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="Duke123!")
        
        query = paste(" match (a:Participant)",
                      " where 1=1",
                      ifelse(input$UCDDx1!="All",
                        ifelse(input$UCDDx1!="null", paste(" and a.UCDDx='", input$UCDDx1, "'", sep=""), " and a.UCDDx is null"), ""),
                      " and toFloat(a.StartAgeDays)>=", input$StartAgeDaysRange1[1]*1000, 
                      " and toFloat(a.StartAgeDays)<=", input$StartAgeDaysRange1[2]*1000,
                      " with a",
                      " match (a)-[:P_SCT]->(b:PNeuro)-[*..1]->(c:Neuro)",
                      " return a.ParticipantId, c.GLabel, c.FSN, a.Sex, a.LiverTransplantStatus",
                      ifelse(input$ObsLimit1>0, paste(" order by a.", input$OrderBy1, " limit ", input$ObsLimit1, sep=""), ""), sep="")

        edges = cypher(graph, query)

        ig = graph_from_data_frame(edges,directed=T)
        
        data <- toVisNetworkData(ig)
        visNetwork(nodes = data$nodes, edges = data$edges) %>%
            visIgraphLayout(layout = "layout_with_fr") %>% visNodes(size = 10)  %>% 
            visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T)  %>%
            visInteraction(navigationButtons = TRUE)
        

    })
    
    output$network2 <- renderVisNetwork({
        graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="Duke123!")
        
        query = paste(" match (a:Participant)",
                      " where 1=1",
                      ifelse(input$UCDDx1!="All",
                             ifelse(input$UCDDx2!="null", paste(" and a.UCDDx='", input$UCDDx2, "'", sep=""), " and a.UCDDx is null"), ""),
                      " and toFloat(a.StartAgeDays)>=", input$StartAgeDaysRange1[1]*1000, 
                      " and toFloat(a.StartAgeDays)<=", input$StartAgeDaysRange1[2]*1000,
                      " with a",
                      " match (a)-[:P_SCT]->(b:PNeuro)-[*..1]->(c:Neuro)",
                      " return a.ParticipantId, c.GLabel, c.FSN, a.Sex, a.LiverTransplantStatus",
                      ifelse(input$ObsLimit2>0, paste(" order by a.", input$OrderBy2, " limit ", input$ObsLimit2, sep=""), ""), sep="")

        edges = cypher(graph, query)

        ig = graph_from_data_frame(edges,directed=F)
        
        data1 <- toVisNetworkData(ig)
        
        cluster <- cluster_louvain(ig)
        
        cluster_df <- data.frame(as.list(membership(cluster)))
        cluster_df <- as.data.frame(t(cluster_df))
        cluster_df$label <- substr(rownames(cluster_df), 2,7)
        
        #Create group column
        nodes <- left_join(data1$nodes, cluster_df, by = "label")
        colnames(nodes)[3] <- "group"
        
        visNetwork(nodes, data1$edges) %>% visNodes(size = 10)  %>% 
            visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T)  %>%
            visInteraction(navigationButtons = TRUE)
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
