
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
               visNetworkOutput('network1')),
        column(6,h4("Clustering based on Louvain method"),
               visNetworkOutput('network2'))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$network1 <- renderVisNetwork({
        graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="Duke123!")
        
        query = "
match (a:Participant) where a.UCDDx = 'OTC'

with a

match (a)-[:P_SCT]->(b:PNeuro)-[*..1]->(c:Neuro)

return a.ParticipantId, c.GLabel, c.FSN, a.Sex, a.LiverTransplantStatus limit 500

"
        edges = cypher(graph, query)
        
        
        ig = graph_from_data_frame(edges,directed=T)
        
        data <- toVisNetworkData(ig)
        visNetwork(nodes = data$nodes, edges = data$edges, height = "500px") %>%
            visIgraphLayout(layout = "layout_with_fr") %>% visNodes(size = 10)  %>% 
            visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T)  %>%
            visInteraction(navigationButtons = TRUE)
        

    })
    
    output$network2 <- renderVisNetwork({
        graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="Duke123!")
        
        query = "
match (a:Participant) where a.UCDDx = 'OTC'

with a

match (a)-[:P_SCT]->(b:PNeuro)-[*..1]->(c:Neuro)

return a.ParticipantId, c.GLabel, c.FSN, a.Sex, a.LiverTransplantStatus limit 500

"
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
