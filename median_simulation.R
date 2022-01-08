library(dplyr)
library(tidyr)
library(tidygraph)
library(ggraph)
library(igraph)
library(gridExtra)
library(gifski)
nodes = data.frame("nodes"=1:100)
graph = create_empty(n=0)%>%
  bind_nodes(nodes)
# create a baseline probability of a new connection
baseline_prob<-.0005
# create a matrix that will indicate pairwise probabilities of a new edge being drawn
prob_matrix<-matrix(baseline_prob,100,100)
# create a while loop for iterations
iteration<-0
max_iterations<-98
while(iteration<=max_iterations){
  iteration = iteration+1
  # create a matrix indicating new edges
  new_edge_matrix<-apply(prob_matrix,
                         c(1,2),
                         function(x) sample(c(0,1),size=1,prob=c(min(1-x,1),
                                                                 max(x,0))))
  # add new edges to graph
  new_edges<-which(new_edge_matrix==1,arr.ind=TRUE)%>%
    as.data.frame()%>%
    filter(row!=col)%>%
    select('to'='row','from'='col')%>%
    mutate(weight=1)
  graph<-graph%>%
    activate('edges')%>%
    bind_edges(new_edges)
  distance_matrix<-graph%>%
    distances()
  # update prob_matrix to increase probability based on distance
  prob_matrix<-prob_matrix+(1/(which(distance_matrix>0)))^3
  # max out prob_matrix at .99
  prob_matrix<-apply(prob_matrix,
                     c(1,2),
                     function(x) min(x,.5))
  summary(as.vector(prob_matrix))
  edges<-graph%>%
    activate(edges)%>%
    as_tibble()%>%
    unique()
  # count the number of edges per character, merge in the FROM's and TO's separately
  froms<-edges%>%
    select('node'='from','connection'='to')
  tos<-edges%>%
    select('node'='to','connection'='from')
  connections<-bind_rows(froms,tos)
  edges_per_node<-connections%>%
    group_by(node)%>%
    summarise('edge_count'=n())
  # create a function that can be applied over nodes and calculates the difference between that nodes connections and the other nodes
  count_comparison<-function(node_name){
    # retrieve the number of connections the node has
    node_connection_count <- edges_per_node%>%
      filter(node==node_name)%>%
      select('node','node_connection_count'=edge_count)
    # retrieve the nodes connected to the given node
    node_connections<-connections%>%
      filter(node==node_name)%>%
      select('connection')
    # count the number of connections for each node connected to the given node
    connection_connection_count<-edges_per_node%>%
      inner_join(node_connections,by=c('node'='connection'))%>%
      select('connection'='node','connection_connection_count'= 'edge_count')%>%
      mutate('node'=node_name)
    # merge node_connection_count with connection_connection_count by node
    result<-node_connection_count%>%
      inner_join(connection_connection_count,by='node')%>%
      filter(node!=connection)
    return(result)
  }
  # apply function over all nodes
  comparison_frame<-lapply(nodes$nodes, count_comparison)%>%
    bind_rows()
  
  diff_dist_frame<-comparison_frame%>%
    group_by(node)%>%
    summarise('node_connection_count' = median(node_connection_count),
              'connection_connection_count' = median(connection_connection_count))%>%
    ungroup()%>%
    mutate(diff=node_connection_count-connection_connection_count)
  diff_dist<-diff_dist_frame%>%
    ggplot(aes(x=diff))+
    geom_density(fill='lightblue',alpha=.5)+
    labs(x="Node connection-count minus median connection-count of node's connections",
         y= "Density of node distribution",
         title = "Distribution of differences between each node's connection-count and the\nmedian connection-count of connected nodes")+
    # force limits to be symmetrical
    xlim(min(diff_dist_frame$diff),0-min(diff_dist_frame$diff))
  
  diffs_for_merge<-comparison_frame%>%
    rename('nodes'='node')%>%
    bind_rows(nodes%>%anti_join(comparison_frame,by=c('nodes'='node')))%>%
    mutate(node_connection_count = replace_na(node_connection_count,0),
           connection_connection_count = replace_na(connection_connection_count,0),
           connection=replace_na(connection,0))%>%
    group_by(nodes)%>%
    summarise(node_connection_count = median(node_connection_count),
              connection_connection_count = median(connection_connection_count))%>%
    mutate(diff_str = case_when(node_connection_count==0~'unconnected node',
                                node_connection_count>connection_connection_count~'node more connected than its connections are',
                                node_connection_count==connection_connection_count~"node connections equal to connections' connections",
                                TRUE~'node less connected than its connections are'),
           diff_factor = factor(diff_str,levels = c('node more connected than its connections are',
                                                    "node connections equal to connections' connections",
                                                    'node less connected than its connections are',
                                                    'unconnected node'
           )))
  
  graph_image<-graph%>%
    activate(nodes)%>%
    left_join(diffs_for_merge,by='nodes')%>%
    ggraph(layout = 'circle') + 
    geom_edge_diagonal(color = "gray", alpha = 0.4)+
    geom_node_text(aes(label=nodes,color=diff_factor), size = 3) +
    scale_color_discrete(drop=FALSE)+
    theme_graph()+
    theme(legend.position='bottom',
          legend.title = element_blank())+
    guides(color=guide_legend(nrow=2,byrow=TRUE))
  p<-arrangeGrob(diff_dist,graph_image)
  # create a string of iteration to name the file
  if(iteration<10){
    iteration_str<-paste0('0',iteration)
  }else{
    iteration_str<-paste0(iteration)
  }
  ggsave(file=paste0('med_images/image ',iteration_str,'.png'),p)
}
png_files <- list.files("med_images/", pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = "100_iterations_median.gif", width = 800, height = 600, delay = 1)
