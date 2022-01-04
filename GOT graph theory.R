library(tidygraph)
library(ggraph)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
# read in game of thrones edge data
url <- "https://raw.githubusercontent.com/melaniewalsh/sample-social-network-datasets/master/sample-datasets/game-of-thrones/got-edges.csv"
thrones <- read_csv(url)

graph_thrones<-as_tbl_graph(thrones,directed=FALSE)

# get the character names to merge in with edge summary
node_names<-graph_thrones%>%
  activate(nodes)%>%
  as_tibble()%>%
  mutate(id = as.integer(rownames(.)))
edges<-graph_thrones%>%
  activate(edges)%>%
  as_tibble()%>%
  inner_join(node_names,by=c('from'='id'))%>%
  rename('from_name'='name')%>%
  inner_join(node_names,by=c('to'='id'))%>%
  rename('to_name'='name')
# count the number of edges per character, merge in the FROM's and TO's separately
froms<-graph_thrones%>%
  activate(edges)%>%
  as_tibble()%>%
  inner_join(node_names,by=c('from'='id'))%>%
  select(name,Weight)%>%
  arrange(name)
tos<-graph_thrones%>%
  activate(edges)%>%
  as_tibble()%>%
  inner_join(node_names,by=c('to'='id'))%>%
  select(name,Weight)%>%
  arrange(name)


edges_per_node<-bind_rows(froms,tos)%>%
  group_by(name)%>%
  summarise(edge_count = n())

edges_per_node%>%
  ggplot(aes(x=edge_count))+
  geom_density()+
  labs(x='Friends per character',y='Character Count',title = '# of Friends per GOT character')+
  theme_minimal()


# create a function that takes a character as an input, and returns a DF with character's friend count and the friend count of each of their friends
character <- "Arya"
count_comparison<-function(character){
  # retrieve the number of friends the character has
  character_friend_count <- edges_per_node%>%
    filter(name==character)%>%
    select('character_name'=name,'character_friend_count'=edge_count)
  # retrieve all friends of the character, separately from the from's and to's
  to_friends<-edges%>%
    filter(from_name==character)%>%
    select('name'='to_name')
  from_friends<-edges%>%
    filter(to_name==character)%>%
    select('name'='from_name')
  # combine from's and to's, merge in with edges per node
  character_friends<-bind_rows(from_friends,to_friends)%>%
    inner_join(edges_per_node)%>%
    select('friend_name'=name,'friend_friend_count'='edge_count')%>%
    mutate('character_name'=character)%>%
    inner_join(character_friend_count)%>%
    # calculate difference between character's count and friend count
    mutate(diff = character_friend_count-friend_friend_count)
  return(character_friends)
}

# apply function over all nodes
comparison_frame<-lapply(node_names$name, count_comparison)%>%
  bind_rows()

# find the average diff per character
comparison_frame%>%
  group_by(character_name)%>%
  summarise('character_friend_count' = mean(character_friend_count),
            'friend_friend_count' = mean(friend_friend_count))%>%
  ungroup()%>%
  mutate(diff=character_friend_count-friend_friend_count)%>%
  ggplot(aes(x=diff))+
  geom_histogram(bins=40)+
  labs(x="Diff between character friend-count and friend's friend-count",
       y= "Number of characters",
       title = "Distribution of differences between friend's count and friend's friend-count")

comparison_frame%>%
  group_by(character_name)%>%
  summarise('character_friend_count' = mean(character_friend_count),
            'friend_friend_count' = mean(friend_friend_count))%>%
  ungroup()%>%
  mutate(diff_factor = case_when(character_friend_count>friend_friend_count~'positive',
                                 TRUE~'negative'))%>%
  ggplot(aes(x=friend_friend_count, y=character_friend_count,color=diff_factor))+
  geom_text(aes(label=character_name))+
  geom_abline(slope=1,intercept=0)+
  xlim(c(0,40))+
  ylim(c(0,40))+
  theme(legend.position='none')+
  labs(y = "Character connection count",
       x = "Average connection's connection count",
       title = "GOT character - difference between character's connection count\n and average connection count of their connections")

# iterate over weight minimums to see if those affect th difference
comparison_frame%>%
  group_by(character_name)%>%
  summarise('character_friend_count' = mean(character_friend_count),
            'friend_friend_count' = mean(friend_friend_count))%>%
  ungroup()%>%
  mutate(diff=character_friend_count-friend_friend_count)

graph_thrones %>% 
  activate(edges)%>%
  filter(Weight>2)%>%
  activate(nodes)%>%
  filter(!node_is_isolated())%>%
  mutate(community = as.factor(group_fast_greedy(weights=Weight))) %>% 
  ggraph(layout = 'kk') + 
  geom_edge_diagonal(color = "gray", alpha = 0.4)+
  geom_node_text(aes(colour = community,label=name), size = 3) +
  theme_graph()+
  guides(color="none")
# create a version of comparison frame that includes the 0s
graph_thrones%>%
  activate(nodes)%>%
  as_tibble()
  