require(networkD3)


#' Function to extract the article from the Project Gutenberg page
#' 
#' @param no.of.words Number of words from each topic to be included in graph
#' @param topic.words Words words extracted from the topic_table function
#' 
#' @return network The graph visualising the network
#' @examples
#' gepi_network(20, topic.words)


gepi_network<- function(no.of.words,topic.words){
  topic_names <- paste("Topic_",1:dim(topic.words)[1], sep = "")
  row.names(topic.words) <- topic_names
  frequent_words <- sapply(topic_names, function(x) 
    list(sort(topic.words[x,], decreasing = T)[1:no.of.words]))
  topic_words <- sapply(frequent_words, names)
  topic_word_values <- sapply(frequent_words, as.numeric)
  
  word_list <- unique(as.vector(topic_words))
  
  names <- c(topic_names, word_list) 
  groups <- c(rep(20, length(topic_names)), 
              rep(1, length(word_list)))
  size <- c(rep(3,length(topic_names)),
            rep(1, length(word_list)))
  
  Nodes <- data.frame(name = names,
                      group = groups, 
                      size = size)
  
  from <- sapply(topic_words, function(x) which(Nodes == x)) - 1
  to <- rep(0:(length(topic_names)-1), each = no.of.words) 
  value <- as.vector(topic_word_values)
  value <- value *20/max(value)
  Links <- data.frame(source = from,
                      target = to,
                      value = value)
  network <-forceNetwork(Links = Links, Nodes = Nodes,
                         Source = "source", Target = "target",
                         Value = "value", NodeID = "name",
                         Group = "group",zoom = TRUE, Nodesize = "group", 
                         opacity = 0.9)
  network
}
x <- gepi_network(20, topic.words)