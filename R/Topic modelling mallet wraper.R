require(stylo)
require(mallet)
require(wordcloud)

filtered_texts <- tag_and_filter_texts("corpus_test", "verb")
prepared_data <- prepare_for_mallet(filtered_texts)
model <- train(prepared_data)
table_of_topics <- topic_table(model)
table_of_topics$topics
table_of_topics$words
topic_wordcloud(table_of_topics)

train <- function(text_array, names = NULL, 
                  stoplist_file = "en.txt",
                  token_regexp = "[A-za-z]+",
                  no_of_topics = 20,
                  alpha_opt = 20,
                  burn_in = 50,
                  train = 200,
                  maximize = 10){
  
  if(is.null(names(text_array)))
    names <- 1:langth(text_array)
  else 
    names <- names(text_array)
  mallet.instances = mallet.import(id.array = names, 
                                   text.array = text_array, 
                                   stoplist.file = stoplist_file,
                                   token.regexp = token_regexp)
  
  topic.model = MalletLDA(num.topics = no.of.topics)
  topic.model$loadDocuments(mallet.instances)
  
  vocabulary = topic.model$getVocabulary()
  word.freqs = mallet.word.freqs(topic.model)
  
  
  topic.model$setAlphaOptimization(alpha_opt, burn_in)
  
  topic.model$train(train)
  topic.model$maximize(maximize)
  
  topic.model
}



topic_table <- function(model){
  doc.topics = mallet.doc.topics(topic.model, smoothed=T, normalized=T)
  topic.words = mallet.topic.words(topic.model, smoothed=T, normalized=T)
  
  colnames(topic.words) = vocabulary
  rownames(doc.topics) = names(deparsed.corpus)
  colnames(doc.topics) = 1:length(doc.topics[1,])
  
  list(topics = doc.topics,
       words = topic.words)
  
}

topic_wordcloud<- function(topic_table, topic.id = 15, 
                           no.of.words = 100, rot.per = 0, 
                           random.order = FALSE){
  current.topic = sort(topic_table$words[topic.id,], decreasing = T)[1:no.of.words]
  wordcloud(names(current.topic),current.topic, 
                 random.order = random.order, 
                 rot.per = rot.per)
}
