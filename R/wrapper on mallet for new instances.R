#' Interfere the topics onto new instances. 
#' With a script from Andrew Goldstone it is possible and easy accesible to 
#' interfere topic probabilities for the new texts. This wrapper makes it more
#' accesible and compatibile with standard R interface. The structure is simmilar
#' to the one proposed in the caret package.

source("new_instance_agolds.R")

#' Function to tag the text using tree tager
#' 
#' @param corpus.dir Path to a corpus directory
#' 
#' @return deparsed.corpus Named vector of texts.
#' @examples
#' deparsed_corpus <- simple_load("corpus")
#' deparsed.corpus <- simple_load("C:\\corpus")

simple_load <- function(corpus.dir){
  parsed.corpus = load.corpus.and.parse(files = dir(), corpus.dir = corpus.dir)
  sliced.corpus = make.samples(parsed.corpus, sampling = "normal.sampling", 
                               sample.size = 1000)
  
  deparsed.corpus = sapply(sliced.corpus, paste, collapse = " ")
  deparsed.corpus
}



#' Function to tag the text using tree tager
#' 
#' @param new_texts Named vector of texts to be interfered.
#' @param topic.model Trained mallettopic model model.
#' 
#' @return ml_inst Data frame with the probabilities of belongings to particular 
#' topics
#' @examples
#' topic_probabilities <- predict(deparsed_corpus, topic.model)


predict <- function(new_texts, topic.model){
  mallet.instances = mallet.import(id.array = names(new_texts), 
                                   text.array = deparsed.corpus, 
                                   stoplist.file = "en.txt",
                                   token.regexp = "[A-za-z]+")
  
  comp_inst <- compatible_instances(names(new_texts), 
                                    new_texts, mallet.instances)
  inf <- inferencer(topic.model)
  inf_top <- infer_topics(inf, comp_inst,
                          n_iterations=100,
                          sampling_interval=10, # aka "thinning"
                          burn_in=10,
                          random_seed=NULL)
  
  ml_inst <- as.data.frame(inf_top)
  
  ml_inst
}

ml_inst1 <- predict(deparsed_corpus, topic.model)











