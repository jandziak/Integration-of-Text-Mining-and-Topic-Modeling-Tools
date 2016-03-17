require(RWeka)
require(tau)
require(tm)
require(wordcloud)
require(mallet)
require(stylo)
require(SnowballC)

#' Function to clean and stem texts in the given directory. 
#' It returns data redy for modelling with the train function.
#' 
#' @param corpus_dir Direcroty of the corpus files.
#' @param sample.size Maximum size of document after sliceing.
#' @param wordlist Listo of words to be excluded from the analysis.
#' 
#' @return texts_vector Transformated and stemmed named text vector.
#' 
#' @examples
#' wordlist <- c("give", "get", "make", "do", "you", "yet", "me")
#' sample.size = 1000
#' corpus.dir = "./corpus"
#' cl_st_doc <- clean_and_stem_document(corpus.dir,1000,wordlist)
#' topic_model <- train(cl_st_doc)

clean_and_stem_document <- function(corpus_dir, sample.size, wordlist){
  parsed.corpus = load.corpus.and.parse(files = dir(), 
                                        corpus.dir = corpus_dir)
  sliced.corpus = make.samples(parsed.corpus, 
                               sampling = "normal.sampling", 
                               sample.size = sample.size)
  deparsed.corpus = sapply(sliced.corpus, paste, collapse = " ")
  
  corpus <- Corpus(VectorSource(deparsed.corpus))
  #dtm <- DocumentTermMatrix(corpus)
  
  ds0.1g <- tm_map(corpus, content_transformer(tolower))
  ds1.1g <- tm_map(ds0.1g, content_transformer(removeWords), wordlist)
  ds1.1g <- tm_map(ds1.1g, content_transformer(removeWords), 
                   stopwords("english"))
  ds2.1g <- tm_map(ds1.1g, stripWhitespace)
  ds3.1g <- tm_map(ds2.1g, removePunctuation)
  ds4.1g <- tm_map(ds3.1g, stemDocument)
  
  dataframe<-data.frame(text=unlist(sapply(ds4.1g, '[', "content")), 
                        stringsAsFactors=F)
  
  texts_vector <- as.character(dataframe[,1])
  names(texts_vector) <- names(deparsed.corpus) 
  
  texts_vector
}