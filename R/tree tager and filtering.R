require(koRpus)
require(dplyr)

#' Configuration fille 
set.kRp.env(TT.cmd="C:\\TreeTagger\\bin\\tag-english.bat", lang="en", 
            preset="en", treetagger="manual", format="file", 
            TT.tknz=TRUE, encoding="UTF-8" )


#' Function to tag the text using tree tager
#' 
#' @param file_path Path to a file to be tagged with the tree tagger 
#' 
#' @return tagged_text The tagged text returned from tree tagger
#' @examples
#' tree_tag_file("corpus\\COM_ado_1599.txt")

tree_tag_file <-  function(file_path){
  tagged_text <- treetag(file_path, treetagger="manual",
                         lang="en", TT.options=list(path="C:\\TreeTagger", preset="en"))
  
  tagged_text
}


#' Function to tag all the texts in the given directory. It returns only filtered patrs of speach.
#' 
#' @param path Path to a directory with files to tag 
#' @param filter String parameter used to filter appropriate part of speach
#' 
#' @return filtered_texts The list of the strings corresponding to appropriate files
#' @return file_names The names of the filtered files
#' @examples
#' tag_and_filter_texts("corpus_test", "verb")

tag_and_filter_texts <- function(path, filter){
  names <- paste(path, "/", dir(path), sep = "")
  tests <- names %>% 
    lapply(tree_tag_file) %>% 
    lapply(taggedText) %>% 
    lapply(function(x) filter(x, wclass == filter)) %>%
    lapply(function(x) filter(x, lemma != "<unknown>"))
  filtered_texts <- tests %>% lapply(function(x) paste(x$lemma, collapse = " "))
  
  return(list(filtered_texts = filtered_texts, file_names = dir(path)))
}


#' Function to tag all the texts in the given directory. It returns only filtered patrs of speach.
#' 
#' @param filtered_texts Output of the tag_and_filter_texts function
#' 
#' @return filtered_texts_vector The named vector of the filtered texts
#' 
#' @examples
#' filtered_texts <- tag_and_filter_texts("corpus_test", "verb")
#' prepared_data <- prepare_for_mallet(filtered_texts)
#' prepared_data

prepare_for_mallet <- function(filtered_texts){
  filtered_texts_vector <- unlist(filtered_texts$filtered_texts)
  names(filtered_texts_vector) <- filtered_texts$file_names
  
  filtered_texts_vector
}

