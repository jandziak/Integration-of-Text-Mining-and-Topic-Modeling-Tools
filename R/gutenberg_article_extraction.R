#' Function to extract the article from the Project Gutenberg page
#' 
#' @param url A complite path to the text file of the selected to download book.
#' @param full_text Logical parameter deretmining wheather to exclude disclaimer or not. 
#' 
#' @return book_string The string of the book content. (With or without disclaimer)
#' @examples
#' extract_article("http://www.gutenberg.org/cache/epub/11503/pg11503.txt", TRUE)
#' extract_article("http://www.gutenberg.org/files/51428/51428-0.txt", FALSE)

extract_article <- function(url, full_text = TRUE){
  
  book <- readLines(url, encoding = "UTF-8")
  if(!full_text){
    disclaimer <- c("\\*\\*\\* START OF THIS PROJECT GUTENBERG",  
                    "\\*\\*\\* END OF THIS PROJECT GUTENBERG")
    matches <- grepl(paste(disclaimer,collapse="|"), book)
    matches_index <- which(matches)
    book <- book[min(matches_index):max(matches_index)]
  }
  book_string <- paste(book, collapse = " ")
  return(book_string)
}
