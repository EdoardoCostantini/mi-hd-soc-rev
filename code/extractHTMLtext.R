# Project:   imputationSociology
# Objective: TODO
# Author:    Edoardo Costantini
# Created:   2022-03-04
# Modified:  2022-03-04

extractHTMLtext <- function(html_folder) {
  # Internals -------------------------------------------------------------

  # html_folder = "../input/html/689268.html"

  # Body ------------------------------------------------------------------
  # Source: https://www.r-bloggers.com/2011/10/reading-html-pages-in-r-for-text-processing/
    # Read and parse HTML file
    doc.html <- htmlTreeParse(html_folder, useInternal = TRUE)

    # Extract all the paragraphs (HTML tag is p, starting at the root of the document)
    doc.text <- xpathApply(doc.html, '//p', xmlValue)

    # Unlist flattens the list to create a character vector.
    doc.text <- unlist(doc.text)

    # Replace all \n by spaces
    doc.text <- gsub('\\n', ' ', doc.text)

    # Join all the elements of the character vector into a single
    # character string, separated by spaces
    doc.text <- paste(doc.text, collapse = ' ')

    # Return
    return(doc.text)
}
