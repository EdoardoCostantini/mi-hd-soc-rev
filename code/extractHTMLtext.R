# Project:   imputationSociology
# Objective: TODO
# Author:    Edoardo Costantini
# Created:   2022-03-04
# Modified:  2022-03-07

extractHTMLtext <- function(html_file, first_stop, last_stop, ...) {
  # Internals -------------------------------------------------------------

    # html_file = "../input/html/689268.html"
    # first_stop = "More"
    # last_stop = "References"

  # Body ------------------------------------------------------------------
  # Source: https://www.r-bloggers.com/2011/10/reading-html-pages-in-r-for-text-processing/
    # Read and parse HTML file
    doc_html <- textreadr::read_html(html_file, ...)

    # Get rid of part of the article until the first_stop
    first_stop_loc <- which(doc_html == first_stop)[sum(doc_html == first_stop)]
    doc_html <- doc_html[(first_stop_loc+1):length(doc_html)]

    # Get rid of part of the article after the last_stop
    last_stop_loc <- tail(which(doc_html == last_stop), 1)
    doc_html <- doc_html[1:(last_stop_loc-1)]

    # Collapse into a single text
    doc_text <- paste0(doc_html, collapse = " ")

    # Return
    return(doc_text)
}
