# Project:   imputationSociology
# Objective: function to extract contexts for given phrases
# Author:    Edoardo Costantini
# Created:   2022-03-07
# Modified:  2022-03-07

extracPhrases <- function(tokens, phrases, ...) {
  # Internals -------------------------------------------------------------

  # tokens = ... # output of quanteda::tokens() call
  # phrases = ... # character vector of alternative words for a concept

  # Body ------------------------------------------------------------------

  # Look for tokens presenting any of the phrases
  out <- lapply(phrases, function (x){
    quanteda::kwic(tokens, pattern = quanteda::phrase(x), ...)
  })

  # Put them all in a single data-frame
  out_df <- do.call(rbind, out)

  # Return output
  return(out_df)
}
