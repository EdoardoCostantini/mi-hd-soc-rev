# Project:   imputationSociology
# Objective: TODO
# Author:    Edoardo Costantini
# Created:   2022-03-04
# Modified:  2022-03-04

 # Load packages
  library(XML)
  library(tm)

# Prepare for text mining analysis ---------------------------------------------

  # Define the location of the pdfs
  path_AJS <- "../input/ALS-2017-2021/"

  # Find what htmls you have in the input folder
  files <- list.files(path = path_AJS,
                      pattern = "html$")

  # Load the texts in your sessions by using extractHTMLtext
  articles <- lapply(paste0(path_AJS, files), extractHTMLtext)

  # Give names
  names(articles) <- paste0("article", 1:length(articles))

  # Create a corpus
  corp <- tm::Corpus(VectorSource(articles))

  # Remove undesired sections
  txt <- c("Text of the article. Reference Section 1: Foo", "Text of the article 2. Reference Section 2: Bar")
  corp <- Corpus(VectorSource(txt))
  removeRefSec <- content_transformer(function(x) sub("(?<=Reference)(?s)(.*$)", "", x))

  corp[[1]]
  removeRefSec(corp[[1]])

  corp <- tm_map(corp, removeRefSec)
  (corp[[2]])

  # Create quanteda tokens
  q_corp <- quanteda::corpus(corp)
  toks <- quanteda::tokens(q_corp)

# Mentions of survey / sample --------------------------------------------------

  suv_phrases <- c("sample*",
                   "survey*")
  sta_phrases <- c("significan*",
                   "standard error",
                   "confidence interval*",
                   "pvalue",
                   "p-value"
  )
  suv_df <- extracPhrases(toks, suv_phrases, window = 20)
  sta_df <- extracPhrases(toks, sta_phrases, window = 20)

  # Papers that mention surveys and do inference
  paper_selected <- unique(suv_df$docname)[unique(suv_df$docname) %in% unique(sta_df$docname)]

# Mentions of missing values ---------------------------------------------------

  # Define possible ways of referring to missing values
  NA_phrases <- c("missing value*",
                  "missing case*",
                  "missing observation*",
                  "missing row*",
                  "nonresponse",
                  "non-response"
  )

  # Run extraction
  NA_df <- extracPhrases(toks[paper_selected], NA_phrases, window = 20)
  length(unique(NA_df$docname))/length(paper_selected) * 100

# Mentions of imputation -------------------------------------------------------

  imp_phrases <- c("imputation*",
                   "single imputation*",
                   "multiple imputation*")

  imp_df <- extracPhrases(toks[paper_selected], imp_phrases, window = 20)
  length(unique(imp_df$docname))/length(unique(NA_df$docname)) * 100
