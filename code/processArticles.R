# Project:   imputationSociology
# Objective: Function to precess articles
# Author:    Edoardo Costantini
# Created:   2022-03-09
# Modified:  2023-08-28

processArticles <- function(file_names, file_paths,
                            first_stop, last_stop,
                            encoding = "UTF-8",
                            par_length = 5) {
  # Internals -------------------------------------------------------------

  # file_names = files_AJS
  # file_paths = path_AJS
  # first_stop = "More"
  # last_stop  = "References"
  # encoding   = "UTF-8"
  # par_length = 5 # number of words to extract for context

  # Body ------------------------------------------------------------------

  # Load text of files
  articles <- lapply(paste0(file_paths, file_names),
                     extractHTMLtext,
                     first_stop = first_stop,
                     last_stop = last_stop,
                     encoding = encoding
  )

  # Give names
  names(articles) <- file_names

  # Create a corpus
  corp <- tm::Corpus(VectorSource(articles))

  # Create quanteda tokens
  q_corp <- quanteda::corpus(corp)
  toks <- quanteda::tokens(q_corp)

  # Mentions of survey / sample ------------------------------------------------

  suv_phrases <- c("sample*",
                   "survey*",
                   "questionnaire")
  sta_phrases <- c("significan*",
                   "standard error",
                   "confidence interval*",
                   "pvalue",
                   "p-value"
  )
  suv_df <- extracPhrases(toks, suv_phrases)
  sta_df <- extracPhrases(toks, sta_phrases)

  # Papers that mention surveys and do inference
  pp_index <- unique(suv_df$docname) %in% unique(sta_df$docname)
  pp_selected <- unique(suv_df$docname)[pp_index]

  # Mentions of missing values -------------------------------------------------

  # Define possible ways of referring to missing values
  NA_phrases <- c("missing value*",
                  "missing case*",
                  "missing observation*",
                  "missing row*",
                  "nonresponse",
                  "non-response"
  )

  # Run extraction
  NA_df <- extracPhrases(toks[pp_selected], NA_phrases, window = par_length)

  # Mentions of imputation -----------------------------------------------------

  imp_phrases <- c("imputation*",
                   "single imputation*",
                   "multiple imputation*")

  imp_df <- extracPhrases(toks[pp_selected], imp_phrases, window = par_length)

  # Identify original papers filenames
  mi_papers <- file_names[as.numeric(gsub("text",
                                          "",
                                          unique(imp_df$docname)))]

  # Make the object a data.frame
  imp_code <- as.data.frame(imp_df)

  # Keep the text id, pre, keyword, and post columns
  important_cols <- colnames(imp_code)[c(1, 4, 5, 6)]
  imp_code <- imp_code[, important_cols]

  # Collapse the pre, keyword, and post columns into paragraphs
  imp_code$paragraph <- apply(imp_code, 1, function (x){
    paste0(x[important_cols[-1]], collapse = " ")
  })

  # Keep only id and paragraph
  imp_code <- imp_code[, c("docname", "paragraph")]

  # Collapse all paragraphs into one
  fullParagraphs <- sapply(unique(imp_code$docname), function(x){
    paste0(imp_code[imp_code$docname == x, "paragraph"],
           collapse = " \n \n [...] ")
  })

  # Create data.frame for annotation
  imp_xls <- data.frame(textName = unique(imp_code$docname),
                        tag_im = "", # actually does some form of imputation
                        tag_mi = "", # performs mi
                        tag_nm = "", # mentions number of imputation
                        tag_pr = "", # describes which predictors are used in the imputation models
                        notes = "")

  # Give meaningful rownames to make it easier for coding
  rownames(imp_xls) <- mi_papers

  # Order by file name
  imp_xls <- imp_xls[order(mi_papers), ]

  # Output ---------------------------------------------------------------------
  return(
    list(coding = imp_xls,
         n_papers = length(pp_selected),
         paragraphs = fullParagraphs,
         topic_NA = NA_df,
         topic_MI = imp_df)
  )
}
