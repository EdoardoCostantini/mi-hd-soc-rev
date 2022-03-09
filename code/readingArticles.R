# Project:   imputationSociology
# Objective: TODO
# Author:    Edoardo Costantini
# Created:   2022-03-04
# Modified:  2022-03-09

  rm(list = ls())
  source("extractHTMLtext.R")
  source("extractPhrases.R")

 # Load packages
  library(XML)
  library(tm)
  library(writexl)

# Prepare for text mining analysis ---------------------------------------------

  # Define the location of the pdfs
  path_AJS <- "../input/ALS-2017-2021/"
  path_ASR <- "../input/ASR"
    path_ASR_internal <- list.dirs(path_ASR, recursive = FALSE)
  files_ASR <- sapply(path_ASR_internal, function (x){
    paste0(x, "/", list.files(x))
  })

  # List of files for the ASR journal
  files_ASR <- unlist(files_ASR)

  # Find what htmls you have in the input folder
  files_AJS <- list.files(path = path_AJS,
                      pattern = "html$")

  # Load the texts in your sessions by using extractHTMLtext
  articles <- lapply(paste0(path_AJS, files_AJS), extractHTMLtext,
                     first_stop = "More",
                     last_stop = "References",
                     encoding = "UTF-8"
  )

  # Load ASR htmls
  articles <- lapply(files_ASR,
                     extractHTMLtext,
                     first_stop = "Article Information",
                     last_stop = "References",
                     encoding = "UTF-8"
  )

  # Give names
  names(articles) <- files_AJS

  # Create a corpus
  corp <- tm::Corpus(VectorSource(articles))

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
  suv_df <- extracPhrases(toks, suv_phrases, window = 3)
  sta_df <- extracPhrases(toks, sta_phrases, window = 3)

  # Papers that mention surveys and do inference
  pp_index <- unique(suv_df$docname) %in% unique(sta_df$docname)
  pp_selected <- unique(suv_df$docname)[pp_index]

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
  NA_df <- extracPhrases(toks[pp_selected], NA_phrases, window = 20)
  length(unique(NA_df$docname))
  length(unique(NA_df$docname))/length(pp_selected) * 100

# Mentions of imputation -------------------------------------------------------

  imp_phrases <- c("imputation*",
                   "single imputation*",
                   "multiple imputation*")

  imp_df <- extracPhrases(toks[pp_selected], imp_phrases, window = 50)
  length(unique(imp_df$docname))
  length(unique(imp_df$docname))/length(unique(NA_df$docname)) * 100

  # Identify original papers filenames
  mi_papers <- files_AJS[as.numeric(gsub("text",
                                         "",
                                         unique(imp_df$docname)))]
  mi_papers <- files_ASR[as.numeric(gsub("text",
                                         "",
                                         unique(imp_df$docname)))]

  # Make the object a data.frame
  imp_xls <- as.data.frame(imp_df)

  # Keep the text id, pre, keyword, and post columns
  important_cols <- colnames(imp_xls)[c(1, 4, 5, 6)]
  imp_xls <- imp_xls[, important_cols]

  # Collapse the pre, keyword, and post columns into paragraphs
  imp_xls$paragraph <- apply(imp_xls, 1, function (x){
    paste0(x[important_cols[-1]], collapse = " ")
  })

  # Keep only id and paragraph
  imp_xls <- imp_xls[, c("docname", "paragraph")]

  # Collapse all paragraphs into one
  fullParagraphs <- sapply(unique(imp_xls$docname), function(x){
    paste0(imp_xls[imp_xls$docname == x, "paragraph"],
           collapse = " \n \n [...] ")
  })

  # Create data.frame for annotation
  imp_xls <- data.frame(fileName = mi_papers,
                        textName = unique(imp_xls$docname),
                        tag_im = "", # actually does some form of imputation
                        tag_mi = "", # performs mi
                        tag_nm = "", # mentions number of imputation
                        tag_pr = "", # describes which predictors are used in the imputation models
                        notes = "")

  # Give meaningful rownames to make it easier for coding
  rownames(imp_xls) <- mi_papers

  # Order the rows by fileName
  imp_xls <- imp_xls[order(imp_xls$fileName), ]

  # Manually code the text data
  imp_xls["690053.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(0, 0, 0, 0)
  imp_xls["691261.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 1, 1, 0)
  imp_xls["691327.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 0, 0, 0)
  imp_xls["692350.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(0, 0, 0, 0)
  imp_xls["693703.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 1, 1, 0)
  imp_xls["696209.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(0, 0, 0, 0)
  imp_xls["697111.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 1, 1, 0)
  imp_xls["697498.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 0, 0, 0)
  imp_xls["697528.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 0, 0, 0)
  imp_xls["698481.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 1, 1, 1)
  imp_xls["699652.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 1, 1, 0)
  imp_xls["702008.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 1, 1, 0)
  imp_xls["702278.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 1, 1, 0)
  imp_xls["703044.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 1, 1, 1)
  imp_xls["707985.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 1, 1, 1)
  imp_xls["709778.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(0, 0, 0, 0)
  imp_xls["711231.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 1, 1, 0)
  imp_xls["712406.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(0, 0, 0, 0)
  imp_xls["712972.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 1, 1, 0)
  imp_xls["714062.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 1, 1, 0)
  imp_xls["717448.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 1, 0, 0)
  imp_xls["718451.html", c("tag_im", "tag_mi", "tag_nm", "tag_pr")] <- c(1, 0, 0, 0)

  # Draw conclusions
  # Papers doing actual imputation
  sum(imp_xls$tag_im == 1)

  # Papers doing multiple imputation
  sum(imp_xls$tag_mi == 1)

  # Papers descirbing how many imputations
  sum(imp_xls$tag_nm == 1)

  # Papers descirbing which predictors are included in the imputation model
  sum(imp_xls$tag_pr == 1)

  # Add paragraphs for excel sheet
  imp_xls$paragraphs <- fullParagraphs

  # Write in an excel sheet
  write_xlsx(imp_xls,"../output/papersImputing.xlsx")

  # Copy all of the html files imputing to the output directory
  lapply(mi_papers, function (x){
    system(command = paste0("cp ", path_AJS, x, " ../output/papersImputing/"))
  })