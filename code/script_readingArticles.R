# Project:   imputationSociology
# Objective: Coding artciels from different journals
# Author:    Edoardo Costantini
# Created:   2022-03-04
# Modified:  2023-08-28

  rm(list = ls())
  source("extractHTMLtext.R")
  source("extractPhrases.R")
  source("processArticles.R")

 # Load packages
  library(XML)
  library(tm)
  library(writexl)
  library(text2map)

# Prepare for text mining analysis ---------------------------------------------

  # Define the location of the pdfs
  path_AJS <- "../input/AJS-2017-2021/"
  path_ASR <- "../input/ASR-2017-2021/"

  # Find what htmls you have in the input folder
  files_AJS <- list.files(path = path_AJS,
                          pattern = "html$")
  files_ASR <- list.files(path = path_ASR,
                          pattern = "html$")


  # Process AJS
  AJS <- processArticles(file_names = files_AJS,
                         file_paths = path_AJS,
                         first_stop = "More",
                         last_stop = "References")
  ASR <- processArticles(file_names = files_ASR,
                         file_paths = path_ASR,
                         first_stop = "Article Information",
                         last_stop = "References")

# Decriptives ------------------------------------------------------------------

  # Mentions of NAs
  length(unique(AJS$topic_NA$docname))
  length(unique(AJS$topic_NA$docname))/length(AJS$n_papers) * 100

  # Mentions of imputation
  length(unique(AJS$topic_MI$docname))
  length(unique(AJS$topic_MI$docname))/length(unique(AJS$topic_NA$docname)) * 100

# Coding of papers -------------------------------------------------------------

  # Copy all of the html files to manually scan to named output directories
  lapply(rownames(AJS$coding), function (x){
    system(command = paste0("cp ", path_AJS, x, " ../output/AJS/"))
  })
  lapply(rownames(ASR$coding), function (x){
    system(command = paste0("cp ", path_ASR, x, " ../output/ASR/"))
  })

  # Columns to fill with manual coding
  code_cols <- c("tag_im", # Was imputation actually performed? 0 = no, 1 = yes
                 "tag_mi", # Was multiple imputation actually performed? 0 = no, 1 = yes
                 "tag_nm", # Was the number of imputation reported? 0 = no, 1 = yes
                 "tag_pr") # Were the predictors for the imputation models described? 0 = no, 1 = yes

  # > Manually code AJS papers -------------------------------------------------

  AJS$coding["690053.html", code_cols] <- c(0, 0, 0, 0)
  AJS$coding["691261.html", code_cols] <- c(1, 1, 1, 0)
  AJS$coding["691327.html", code_cols] <- c(1, 0, 0, 0)
  AJS$coding["692350.html", code_cols] <- c(0, 0, 0, 0)
  AJS$coding["693703.html", code_cols] <- c(1, 1, 1, 0)
  AJS$coding["696209.html", code_cols] <- c(0, 0, 0, 0)
  AJS$coding["697111.html", code_cols] <- c(1, 1, 1, 0)
  AJS$coding["697498.html", code_cols] <- c(1, 0, 0, 0)
  AJS$coding["697528.html", code_cols] <- c(1, 0, 0, 0)
  AJS$coding["698481.html", code_cols] <- c(1, 1, 1, 1) # all analysis models variables
  AJS$coding["699652.html", code_cols] <- c(1, 1, 1, 0)
  AJS$coding["702008.html", code_cols] <- c(1, 1, 1, 0)
  AJS$coding["702278.html", code_cols] <- c(1, 1, 1, 0)
  AJS$coding["703044.html", code_cols] <- c(1, 1, 1, 1)
  AJS$coding["707985.html", code_cols] <- c(1, 1, 1, 1) # all independent variables in our analyses
  AJS$coding["709778.html", code_cols] <- c(0, 0, 0, 0)
  AJS$coding["711231.html", code_cols] <- c(1, 1, 1, 0)
  AJS$coding["712406.html", code_cols] <- c(0, 0, 0, 0)
  AJS$coding["712972.html", code_cols] <- c(1, 1, 1, 0)
  AJS$coding["714062.html", code_cols] <- c(1, 1, 1, 0)
  AJS$coding["717448.html", code_cols] <- c(1, 1, 0, 0)
  AJS$coding["718451.html", code_cols] <- c(1, 0, 0, 0)

  # Draw conclusions
  # Total survey/sample + inference articles
  AJS$n_papers

  # Papers doing actual imputation
  sum(AJS$coding$tag_im == 1)

  # Papers doing multiple imputation
  sum(AJS$coding$tag_mi == 1)

  # Papers descirbing how many imputations
  sum(AJS$coding$tag_nm == 1)

  # Papers descirbing which predictors are included in the imputation model
  rownames(AJS$coding[which(AJS$coding$tag_pr == 1), ])
  sum(AJS$coding$tag_pr == 1)

  # Add paragraphs for excel sheet
  AJS$coding$paragraphs <- AJS$paragraphs

  # Write in an excel sheet
  write_xlsx(AJS$coding,"../output/papersImputing.xlsx")

  # > Manually code ASR papers -------------------------------------------------

  ASR$coding["0003122416684777.html" , code_cols] <- c(1, 1, 1, 0)
  ASR$coding["0003122416686521.html" , code_cols] <- c(1, 1, 0, 0)
  ASR$coding["0003122417691952.html" , code_cols] <- c(0, 0, 0, 0)
  ASR$coding["0003122417701115.html" , code_cols] <- c(1, 1, 0, 0)
  ASR$coding["0003122417706463.html" , code_cols] <- c(1, 0, 0, 0)
  ASR$coding["0003122417709294.html" , code_cols] <- c(1, 1, 1, 0)
  ASR$coding["0003122417712729.html" , code_cols] <- c(1, 1, 0, 0)
  ASR$coding["0003122417714422.html" , code_cols] <- c(1, 1, 0, 0)
  ASR$coding["0003122417716611.html" , code_cols] <- c(1, 1, 0, 0)
  ASR$coding["0003122417724001.html" , code_cols] <- c(0, 0, 0, 0)
  ASR$coding["0003122417734353.html" , code_cols] <- c(1, 1, 1, 1) # as a function of the other variables in the analysis
  ASR$coding["0003122417751442.html" , code_cols] <- c(0, 0, 0, 0)
  ASR$coding["0003122418759651.html" , code_cols] <- c(1, 1, 0, 1) # uses two precise variables
  ASR$coding["0003122418773353.html" , code_cols] <- c(0, 0, 0, 0)
  ASR$coding["0003122418785368.html" , code_cols] <- c(0, 0, 0, 0)
  ASR$coding["0003122418792836.html" , code_cols] <- c(1, 1, 1, 1) # all variables as predictors
  ASR$coding["0003122418795856.html" , code_cols] <- c(1, 1, 0, 0)
  ASR$coding["0003122418808005.html" , code_cols] <- c(1, 1, 1, 1)
  ASR$coding["0003122418820702.html" , code_cols] <- c(1, 1, 1, 0)
  ASR$coding["0003122418821339.html" , code_cols] <- c(1, 1, 1, 0)
  ASR$coding["0003122418823184.html" , code_cols] <- c(1, 1, 0, 0)
  ASR$coding["0003122419832497.html" , code_cols] <- c(1, 1, 0, 1)
  ASR$coding["0003122419847165.html" , code_cols] <- c(1, 1, 1, 0)
  ASR$coding["0003122419848723.html" , code_cols] <- c(0, 0, 0, 0)
  ASR$coding["0003122419885094.html" , code_cols] <- c(0, 0, 0, 0)
  ASR$coding["0003122419886550.html" , code_cols] <- c(0, 0, 0, 0)
  ASR$coding["0003122420930018.html" , code_cols] <- c(1, 1, 0, 0)
  ASR$coding["0003122420967647.html" , code_cols] <- c(0, 0, 0, 0)
  ASR$coding["0003122421996686.html" , code_cols] <- c(1, 1, 0, 0)
  ASR$coding["00031224211004187.html", code_cols] <- c(0, 0, 0, 0)
  ASR$coding["00031224211027800.html", code_cols] <- c(0, 0, 0, 0)
  ASR$coding["00031224211038507.html", code_cols] <- c(1, 1, 1, 1) # all variables!
  ASR$coding["00031224211056267.html", code_cols] <- c(0, 0, 0, 0)

  # Draw conclusions
  # Total survey/sample + inference articles
  ASR$n_papers

  # Papers doing actual imputation
  sum(ASR$coding$tag_im == 1)

  # Papers doing multiple imputation
  sum(ASR$coding$tag_mi == 1)

  # Papers descirbing how many imputations
  sum(ASR$coding$tag_nm == 1)

  # Papers descirbing which predictors are included in the imputation model
  rownames(ASR$coding[which(ASR$coding$tag_pr == 1), ])
  sum(ASR$coding$tag_pr == 1)

  # Add paragraphs for excel sheet
  ASR$coding$paragraphs <- ASR$paragraphs
