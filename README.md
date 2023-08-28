# Multiple Imputation with High-dimensional Imputation Models Sociological Review

This repository contains the code to analyze the html text for 169 Research articles published in the American Journal of Sociology (AJS), and American Sociological Review (ASR). between January 2017 and January 2022.

## Replication guide

1. Download the articles

   1. Go to the [AJS main page](https://www-journals-uchicago-edu.tilburguniversity.idm.oclc.org/loi/ajs)
   2. Search for all articles with the following filters:
      1. `Research Article`
      2. `2017-2022`
      3. `American Journal of Sociology`
   3. Display 100 elements
   4. Use [DownThemAll!](https://www.downthemall.net/) to download all the "full text" in html for the period of interest
   5. Store the articles in the folder `input/AJS-2017-2021/`
   6. Repeat the same procedure for ASR

2. Run analysis

   1. Open the script: `script_readingArticles.R`
   2. Run it
