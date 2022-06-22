# Source the R codes
setwd("initialization")
source("00_CORE.R")
setwd("..")

# DOCX
render("rmd/00_DOCX.Rmd",
       output_format = "word_document2",
       output_dir    = "outputs/",
       output_file = "Albacore-Length-Weight-Relationship.docx")

