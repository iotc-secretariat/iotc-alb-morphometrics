# Source the R codes
setwd("initialization")
source("00_CORE.R")
setwd("..")

# DOCX
render("rmd/00_DOCX.Rmd",
       output_format = "word_document2",
       output_dir    = "outputs/",
       output_file = "IOTC-2022-WPTmT08-06_Rev1.docx")

# PPTX ####
render("rmd/00_PPTX.Rmd",
       output_format = powerpoint_presentation(reference_doc = "../templates/IOTC_WPTmT08_template.pptx", slide_level = 2),
       output_dir    = "outputs/",
       output_file   = "IOTC-2022-WPTmT08-06_Rev1.pptx"
)
