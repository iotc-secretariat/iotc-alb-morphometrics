# Source the R codes
setwd("initialization")
source("00_CORE.R")
setwd("..")

options(scipen = 100)

# # DOCX
# render("rmd/00_DOCX_HTML.Rmd", 
#        output_format = "word_document2", 
#        output_dir    = "outputs/", 
#        output_file   = paste0(TITLE, ".docx")
# )
# 
# # HTML
# render("rmd/00_DOCX_HTML.Rmd",
#        output_format = "html_document2",
#        output_dir    = "outputs/",
#        output_file   = paste0(TITLE, ".html")
# )
# 
# PPTX
render("rmd/00_PPTX.Rmd",
       output_format = powerpoint_presentation(reference_doc = "../templates/ppt_template.potx", slide_level = 1),
       output_dir    = "outputs/"
       )

