require(pocr)
require(knitr)

display_county <- "Pierce"

knit2pdf("development/template_v10.Rnw",
         compiler = "xelatex")


