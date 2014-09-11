require(pocr)
require(knitr)

display_county <- "Pierce"

knit2pdf("S:/Data Portal/county_reports/development/template_v8.Rnw",
         compiler = "xelatex")


