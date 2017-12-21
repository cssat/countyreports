## Report Brewer

## Remember to
##  - disable focus_county in county_reports.R
##  - take versions suffixes off of file names
##  - take version suffix out of reference to .R file in .Rnw file

require(pocr)
require(brew)
require(knitr)
require(RCurl)
require(tools)
library(stringr)
library(lubridate)
if (!str_detect(getwd(), "/reports$")) setwd("reports/")

write_report <- function(county_arg){
    Sys.setenv(PDFLATEX = "xelatex")
    display_county <<- county_arg
    rnw_file <- sprintf("county_report_%s.Rnw", county_arg)
    brew("template.Rnw", rnw_file)
    knit2pdf(input = rnw_file,
             output = str_replace(rnw_file, pattern = "Rnw", replacement = "tex"),
             compiler = "xelatex")
    texi2pdf(file = str_replace(rnw_file, pattern = "Rnw", replacement = "tex"),
             clean = TRUE)
}

# produce reports
omit_ooh_counties <- c("Adams", "Asotin", "Columbia", "Ferry", "Garfield",
                       "Klickitat", "Lincoln", "Pacific", "Pend Oreille",
                       "San Juan", "Skamania", "Wahkiakum")
counties <- ref_lookup_county[ref_lookup_county$county_cd %in% 1:39,]$county_desc
counties <- counties[counties %nin% omit_ooh_counties]
# counties <- "King"
#counties <- c("Cowlitz", "Clark", "Clallam", "Jefferson", "Thurston")
#counties <- c("Grays Harbor", "San Juan", "Pend Oreille", "Walla Walla")
#counties <- c("Benton", "Franklin", "Kittitas", "Klickitat", "Walla Walla", "Yakima")


for (i in seq_along(counties)) {
    try(write_report(counties[i]))
}

bad_exts = c("*.tex", "*.log", "*.out", "*.aux")
unlink(x = bad_exts)


