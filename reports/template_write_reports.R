## Report Brewer

## Set working directory appropriately!
setwd("S:/Data Portal/county_reports/reports")

## Remember to
##  - disable focus_county in county_reports.R
##  - take versions suffixes off of file names
##  - take version suffix out of reference to .R file in .Rnw file

require(pocr)
require(brew)
require(knitr)
require(RCurl)
require(tools)

write_report <- function(county_arg, upload = FALSE){
    Sys.setenv(PDFLATEX = "xelatex")
    display_county <<- county_arg
    rnw_file <- sprintf("county_report_%s.Rnw", county_arg)
    brew("template.Rnw", rnw_file)
    knit2pdf(rnw_file, compiler = "xelatex")
    texi2pdf(file = str_replace(rnw_file, pattern = "Rnw", replacement = "tex"))
    if (upload) {
        pdf_file <- sprintf("county_report_%s.pdf", county_arg)
        ftpUpload(what = pdf_file,
                  to = paste0("sftp://gregorp:PASSWORD@pocweb.cac.washington.edu/data/pocweb/www-review/county_reports/",
                              pdf_file))
    }
}

# produce reports
omit_ooh_counties <- c("Garfield", "Lincoln", "San Juan", "Wahkiakum", "Skamania", "Columbia")
counties <- ref_lookup_county[1:39, "county"]
counties <- counties[counties %nin% omit_ooh_counties]
counties <- "King"
#counties <- c("Cowlitz", "Clark", "Clallam", "Jefferson", "Thurston")
#counties <- c("Grays Harbor", "San Juan", "Pend Oreille", "Walla Walla")
#counties <- c("Benton", "Franklin", "Kittitas", "Klickitat", "Walla Walla", "Yakima")

plyr::l_ply(counties, function(x) write_report(x, upload = FALSE))

plyr::l_ply(counties, function(x) write_report(x, upload = TRUE))

