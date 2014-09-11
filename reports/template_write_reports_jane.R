## Report Brewer

## Set working directory appropriately!
setwd("\\\\poc/pocShare/Data Portal/county_reports/jane_test")

require(poc)
require(brew)
require(knitr)
require(plyr)
#require(RCurl)

write_report <- function(county_arg, upload = FALSE){
    rnw_file <- sprintf("county_report_%s.rnw", county_arg)
    brew::brew("../template/county_report_template.brew", rnw_file)
    knit2pdf(rnw_file, compiler = "XELATEX")
    if (upload) {
        pdf_file <- sprintf("county_report_%s.pdf", county_arg)
        ftpUpload(what = pdf_file,
                  to = paste0("sftp://mienkoja:bigjose007@pocweb.cac.washington.edu/data/pocweb/www-test/county_reports/",
                              pdf_file))
    }
}

# produce reports
counties <- ref_lookup_county$county[1:39]

plyr::l_ply(counties, function(x) write_report(x, upload = FALSE))
# plyr::l_ply(counties, function(x) write_report(x, upload = TRUE))


