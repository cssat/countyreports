countyreports
=============

R code for creating POC's County Reports.

## Installation Requirements

You will need to have installed

- R
- The following R packages:
    - pocr (see the pocr git repository)
    - extrafont
    - knitr 
    - brew
    - xtable
- TeX (with XeLaTeX compiler)

You will also need to have an ODBC connection set up to annie (or test_annie or review_annie if you're building from there). For production annie, you may need to set up a pipe (e.g., via Putty) to allow R to talk to the db.

## First time set-up or recently updated R

If you haven't used the `extrafont` package before, you will need to load it in an R session and import the system fonts. This will take some time (between 10 and 30 minutes) but only needs to be done once per base R installation. The code is as follows:

```{r}
library(extrafont)
font_import()

# verify that it worked
loadfonts()
```

## Creating county reports

In the `reports/` directory, open the file `template_write_reports.R`. It has some reminders at the top which shouldn't require any action unless you're updating the report template. Simply run the code in `template_write_reports.R` to produce the county report PDFs and clean up the intermediate files.

Check that all the PDFs are about the right size (400-500 KB), and open a few of them to verify nothing's amiss, then they can be uploaded to `data/pocweb/www-*/county_reports/` with the `*` filled in with the appropriate version of the website.

## Technical overview

The tempalate is separated into two files: `template.Rnw` contains all the text and TeX code to produce the formatting, as well as placeholders for R code. The R code is externalized to `county_reports.R`, where the code chunks are named with a `## @knitr chunk_name` comment for placement into the `.Rnw` file. (See the knitr manual for details on Rnw documents with externalized R code.) Many of the graphs are produced by functions defined in the `pocr` package. At some point, they should probably be put into their own `countyreports` package...

To write a report (this is done with the `write_report` function in `template_write_reports.R`), we loop through each county, use `brew::brew` to create the individual county .Rnw file, `knit` the .Rnw file into .tex and .PDF files, and re-compile the PDF from the .tex file to fix internal references (this is supposed to happen automatically with `knit2pdf`, but it seems to need one more nudge).

There is code there to auto-upload the files, but I've had mixed success with it in the past so I generally transfer the files manually.