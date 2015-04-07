# R code for auto_reports v2

## frontmatter----
## @knitr frontmatter
display_county <- "Pierce"
trend_date_type <- 1
data_through_date <- "2012-12-31" ## TODO build other dates off of this one

# Date ranges for queries. DO NOT RENAME without changing .Rnw file too
ia_start_date <-  "2009-01-01"
ihs_start_date <- "2009-01-01"
ooh_start_date <- "2009-01-01"
context_date <- "2012-10-01" ## Using last quarter for context
context_date_string <- paste0("Quarter ", quarter(context_date), ", ", year(context_date))
ia_end_date <- ihs_end_date <- ooh_end_date <- "2012-12-01"
cohort_date <- "2010-01-01"
wb_start_date <- as.Date("2009-01-01")
wb_end_date <- as.Date(wb_start_date + duration(2, "years") - duration(1, "months"))

cohort_year <- year(cohort_date)

# Setting figure dimensions
fig_width <- 6
trend_fig_height <- 2
context_fig_2line_coef <- 0.3
context_fig_1line_coef <- 0.2

## All subsequent chunks should be able to depend on this one alone.
require(poc)
require(xtable)
require(lubridate)

## xtable and ggplot options
options(xtable.NA.string = "NA")
dotplot_colors <- portal_colors[c(4, 8, 6)]
plot_title_size = 0.8 ## rel size

odbcCloseAll()
con <- odbcConnect("test_annie")

focus_county <- tolower(display_county)
focus_county_cd <- ref_lookup_county$county_cd[tolower(ref_lookup_county$county) == focus_county]
focus_info <- county_to_office(focus_county)
state_label <- "WASHINGTON"
omit_ooh_counties <- c("Garfield", "Lincoln", "San Juan", "Wahkiakum", "Skamania", "Columbia")

## Get relevant offices
focus_office <- sort(unique(focus_info$cd_office))
focus_office_tx <- sort(unique(as.character(focus_info$tx_office)))
focus_group <- sort(unique(county_to_office(focus_county)$cd_office_county_grp))
focus_group_tx <- sort(unique(as.character(focus_info$tx_office_county_grp)))

## Get counties in same region
region_cd <- ref_lookup_county$region_cd[ref_lookup_county$county_cd == focus_county_cd]
region_counties <- ref_lookup_county[ref_lookup_county$region_cd %in% ref_lookup_county[tolower(ref_lookup_county$county) == focus_county, 3], 1]
region_counties_tx <- ref_lookup_county$county[region_counties]
region_info <- do.call(rbind, poc:::county_to_office_v(region_counties))
region_offices <- sort(unique(region_info$cd_office))
region_groups <- sort(unique(region_info$cd_office_county_grp))

## Background info
context_year <- strsplit(context_date, "-")[[1]][1]
focus_pop_person <- sqlQuery(con, paste0("call sp_population_person(", focus_county_cd, ",", context_year, ");"))$total
focus_pop_house <- sqlQuery(con, paste0("call sp_population_household(", focus_county_cd, ",", context_year, ");"))$total

## Get data
## Focus Data
ia_call <- stored_procedure(sp = "poc2",
                            date = paste(ia_start_date, ia_end_date, sep = ","),
                            type = "counts",
                            office = focus_office)
ihs_call <- stored_procedure(sp = "poc3",
                             date = paste(ihs_start_date, ihs_end_date, sep = ","),
                             type = "counts",
                             office = focus_office)
ooh_call <- stored_procedure(sp = "poc1ab",
                             date = paste(ooh_start_date, ooh_end_date, sep = ","),
                             type = "counts",
                             county = focus_county)

ia_focus <- sqlQuery(con, ia_call)
ihs_focus <- sqlQuery(con, ihs_call)
ooh_focus <- sqlQuery(con, ooh_call)

## Context Data
ia_region_call <- stored_procedure(sp = "poc2",
                                   date = context_date,
                                   type = "perCapita",
                                   office = c(0, region_groups))
ihs_region_call <- stored_procedure(sp = "poc3",
                                    date = context_date,
                                    type = "perCapita",
                                    office = c(0, region_groups))
ooh_region_call <- stored_procedure(sp = "poc1ab",
                                    date = context_date,
                                    type = "perCapita",
                                    county = c(0, region_counties[region_counties %nin% omit_ooh_counties]))

ia_region <- sqlQuery(con, ia_region_call)
ihs_region <- sqlQuery(con, ihs_region_call)
ooh_region <- sqlQuery(con, ooh_region_call)

levels(ia_region$`DCFS Office Group`)[levels(ia_region$`DCFS Office Group`) == "All"] <- state_label
levels(ihs_region$`DCFS Office Group`)[levels(ihs_region$`DCFS Office Group`) == "All"] <- state_label
levels(ooh_region$County)[levels(ooh_region$County) == "All"] <- state_label

## Permanency Data  -- uses cohort_date
perm_call <- list()
perm_call[1] <- stored_procedure(sp = "discharge_exit_over_time_mr", date = cohort_date, county = c("all", focus_county)) # State level
perm_call[2] <- stored_procedure(sp = "discharge_exit_over_time", date = cohort_date, county = region_counties)

perm <- sqlQuery(con, perm_call[[1]])
names(perm) <- tolower(names(perm))
perm <- perm[, names(perm) %nin% c("age grouping", "gender", "race/ethnicity", "initial placement",
                                   "last placement", "county")]
perm2 <- sqlQuery(con, perm_call[[2]])
names(perm2) <- names(perm)
perm <- rbind(perm, perm2)
perm <- perm[perm$date_type == 2, c("county_cd", "discharge_type_cd", "discharge type", "m12", "m24")]
names(perm)[3] <- "outcome"
perm$outcome <- factor(perm$outcome, levels = c("Reunification", "Adoption", "Guardianship", "Emancipation",
                                                "Other", "Still in Out-of-Home Care"),
                       ordered = TRUE)
levels(perm$outcome)[6] <- "Still in Care"

perm$geo <- paste("Region", region_cd)
perm$geo[perm$county_cd == 0] <- state_label
perm$geo[perm$county_cd == focus_county_cd] <- paste(display_county, "County")

## IHS Safety
ihs_safety_call <- stored_procedure(sp = "PBCS3", date = cohort_date, office = c(0, focus_office))
ihs_safety_data <- sqlQuery(con, ihs_safety_call)
ihs_safety_data <- ihs_safety_data[ihs_safety_data$date_type == 2 &
                               ihs_safety_data$Months %in% c(12, 24),
                           c(4, 5, 7)]
levels(ihs_safety_data$Office)[levels(ihs_safety_data$Office) == "All"] <- state_label
ihs_safety_data <- dcast(ihs_safety_data, Office ~ Months, value.var = "Placed")

## OOH Safety 
ooh_safety_call <- stored_procedure(sp = "PBCP5", date = cohort_date,
                                    county = c(0, region_counties))

ooh_safety_data <- sqlQuery(con, ooh_safety_call)
ooh_safety_data <- ooh_safety_data[ooh_safety_data$date_type == 2,
                                   c("County", "Discharge Type", "M24")]
levels(ooh_safety_data$County)[levels(ooh_safety_data$County) == "All"] <- state_label
ooh_safety_data <- dcast(ooh_safety_data, County ~ `Discharge Type`, value.var = "M24")
ooh_safety_data[, 2:4] <- round(ooh_safety_data[, 2:4], 1)
ooh_safety_data <- ooh_safety_data[order(-ooh_safety_data[, 4]), ]
for (i in 2:ncol(ooh_safety_data)) {
    ooh_safety_data[!is.na(ooh_safety_data[, i]), i] <- paste0(ooh_safety_data[!is.na(ooh_safety_data[, i]), i], "%")
}
ooh_safety_data <- ooh_safety_data[, c(1, 4, 2, 3)]
odbcClose(con)

## overview----
## @knitr overview

qf_focus <- quickfacts[c(which(quickfacts$text == "Pierce"), 1), -1]
qf_table <- as.data.frame(t(qf_focus[, 2:ncol(qf_focus)]))

qf_print <- as.data.frame(qf_table)
names(qf_print) <- c(paste(display_county, "County"), "Washington")
row.names(qf_print) <- qf_dict[, 2]

qf_print[, 1] <- NA
qf_print[, 2] <- NA

qf_print[1, ] <- prettyNum(qf_table[1, ], format = "d", big.mark = ",",
                           preserve.width = "none")

qf_print[2:nrow(qf_table), 1] <- formatC(qf_table[2:nrow(qf_table), 1],
                                         big.mark = ",", digits = 1, format = "f",
                                         preserve.width = "individual")
qf_print[2:nrow(qf_table), 2] <- formatC(qf_table[2:nrow(qf_table), 2],
                                         big.mark = ",", digits = 1, format = "f",
                                         preserve.width = "individual")
pcts <- which(qf_dict$Unit == "PCT")
qf_print[pcts, 1] <- paste0(qf_print[pcts, 1], "%")
qf_print[pcts, 2] <- paste0(qf_print[pcts, 2], "%")

qf_print <- qf_print[-c(13, 15, 17), ]

print(xtable(qf_print,
             caption = paste(display_county, "County Summary"),
             align = c("l", "r", "r")),
      caption.placement = "top",
      include.rownames = TRUE,
      include.colnames = TRUE,
      floating = TRUE,
      booktabs = TRUE)

# qf_plot <- qf_plot[str_detect(rownames(qf_plot), pattern = "Population:"), ] / 100
# qf_plot$race <- str_replace(rownames(qf_plot), "Population: ", "")
# qf_plot <- melt(qf_plot, id = race)
# 
# ggplot(qf_plot, aes(x = factor(1), y = value, fill = variable)) +
#     geom_bar(stat = "identity", position = "dodge") +
#     facet_wrap(~race, scales = "free_y") +
#     scale_y_continuous(labels = percent_format()) +
#     theme_bw()


## ia_focus----
## @knitr ia_focus
names(ia_focus) <- tolower(names(ia_focus))
for (i in 1:length(focus_office)) {
    ia_trend_data <- ia_focus[ia_focus$date_type == trend_date_type &
                                  ia_focus$qry_type_poc2 == 0 &
                                  ia_focus$`dcfs office cd` == focus_office[i],
                              c("period_start", "total cases first day",
                                "opened cases", "case closures")]
    
    trend_plot(ia_trend_data, type = "ia",
               title = paste0("Trends in Investigations & Assessments:\n",
                              as.character(focus_info$tx_office[focus_info$cd_office == focus_office[i]]),
                              " DCFS Office"),
               title_size = plot_title_size
    )
}

## ia_context----
## @knitr ia_context
ia_region <- subset(ia_region, qry_type_poc2 == 0 & date_type == 1,
                    select = c("DCFS Office Group", "Total Cases First Day"))

levels(ia_region$`DCFS Office Group`)[levels(ia_region$`DCFS Office Group`) == "Pierce East, Pierce South & Pierce West (Pierce Cnty)"] <- "Pierce East & Pierce West (Pierce Cnty)"

context_plot(ia_region, focus = ifelse(str_detect(focus_group_tx, pattern = fixed("Pierce")),
                                       "Pierce East & Pierce West (Pierce Cnty)", focus_group_tx),
             xlab = "Rate of Investigations & Assessments (per 1,000 Households)",
             title = paste("Investigations & Assessments: Region", region_cd, "\n", context_date_string),
             title_size = plot_title_size)

## ihs_focus----
## @knitr ihs_focus
names(ihs_focus) <- tolower(names(ihs_focus))
for (i in 1:length(focus_office)) {
    ihs_trend_data <- ihs_focus[ihs_focus$date_type == trend_date_type &
                                    ihs_focus$qry_type_poc3 == 0 &
                                    ihs_focus$`dcfs office cd` == focus_office[i],
                                c("period_start", "total cases first day",
                                  "opened cases", "case closures")]
    
    trend_plot(ihs_trend_data, type = "ihs",
               title = paste0("Trends in In-Home Services:\n",
                              as.character(focus_info$tx_office[focus_info$cd_office == focus_office[i]]),
                              " DCFS Office"),
               title_size = plot_title_size)
}

## ihs_context----
## @knitr ihs_context
ihs_region <- subset(ihs_region, qry_type_poc3 == 0 & date_type == 1,
                     select = c("DCFS Office Group", "Total Cases First Day"))

levels(ihs_region$`DCFS Office Group`)[levels(ihs_region$`DCFS Office Group`) == "Pierce East, Pierce South & Pierce West (Pierce Cnty)"] <- "Pierce East & Pierce West (Pierce Cnty)"
context_plot(ihs_region, focus = ifelse(str_detect(focus_group_tx, pattern = fixed("Pierce")),
                                        "Pierce East & Pierce West (Pierce Cnty)", focus_group_tx),
             xlab = "Rate of In-Home Services (per 1,000 Households)",
             title = paste("In-Home Services: Region", region_cd, "\n", context_date_string),
             colors = dotplot_colors,
             title_size = plot_title_size)

## ihs_safety----
## @knitr ihs_safety
names(ihs_safety_data) <- c("", "Within 1 Year", "Within 2 Years")
ihs_safety_data <- ihs_safety_data[order(ihs_safety_data[, 2]), ]
ihs_table <- ihs_safety_data
ihs_table[, 2] <- paste0(round(ihs_safety_data[, 2]), "%")
ihs_table[, 3] <- paste0(round(ihs_safety_data[, 3]), "%")

print.xtable(xtable(ihs_table,
                    caption = "Percent of In-Home Service Cases Resulting in Out-of-Home Care Placement.",
                    align = c("l", "l", "r", "r")),
             caption.placement = "top",
             include.rownames = FALSE,
             floating = TRUE,
             booktabs = TRUE)

## ooh_focus----
## @knitr ooh_focus
if (display_county != "Lincoln") {
    names(ooh_focus) <- tolower(names(ooh_focus))
    
    ooh_focus <- subset(ooh_focus,
                        date_type == trend_date_type & qry_type_poc1 == 0,
                        select = c("month", "total in care first day",
                                   "number of entries", "number of exits"))
    trend_plot(ooh_focus, type = "ooh",
               title = paste("Trends in Out-of-Home Care:", display_county, "County"),
               title_size = plot_title_size)

}

## ooh_context----
## @knitr ooh_context
names(ooh_region) <- tolower(names(ooh_region))
ooh_region <- subset(ooh_region,
                     qry_type_poc1 == 0 & date_type == 1,
                     select = c("county", "total in care first day"))
ooh_region <- ooh_region[ooh_region$county %nin% omit_ooh_counties, , drop = TRUE]
context_plot(ooh_region, focus = display_county,
             xlab = "Rate of Out-of-Home Care (per 1,000 Children)",
             title = paste("Out-of-Home Care: Region", region_cd, "\n", context_date_string),
             title_size = plot_title_size)

## ooh_safety----
## @knitr ooh_safety
ooh_safety_data$County <- as.character(ooh_safety_data$County)
ooh_safety_data$County[ooh_safety_data$County == display_county] <- paste0("\\textbf{", display_county, "}")
rownames(ooh_safety_data) <- ooh_safety_data$County

ooh_safety_cap <- paste0('Percentage of Children Re-Entering Out-of-Home Care within Two Years of Discharge')

print.xtable(xtable(ooh_safety_data[ooh_safety_data[, 1] %nin% omit_ooh_counties, -1],
                    caption = ooh_safety_cap,
                    align = c("l", "r", "r", "r")),
             caption.placement = "top",
             include.rownames = TRUE,
             sanitize.rownames.function = as.character,
             floating = TRUE,
             booktabs = TRUE)

## ooh_outcomes----
## @knitr ooh_outcomes
ggplot(perm, aes(x = outcome, y = m24/100, fill = geo)) +
    theme_bw() +
    geom_bar(position = "dodge", stat = "identity") +
    scale_fill_manual(values=portal_colors[c(8, 3, 6)]) +
    scale_y_continuous(labels = percent) +
    labs(x = "",
         y = "Percent experiencing outcome",
         fill = "",
         title = "Outcomes Two Years After Entering Out-of-Home Care") +
    theme(axis.title = element_text(size = rel(0.8)),
          plot.title = element_text(size = rel(plot_title_size), vjust = 1),
          axis.text.y = element_text(size = rel(0.8)),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1))

## ooh_wb----
## @knitr ooh_wb
wb_dates <- seq.Date(from = as.Date("2008-01-01"),
                     to = as.Date("2012-12-01"),
                     by = "month")
#county_arg <- c("pierce", "spokane", "all")
county_arg <- 0:39

wb_call <- stored_procedure(sp="PBCW3", date = wb_dates,
                            county = county_arg)

odbcCloseAll()
con <- odbcConnect("test_annie")
wb <- sqlQuery(con, wb_call)

names(wb) <- tolower(names(wb))
wb <- wb[, c("cohort begin date", "county", "family setting (kin placement)")]

wb_pop_call <- stored_procedure(sp="poc1ab", date = wb_dates,
                                county = county_arg)
wb_pop <- sqlQuery(con, wb_pop_call)
names(wb_pop) <- tolower(names(wb_pop))
wb_pop <- wb_pop[wb_pop$qry_type_poc1 == 0 & wb_pop$date_type == 1,
                 c("month", "county", "total in care first day")]
names(wb)[1] <- "month"

wb <- merge(wb, wb_pop, by= c("month", "county"))

wb_fn <- function(wb) {
    sum(wb[, 3] * wb[, 4]) / sum(wb[, 4])
}
wb_5 <- ddply(wb, .variables= "county", .fun= wb_fn)

wb_context <- wb_5[wb_5$county %in% c(region_counties_tx[region_counties_tx %nin% omit_ooh_counties], "All"), , drop = TRUE]
levels(wb_context$county)[levels(wb_context$county) == "All"] <- state_label
wb_context$county <- reorder(wb_context$county, X=wb_context$V1)

wb_context$context_highlight <- ifelse(wb_context$county == display_county, 1, 0)
wb_context$context_highlight[wb_context$county == state_label] <- 2
wb_context$context_highlight <- factor(wb_context$context_highlight)

wb_title <- paste0("Kinship Care: Region ", region_cd,
                   " (Quarter ", quarter(wb_start_date), ", ", year(wb_start_date),
                   " through ", "Quarter ", quarter(wb_end_date), ", ", year(wb_end_date), ")")
ggplot(wb_context, aes(x = V1 / 100, y = county, color = context_highlight)) +
    geom_point(size = 5, shape = 18) +
    scale_color_manual(values = dotplot_colors) +
    labs(x = "Percent of Care-Days Spent in Kinship Care",
         y = "",
         title = wb_title) +
    scale_x_continuous(labels = percent_format(), limits = c(.00, 1.15 * max(wb_context$V1 / 100)),
                       expand = c(0, 0)) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_text(size = rel(0.8)),
          axis.text.y = element_text(size = rel(0.8)),
          plot.title = element_text(size = rel(plot_title_size)))



