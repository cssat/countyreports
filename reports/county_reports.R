# R code for auto_reports v2

## frontmatter ----
## @knitr frontmatter
require(pocr)
#display_county <- "King"

#### Pick a database ####

#db <- "production"
db <- "review"
#db <- "test"

#########################

trend_date_type <- 1
odbcCloseAll()

if (db == "production") {
    con <- odbcConnect("Annie")
    con_test <- odbcConnect("test_annie")
}

if (db == "review") {
    con <- odbcConnect("review_annie")
    con_test <- odbcConnect("test_annie")
}


if (db == "test") con <- odbcConnect("test_annie")

if (db == "production") data_through_date <- sqlQuery(con, "select * from last_db_update")[1, 1] ## TODO build other dates off of this one
if (db == "review") data_through_date <- sqlQuery(con_test, "select * from last_db_update")[1, 1]
if (db == "test") data_through_date <- sqlQuery(con, "select * from last_db_update")[1, 1] ## TODO build other dates off of this one

last_complete <- list()
last_complete$year <- year(data_through_date) - 1

# Date ranges for queries. DO NOT RENAME without changing .Rnw file too ----
ia_start_date <-  "2009-01-01"
ihs_start_date <- "2009-01-01"
ooh_start_date <- "2009-01-01"
context_date <- "2013-04-01" ## Using last quarter for context
context_date_string <- paste0("Quarter ", quarter(context_date), ", ", year(context_date))

ia_context_date <- "2012-12-01" ## Using last quarter for context
ia_context_date_string <- paste(month(ia_context_date, label = T, abbr = F),
                                 "1,", year(ia_context_date))

ia_end_date <- "2012-12-01"
ihs_end_date <- ooh_end_date <- "2013-05-01"
cohort_date <- "2010-01-01"
wb_start_date <- as.Date("2009-01-01")
wb_end_date <- as.Date(wb_start_date + duration(2, "years") - duration(1, "months"))

cohort_year <- year(cohort_date)

## Setting figure dimensions ----
fig_width <- 6
# Trend figure height is a*x + b where x is the number of graphs
trend_fig_a <- 0.7
trend_fig_b <- 2.8
# Context figure height is a_2 x_2 + a_1 x_1 + b where x_2 is the number of 2-liners and a_1 is the number of 1-liners
context_fig_a_2 <- 0.3
context_fig_a_1 <- 0.2
context_fig_b <- 1.2

## All subsequent chunks should be able to depend on this one alone. ----
require(xtable)
require(lubridate)
require(extrafont)
loadfonts(quiet = TRUE)

## xtable and ggplot options
options(xtable.NA.string = "NA")
dotplot_colors <- portal_colors[c(8, 4, 2)]
plot_title_size = 1.0 ## rel size

focus_county <- tolower(display_county)
focus_county_cd <- ref_lookup_county$county_cd[tolower(ref_lookup_county$county) == focus_county]
focus_info <- county_to_office(focus_county)
state_label <- "Washington"
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
region_info <- do.call(rbind, county_to_office_v(region_counties))
region_offices <- sort(unique(region_info$cd_office))
region_groups <- sort(unique(region_info$cd_office_county_grp))

## Background info
context_year <- strsplit(context_date, "-")[[1]][1]
if (db == "production" | db == "review") {
    focus_pop_person <- sqlQuery(con_test, paste0("call sp_population_person(", focus_county_cd, ",", context_year, ");"))$total
    focus_pop_house <- sqlQuery(con_test, paste0("call sp_population_household(", focus_county_cd, ",", context_year, ");"))$total
}
if (db == "test") {
    focus_pop_person <- sqlQuery(con, paste0("call sp_population_person(", focus_county_cd, ",", context_year, ");"))$total
    focus_pop_house <- sqlQuery(con, paste0("call sp_population_household(", focus_county_cd, ",", context_year, ");"))$total
}

####################################
#### Get data

#### Focus Data ####
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

#### Expand focus data (filling in 0's)
names(ia_focus) <- tolower(names(ia_focus))
ia_focus <- ia_focus[ia_focus$date_type == trend_date_type &
                         ia_focus$qry_type_poc2 == 0,
                     c("period_start",
                       "total cases first day",
                       "dcfs office")]

ia_focus <- fill_in(ia_focus, value_name = "total cases first day", fill_value=0)

names(ihs_focus) <- tolower(names(ihs_focus))
ihs_trend_data <- ihs_focus[ihs_focus$date_type == trend_date_type &
                                ihs_focus$qry_type_poc3 == 0,
                            c("period_start", "total cases first day",
                              "dcfs office")]

ihs_trend_data <- fill_in(ihs_trend_data, value_name = "total cases first day")

if (display_county != "Lincoln") {
    names(ooh_focus) <- tolower(names(ooh_focus))    
    ooh_focus <- ooh_focus[ooh_focus$date_type == trend_date_type & ooh_focus$qry_type_poc1 == 0,
                           c("month",
                             "total in care first day",
                             "county")]
    ooh_focus <- fill_in(ooh_focus, value_name = "total in care first day", fill_value = 0)
}    

#### Context Data ####
ia_region_call <- stored_procedure(sp = "poc2",
                                   date = ia_context_date,
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
ooh_safety_data <- ooh_safety_data[ooh_safety_data$date_type == 2 & ooh_safety_data$`Discharge Type` == "Reunification",
                                   c("County", "Discharge Type", "M24")]
levels(ooh_safety_data$County)[levels(ooh_safety_data$County) == "All"] <- state_label

ooh_safety_data <- dcast(ooh_safety_data, County ~ `Discharge Type`, value.var = "M24")

ooh_safety_data[, "Reunification"] <- round(ooh_safety_data[, "Reunification"], 1)
ooh_safety_data <- ooh_safety_data[order(-ooh_safety_data[, "Reunification"]), ]
for (i in 2:ncol(ooh_safety_data)) {
    ooh_safety_data[!is.na(ooh_safety_data[, i]), i] <- paste0(ooh_safety_data[!is.na(ooh_safety_data[, i]), i], "%")
}
names(ooh_safety_data)[2] <- "Re-entry from Reunification"
## Data processing with Adoption and Guardianship --deprecated
# ooh_safety_data[, 2:4] <- round(ooh_safety_data[, 2:4], 1)
# ooh_safety_data <- ooh_safety_data[order(-ooh_safety_data[, 4]), ]
# for (i in 2:ncol(ooh_safety_data)) {
#     ooh_safety_data[!is.na(ooh_safety_data[, i]), i] <- paste0(ooh_safety_data[!is.na(ooh_safety_data[, i]), i], "%")
# }
# ooh_safety_data <- ooh_safety_data[, c(1, 4, 2, 3)]


## Well-Being Data
wb_dates <- seq.Date(from = wb_start_date,
                     to = wb_end_date,
                     by = "month")
#county_arg <- c("pierce", "spokane", "all")
county_arg <- 0:39

wb_call <- stored_procedure(sp="PBCW3", date = wb_dates,
                            county = county_arg)

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

## Census Data Processing (for highilights and overview (appendix))

qf_focus <- quickfacts[c(which(quickfacts$text == display_county), 1), -1]
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

## Highlights
hl_date <- as.POSIXlt(context_date)
hl_ooh <- sqlQuery(con, stored_procedure(sp="poc1ab", type="counts", date=as.character(hl_date), county = focus_county_cd, age=0:1))
hl_ooh <- hl_ooh[hl_ooh$qry_type_poc1 == 0 & hl_ooh$date_type == 0, c("Total In Care First Day", "Age_Grouping_Cd")]

hl_ia <- sqlQuery(con, stored_procedure(sp = "poc2", type = "counts", date=as.character(hl_date), office = focus_office))
hl_ia <- sum(hl_ia[hl_ia$qry_type_poc2 == 0 & hl_ia$date_type == 0, c("Total Cases First Day")])

hl_ihs <- sqlQuery(con, stored_procedure(sp = "poc3", type = "counts", date=as.character(hl_date), office = focus_office))
hl_ihs <- sum(hl_ihs[hl_ihs$qry_type_poc3 == 0 & hl_ihs$date_type == 0, c("Total Cases First Day")])

odbcCloseAll()


## overview ----
## @knitr overview

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


## highlights ----
## @knitr highlights

hl <- qf_print[c(1, 3, 4), 1]

hl <- c("",
        hl,
        "",
        formatC(hl_ia, big.mark=","),
        formatC(hl_ihs, big.mark=","),
        formatC(hl_ooh[hl_ooh$Age_Grouping_Cd == 0, 1], big.mark = ","),
        paste0(round(100*hl_ooh[hl_ooh$Age_Grouping_Cd == 1, 1] / hl_ooh[hl_ooh$Age_Grouping_Cd == 0, 1]), "%"))
hl <- str_replace_all(hl, pattern="%", replacement="\\\\%")

hl_names <- c("\\textbf{U.S. Census Bureau (2012)}", 
              "\\quad Total Population",
              "\\quad Percent of Population Under 5 Years",
              "\\quad Percent of Population Under 18 Years",
              paste0("\\textbf{Child Well-Being Data Portal (", pretty_date(hl_date), ")}"),
              "\\quad Number of Open Investigations \\& Assessments",
              "\\quad Number of Open In-Home Service Cases",
              "\\quad Number of Open Out-of-Home Care Cases",
              "\\quad Percent of Out-of-Home Care Cases: Children Under 5 Years")

hl <- data.frame(v1 = hl_names, v2 = hl)

print.xtable(xtable(hl,
                    align="llr"),
             sanitize.text.function = as.character,
             include.rownames = FALSE, include.colnames = FALSE, booktabs = TRUE, floating = FALSE,
             hline.after = c(-1, nrow(hl)))


## ia_focus ----
## @knitr ia_focus    
trend_plot3(ia_focus, type = "ia",
            title_size = plot_title_size)


## ia_context ----
## @knitr ia_context
ia_region <- subset(ia_region, qry_type_poc2 == 0 & date_type == 0,
                    select = c("DCFS Office Group", "Total Cases First Day"))

levels(ia_region$`DCFS Office Group`)[levels(ia_region$`DCFS Office Group`) == "Pierce East, Pierce South & Pierce West (Pierce Cnty)"] <- "Pierce East & Pierce West (Pierce Cnty)"

context_plot(ia_region,
             focus = ifelse(str_detect(focus_group_tx, pattern = fixed("Pierce")),
                                       "Pierce East & Pierce West (Pierce Cnty)", focus_group_tx),
             colors = dotplot_colors,
             xlab = "Rate of Investigations & Assessments\n(per 1,000 Households)",
             title = paste0("Investigations & Assessments:\nRegion ", region_cd, ", ", ia_context_date_string),
             title_size = plot_title_size)


## ihs_focus ----
## @knitr ihs_focus
trend_plot3(ihs_trend_data, type = "ihs",
            title_size = plot_title_size)


## ihs_context----
## @knitr ihs_context
ihs_region <- subset(ihs_region, qry_type_poc3 == 0 & date_type == 1,
                     select = c("DCFS Office Group", "Total Cases First Day"))

levels(ihs_region$`DCFS Office Group`)[levels(ihs_region$`DCFS Office Group`) == "Pierce East, Pierce South & Pierce West (Pierce Cnty)"] <- "Pierce East & Pierce West (Pierce Cnty)"
context_plot(ihs_region, focus = ifelse(str_detect(focus_group_tx, pattern = fixed("Pierce")),
                                        "Pierce East & Pierce West (Pierce Cnty)", focus_group_tx),
             xlab = "Rate of In-Home Services (per 1,000 Households)",
             title = paste0("In-Home Services:\nRegion ", region_cd, ", ", context_date_string),
             colors = dotplot_colors,
             title_size = plot_title_size)


## ihs_safety ----
## @knitr ihs_safety
names(ihs_safety_data) <- c("", "Within 1 Year", "Within 2 Years")
ihs_safety_data <- ihs_safety_data[order(ihs_safety_data[, 2]), ]
ihs_table <- ihs_safety_data
ihs_table[, 2] <- paste0(round(ihs_safety_data[, 2]), "%")
ihs_table[, 3] <- paste0(round(ihs_safety_data[, 3]), "%")

ihs_cap <- paste("Percent of", cohort_year, "In-Home Service Cases Resulting in Out-of-Home Care Placement")

print.xtable(xtable(ihs_table,
                    caption = ihs_cap,
                    align = c("l", "l", "r", "r")),
             caption.placement = "top",
             include.rownames = FALSE,
             floating = TRUE,
             booktabs = TRUE)


## ooh_focus ----
## @knitr ooh_focus    
trend_plot3(ooh_focus, type = "ooh")


## ooh_context ----
## @knitr ooh_context
names(ooh_region) <- tolower(names(ooh_region))
ooh_region <- subset(ooh_region,
                     qry_type_poc1 == 0 & date_type == 1,
                     select = c("county", "total in care first day"))
ooh_region <- ooh_region[ooh_region$county %nin% omit_ooh_counties, , drop = TRUE]
context_plot(ooh_region, focus = display_county,
             xlab = "Rate of Out-of-Home Care (per 1,000 Children)",
             title = paste0("Out-of-Home Care:\nRegion ", region_cd, ", ", context_date_string),
             title_size = plot_title_size)

## ooh_safety----
## @knitr ooh_safety
ooh_safety_data$County <- as.character(ooh_safety_data$County)
ooh_safety_data$County[ooh_safety_data$County == display_county] <- paste0("\\textbf{", display_county, "}")
ooh_safety_data$County[ooh_safety_data$County == state_label] <- paste0("\\textbf{", state_label, "}")
rownames(ooh_safety_data) <- ooh_safety_data$County

ooh_safety_cap <- paste('Percentage of Children Re-Entering Out-of-Home Care within Two Years of Exiting Out-of-Home Care,',
                        cohort_year,
                        "Exit Cohort")

print.xtable(xtable(ooh_safety_data[ooh_safety_data[, 1] %nin% omit_ooh_counties, -1, drop = FALSE],
                    caption = ooh_safety_cap,
                    align = c("l", "r")),
             caption.placement = "top",
             include.rownames = TRUE,
             sanitize.rownames.function = as.character,
             floating = TRUE,
             booktabs = TRUE)

## ooh_outcomes----
## @knitr ooh_outcomes
outcome_plot_title <- paste("Outcomes Two Years After Entering\nOut-of-Home Care,",
                             cohort_year, "Entry Cohort")
ggplot(perm, aes(x = outcome, y = m24/100, fill = geo)) +
    theme_bw() +
    geom_bar(position = "dodge", stat = "identity") +
    scale_fill_manual(values=portal_colors[c(8, 3, 6)]) +
    scale_y_continuous(labels = percent) +
    labs(x = "",
         y = "Percent experiencing outcome",
         fill = "",
         title = outcome_plot_title) +
    theme(text = element_text(family = "Frutiger LT Std 45 Light"),
          axis.title = element_text(size = rel(0.8)),
          plot.title = element_text(size = rel(plot_title_size),
                                    vjust = 1,
                                    hjust = 0),
          axis.text.y = element_text(size = rel(0.8)),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1))

## ooh_wb ----
## @knitr ooh_wb
wb_title <- paste0("Kinship Care: Region ", region_cd,
                   "\nQuarter ", quarter(wb_start_date), ", ", year(wb_start_date),
                   " through ", "Quarter ", quarter(wb_end_date), ", ", year(wb_end_date))

ggplot(wb_context, aes(x = V1 / 100, y = county, color = context_highlight)) +
    geom_point(size = 5, shape = 18) +
    scale_color_manual(values = dotplot_colors) +
    labs(x = "Percent of Days Spent in Kinship Care",
         y = "",
         title = wb_title) +
    scale_x_continuous(labels = percent_format(), limits = c(.00, 1.15 * max(wb_context$V1 / 100)),
                       expand = c(0, 0)) +
    theme_bw() +
    theme(text = element_text(family = "Frutiger LT Std 45 Light"),
          legend.position = "none",
          axis.title = element_text(size = rel(0.8)),
          axis.text.y = element_text(size = rel(0.8)),
          plot.title = element_text(size = rel(plot_title_size), hjust = 0))



