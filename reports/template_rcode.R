# R code for county reports

## frontmatter ----
## @knitr frontmatter
library(pocr)
library(RODBC)
library(stringr)
library(lubridate)
library(xtable)
library(extrafont)
library(stringr)
library(dplyr)

#### Pick a database ####

# db <- "production"
#db <- "review"
db <- "test"

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


## Date stuff!

data_through_date <- sqlQuery(con, "select * from ref_last_dw_transfer")[1, 1]

last_complete <- list()
last_complete$year <- year(data_through_date) - 1

# Date ranges for queries. DO NOT RENAME without changing .Rnw file too ----
# data_through_date
dtd = as.Date(data_through_date)
start_date = floor_date(dtd, unit = "year") - years(3)

floor_quarter = function(x) {
   x = floor_date(x, unit = "month")
   q = ((month(x) - 1) %/% 3) + 1
   update(x, month = (q - 1) * 3 + 1)
}

ia_start_date  <- start_date
ihs_start_date <- start_date
ooh_start_date <- start_date

# Use last quarter for context date
context_date = floor_quarter(dtd) - months(3)
context_date_string <- paste0("Quarter ", quarter(context_date), ", ", year(context_date))

ooh_wb <- floor_quarter(dtd)
wb_start_date <- floor_quarter(dtd)

cohort_date <- floor_date(dtd, unit = "year") - years(3)
cohort_year <- year(cohort_date)

## Figure geometry

## Setting figure dimensions ----
fig_width <- 6
# Trend figure height is a*x + b where x is the number of graphs
trend_fig_a <- 0.7
trend_fig_b <- 2.8
# Context figure height is a * x + b where x is the number of 1-liners and b is an "intercept" minimum size
context_fig_a <- .2
context_fig_b <- 1.4


## All subsequent chunks should be able to depend on this one alone. ----

## xtable and ggplot options
options(xtable.NA.string = "NA")
dotplot_colors <- portal_colors[c(8, 4, 2)]
plot_title_size = 1.0 ## rel size
font = "Open Sans"

display_county <- 'King'

focus_county <- tolower(display_county)
focus_county_cd <- ref_lookup_county$county_cd[tolower(ref_lookup_county$county) == focus_county]
state_label <- "Washington"
omit_ooh_counties <- c("Adams", "Asotin", "Columbia", "Ferry", "Garfield",
                       "Klickitat", "Lincoln", "Pacific", "Pend Oreille",
                       "San Juan", "Skamania", "Wahkiakum")

## Get counties in same region
region_cd <- ref_lookup_county$region_cd[ref_lookup_county$county_cd == focus_county_cd]
region_counties <- ref_lookup_county[ref_lookup_county$region_cd %in% ref_lookup_county[tolower(ref_lookup_county$county) == focus_county, 3], 1]
region_counties_tx <- ref_lookup_county$county[region_counties]
#region_info <- do.call(rbind, county_to_office_v(region_counties))


## Background info
context_year <- year(context_date)

focus_pop_person <- sqlQuery(con, paste0("call sp_population_person(", focus_county_cd, ",", context_year, ");"))$total
focus_pop_house <- sqlQuery(con, paste0("call sp_population_household(", focus_county_cd, ",", context_year, ");"))$total

####################################
#### Get data

#### Focus Data ####
ia_call  <- stored_procedure("ia_trends_counts", county = focus_county)
ooh_call <- stored_procedure("ooh_pit_counts", county = c(focus_county))

ia_focus  <- sqlQuery(con, ia_call)
ooh_focus <- sqlQuery(con, ooh_call)

#### Expand focus data (filling in 0's)

ia_focus <- cr_clean(ia_focus)
ia_focus <- filter(ia_focus, date >= ooh_start_date)# & date <= ooh_end_date)

ooh_focus <- cr_clean(ooh_focus)
ooh_focus <- filter(ooh_focus, date >= ooh_start_date)# & date <= ooh_end_date)

#### Context Data ####

ia_region_call  <- stored_procedure("ia_trends_rates", county = c(0, region_counties))
ooh_region_call <- stored_procedure("ooh_pit_rates", county = c(0, region_counties))

ia_region  <- sqlQuery(con, ia_region_call)
ooh_region <- sqlQuery(con, ooh_region_call)

ia_region  <- cr_clean(ia_region)
ooh_region <- cr_clean(ooh_region)

ia_region_a <- filter(ia_region, date == max(date)) %>%
    select(county, opened.investigations.and.assessments) %>%
    mutate(county = gsub("All", "Washington", county)) %>%
    filter(county %nin% omit_ooh_counties) # might need to drop unused levels here

ia_context_date <- max(ia_region$date)
ia_context_date_string <- paste0("Quarter ", quarter(ia_context_date), ", ", year(ia_context_date))


## Permanency Data  -- uses cohort_date

perm_call <- list()
perm_call[1] <- stored_procedure("ooh_outcomes_24m", county = c("All", focus_county))
perm_call[2] <- stored_procedure("ooh_outcomes_24m", county = region_counties)

ent_query <- stored_procedure("ooh_flow_entries_counts", county = region_counties)
ent <- sqlQuery(con, ent_query)
ent <- cr_clean(ent, date.type = 2) 

perm1 <- sqlQuery(con, perm_call[[1]])
perm2 <- sqlQuery(con, perm_call[[2]])

perm1 <- cr_clean(perm1, date.type = 2)
perm2 <- cr_clean(perm2, date.type = 2)

cohort_period <- max(perm2$cohort.period)

perm_ent <- perm2 %>% left_join(ent) %>% filter(cohort.period == cohort_period) %>%
	group_by(discharge) %>%
	summarize(percent = weighted.mean(x = percent, w = number.of.entries)) %>%
	mutate(geo = paste('Region', region_cd)) %>%
    select(geo, discharge, percent)


f_county_perm <- perm1 %>% filter(cohort.period == cohort_period) %>%
			select(county, discharge, percent) 			
			
names(f_county_perm) <- names(perm_ent)

perm <- rbind(f_county_perm, perm_ent)

perm$discharge <- factor(perm$discharge, levels = c("Reunification", "Adoption", "Guardianship", "Emancipation",
                                                "Other", "Still in Out-of-Home Care"), ordered = T)

perm$geo <- gsub("All", "Washington", perm$geo)

## OOH Safety 

ooh_safety_call <- stored_procedure("ooh_reentry", date = cohort_date, county = c(0, region_counties))
ooh_safety_data <- sqlQuery(con, ooh_safety_call)
ooh_safety_data <- cr_clean(ooh_safety_data)

## Well-Being Data

wb_year = last_complete$year

wb_call <- stored_procedure("ooh_wb_familysettings", county = c(0, region_counties))
wb <- sqlQuery(con, wb_call)
wb <- cr_clean(wb) %>%
    filter(year(date) == wb_year)
wb <- wb[, c("date", "county", "family.setting..kin.placement.")]

wb_pop_call <- stored_procedure("ooh_pit_counts", county = c(0, region_counties))
wb_pop <- sqlQuery(con, wb_pop_call)
wb_pop <- cr_clean(wb_pop, date.type = 2)

wb <- inner_join(wb, wb_pop, by = c("date", "county"))

wb_context = wb %>% 
    filter(county %nin% omit_ooh_counties &
               county %in% region_counties_tx |
               county == "All")

#names(wb_context) <- c("county", "V1")
levels(wb_context$county)[levels(wb_context$county) == "All"] <- state_label
wb_context$county <- reorder(wb_context$county,
                             X = wb_context$family.setting..kin.placement.)

wb_context$context_highlight <- ifelse(wb_context$county == display_county, 1, 0)
wb_context$context_highlight[wb_context$county == state_label] <- 2
wb_context$context_highlight <- factor(wb_context$context_highlight)

# Census Data Processing (for highlights and overview (appendix))

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

hl_ooh <- stored_procedure("ooh_pit_counts", date = context_date, county = focus_county_cd, age=0:1)
hl_ooh <- sqlQuery(con, hl_ooh)
hl_ooh <- cr_clean(hl_ooh)

hl_ia <- stored_procedure("ia_trends_counts", date = context_date, county = focus_county_cd, age=0:1)
hl_ia <- sqlQuery(con, hl_ia)
hl_ia <- cr_clean(hl_ia)

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
      booktabs = TRUE,
      size = "\\fontsize{9pt}{10pt}\\selectfont")

## highlights ----
## @knitr highlights

hl_ia_a <- dplyr::filter(hl_ia, date == max(date))
# hl_ihs_a <- dplyr::filter(hl_ihs, date == context_date)
hl_ooh_a <- dplyr::filter(hl_ooh, date == max(date))

hl <- qf_print[c(1, 3, 4), 1]

hl <- c("",
        hl,
        "",
        formatC(hl_ia_a[1,4], big.mark = ","),
        # formatC(hl_ihs_a[1,6], big.mark=","),
        formatC(hl_ooh_a[hl_ooh_a$age.grouping.cd == 0, 4], big.mark = ","),
        paste0(round(100*hl_ooh_a[hl_ooh_a$age.grouping.cd == 1, 4] / hl_ooh_a[hl_ooh_a$age.grouping.cd == 0, 4]), "%"))
hl <- stringr::str_replace_all(hl, pattern = "%", replacement = "\\\\%")

hl_names <- c("\\textbf{U.S. Census Bureau (2012)}", 
              "\\quad Total Population",
              "\\quad Percent of Population Under 5 Years",
              "\\quad Percent of Population Under 18 Years",
              paste0("\\textbf{Child Well-Being Data Portal}"),
              paste0("\\quad Number of Open Investigations \\& Assessments, (", pretty_date(hl_ia_a$date[[1]]), ")"),
              paste0("\\quad Number of Open Out-of-Home Care Cases, (", pretty_date(hl_ooh_a$date[[1]]), ")"),
              paste0("\\quad Percent of Out-of-Home Care Cases: Children Under 5 Years, (", pretty_date(hl_ooh_a$date[[1]]), ")"))

hl <- data.frame(v1 = hl_names, v2 = hl)

print.xtable(xtable(hl, align = "llr"),
             sanitize.text.function = as.character,
             size = "\\small",
             include.rownames = FALSE, 
             include.colnames = FALSE, 
             booktabs = TRUE,
             floating = FALSE,
             hline.after = c(-1, nrow(hl)))

## ia_focus ----
## @knitr ia_focus
ia_focus_a <- ia_focus[,c(1:2)]
ia_focus_a$county <- paste(display_county, "County")
trend_plot(ia_focus_a, type = "ia",
            title_size = plot_title_size)


## ia_context ----
## @knitr ia_context

context_plot(ia_region_a,
             focus = display_county,
             colors = dotplot_colors,
             xlab = "Rate of Investigations & Assessments\n(per 1,000 Households)",
             title = paste0("Investigations & Assessments:\nRegion ", region_cd, ", ", ia_context_date_string),
             title_size = plot_title_size,
             font = font)

## ooh_focus ----
## @knitr ooh_focus   
 
ooh_focus_a <- ooh_focus
ooh_focus_a$geo = paste(display_county, "County")

trend_plot(ooh_focus_a, type = "ooh")

## ooh_context ----
## @knitr ooh_context

ooh_region_a <- dplyr::filter(ooh_region, date == context_date)	%>%
    select(county, total.in.out.of.home.care.1st.day) %>%
    mutate(county = str_replace(county, "All", "Washington"))

ooh_region_a <- ooh_region_a[ooh_region_a$county %nin% omit_ooh_counties, , drop = TRUE]

context_plot(ooh_region_a, focus = display_county,
             xlab = "Rate of Out-of-Home Care (per 1,000 Children)",
             title = paste0("Out-of-Home Care:\nRegion ", region_cd, ", ", context_date_string),
             title_size = plot_title_size)		 
			 
## ooh_safety----
## @knitr ooh_safety

# test <- ooh_safety_data

ooh_safety_data <- filter(ooh_safety_data,
                          cohort.entry.date == cohort_year,
                          discharge.type %in% c("Reunification", 'Guardianship', 'Adoption'),
                          months.since.exiting.out.of.home.care == 12,
                          ! county %in% omit_ooh_counties) %>%
                          group_by(county) %>%
                          summarize(`Re-Entry` = sum(re.entry.percent)) %>%
                          arrange(`Re-Entry`) %>%
                          rename(County = county) %>%
                          mutate(`Re-Entry` = paste0(round(`Re-Entry`), '%')
                                 ,County = str_replace(County, 'All', '\\\\textbf{Washington}')
                                 ,County = ifelse(County == display_county, paste0("\\textbf{", display_county, "}"), County)) %>%
                          as.data.frame()

ooh_safety_cap <- paste('Percentage of Children Re-Entering Out-of-Home Care within One Year of Exiting Out-of-Home Care,',
                        cohort_year,
                        "Exit Cohort")

row.names(ooh_safety_data) <- ooh_safety_data[[1]]

print.xtable(xtable(ooh_safety_data[, -1, drop = FALSE],
                    caption = ooh_safety_cap),
                    align = c("l", "r"),
             caption.placement = "top",
             include.rownames = TRUE,
             sanitize.rownames.function = as.character,
             floating = TRUE,
             booktabs = TRUE)

## ooh_outcomes----
## @knitr ooh_outcomes

outcome_plot_title <- paste("Outcomes Two Years After Entering\nOut-of-Home Care,",
                             cohort_year, "Entry Cohort")
ggplot(perm, aes(x = discharge, y = percent/100, fill = geo)) +
    theme_bw() +
    geom_bar(position = "dodge", stat = "identity") +
    scale_fill_manual(values = dotplot_colors) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "",
         y = "Percent experiencing outcome",
         fill = "",
         title = outcome_plot_title) +
    theme(text = element_text(family = font),
          axis.title = element_text(size = rel(0.8)),
          plot.title = element_text(size = rel(plot_title_size),
                                    vjust = 1,
                                    hjust = 0),
          axis.text.y = element_text(size = rel(0.8)),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1))  
		  
## ooh_wb ----
## @knitr ooh_wb

# wb_title <- paste0("Kinship Care: Region ", region_cd,
                   # "\nQuarter ", quarter(wb_start_date), ", ", year(wb_start_date),
                   # " through ", "Quarter ", quarter(wb_end_date), ", ", year(wb_end_date))

wb_title <- paste0("Kinship Care, ", pretty_date(wb_context$date[1]), ": Region ", region_cd)
				   
ggplot(wb_context, aes(x = family.setting..kin.placement. / 100,
                       y = county, color = context_highlight)) +
    geom_point(size = 5, shape = 18) +
    scale_color_manual(values = dotplot_colors) +
    labs(x = "Percent of Children in Kinship Care",
         y = "",
         title = wb_title) +
    scale_x_continuous(labels = scales::percent,
                       limits = c(.00, 1.15 * max(wb_context$family.setting..kin.placement. / 100)),
                       expand = c(0, 0)) +
    theme_bw() +
    theme(text = element_text(family = font),
          legend.position = "none",
          axis.title = element_text(size = rel(0.8)),
          axis.text.y = element_text(size = rel(0.8)),
          plot.title = element_text(size = rel(plot_title_size), hjust = 0))
