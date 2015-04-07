require(poc)
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


# wb_5$window <- 5

# wb_1 <- ddply(wb[wb$month > as.POSIXlt("2012-01-01"), ], .variables= "county", .fun= wb_fn)
# wb_1$window <- 1
# 
# wb <- rbind(wb_5, wb_1)
# 
# wb_1_lincoln <- wb[1, ]
# wb_1_lincoln$county <- "Lincoln"
# wb_1_lincoln$V1 <- NA
# wb_1_lincoln$window <- 1
# wb <- rbind(wb, wb_1_lincoln)
# 
# wb <- wb[order(wb$window, wb$V1, na.last = FALSE), ]
# wb$county <- factor(wb$county, ordered = TRUE, levels = wb$county[1:40])
# 
# ggplot(wb, aes(x = V1, y = county, color = as.factor(window))) +
#     geom_point(alpha = 0.8, size = 5, shape = 18) +
#     labs(x = "Average percent of Care-days spent with Kin",
#          color = "Over the last X years",
#          y = "") +
#     theme_bw()

## Save wb_5 and include it in the county_report package

wb_context <- wb_5[wb_5$county %in% c(region_counties_tx[region_counties_tx %nin% omit_ooh_counties], "All"), , drop = TRUE]
levels(wb_context$county)[levels(wb_context$county) == "All"] <- state_label
wb_context$county <- reorder(wb_context$county, X=wb_context$V1)

wb_context$context_highlight <- factor(ifelse(wb_context$county == display_county, 1, 0))
wb_context$state_highlight <- factor(ifelse(wb_context$county == state_label, 1, 0))

ggplot(wb_context, aes(x = V1 / 100, y = county, color = context_highlight,
                       shape = state_highlight)) +
    geom_point(alpha = 0.8, size = 5) +
    scale_shape_manual(values = c(18, 16)) +
    scale_color_manual(values = portal_colors[c(8, 4)]) +
    labs(x = "Percent of Care-days spent in Kinship care (2008-2012)",
         y = "") +
    scale_x_continuous(labels = percent_format(), limits = c(.01, max(wb_context$V1 / 100))) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_text(size = rel(0.8)),
          axis.text.y = element_text(size = rel(0.8)),
          title = "Kinship Care as an Estimate of Wellbeing")
