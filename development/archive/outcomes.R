## outcomes for county reports

out_query <- stored_procedure(sp = "PBCP5", county = 0:39, date = "2009-01-01", age = 1:4)

out <- sqlQuery(con, out_query)
out <- out[out$date_type == 2, c("County", "Discharge Type", "M24")]
names(out)[2] <- "type"

out_plot <- out[out$County %in% c(region_counties_tx, "All"), ]

ggplot(out_plot, aes(y = M24 / 100, x = County, fill = type, group = type)) +
    geom_bar(position = "dodge", stat = "identity") +
    facet_wrap(~type, scales="free_y") + 
    scale_y_continuous(labels = percent_format()) +
    theme_bw() +
    labs(y = "Percent experiencing outcome")