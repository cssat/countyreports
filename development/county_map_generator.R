require(pocr)
require(rgdal)
require(gpclib)
gpclibPermit()

county<- readOGR(dsn = "S:/Data Portal/geo_data", layer = "wa_counties")
names(county@data)[4] <- "id"
county_region <- ref_lookup_county
names(county_region)[2] <- "id"
county@data <- merge(county@data, county_region)

county_gg <- fortify(county, region = "id")
county_gg <- merge(county_gg, county_region)
county_gg$region_cd <- factor(county_gg$region_cd)

# display_county <- "San Juan"
# display_county <- "Pierce"

setwd("S:/Data Portal/county_reports/development/")

## This for loop has legend on the right ----
for (display_county in ref_lookup_county$county[1:39]) {
    display_region <- ref_lookup_county$region_cd[ref_lookup_county$county == display_county]
    county_gg$display <- paste("Region", county_gg$region_cd)
    county_label <- paste0(display_county, " County (Region ", display_region, ")")
    county_gg$display[county_gg$id == display_county] <- county_label
    county_gg$display <- factor(county_gg$display,
                                levels = c("Region 1", "Region 2", "Region 3", county_label),
                                ordered = TRUE)

    county_map <- ggplot(county_gg, aes(long, lat, group = group, fill = display)) +
        geom_polygon(color = "white", size = 0.2) +
        coord_map(projection = "azequalarea") +
        scale_fill_manual(values = portal_colors[c(2:4, 12)], name = "") +
#         geom_polygon(color = NA,
#                      fill = portal_colors[12],
#                      size = 0.2,
#                      data = county_gg[county_gg$id == display_county, ]) +
        theme_clean() +
        theme(plot.margin = unit(rep(0, 4), "points")) +
        guides(fill = guide_legend(override.aes = list(colour = NA))) 
    # county_map
    save_location <- str_replace(paste0("county_maps/", display_county, ".pdf"),
                                 pattern = " ", replacement = "_")

    ggsave(save_location, county_map, height = 2.8, width = 6)    
    
}

## This one has legend below ----
for (display_county in ref_lookup_county$county[1:39]) {
    display_region <- ref_lookup_county$region_cd[ref_lookup_county$county == display_county]
    county_gg$display <- paste("Region", county_gg$region_cd)
    county_label <- paste0(display_county, " County (Region ", display_region, ")")
    county_gg$display[county_gg$id == display_county] <- county_label
    county_gg$display <- factor(county_gg$display,
                                levels = c("Region 1", "Region 2", "Region 3", county_label),
                                labels = c("Region 1     ", "Region 2", "Region 3", county_label),
                                ## Note: whitespace added to keep the legend columns separate
                                ordered = TRUE)
    
    county_map <- ggplot(county_gg, aes(long, lat, group = group, fill = display)) +
        geom_polygon(color = "white", size = 0.2) +
        coord_map(projection = "azequalarea") +
        scale_fill_manual(values = portal_colors[c(2:4, 12)],
                          name = "") +
        #         geom_polygon(color = NA,
        #                      fill = portal_colors[12],
        #                      size = 0.2,
        #                      data = county_gg[county_gg$id == display_county, ]) +
        theme_clean() +
        theme(plot.margin = unit(rep(0, 4), "points"),
              legend.position = "bottom") +
			  guides(fill = guide_legend(override.aes = list(colour = NA), ncol = 2))
    county_map

    save_location <- paste0("S:/Data Portal/county_reports/development/county_maps/",
                            str_replace(paste0(display_county, "-b", ".pdf"),
                                        pattern = " ", replacement = "_"))                                 
    ggsave(save_location, county_map, height = 3, width = 4)    
    save_location <- paste0("S:/Data Portal/county_reports/reports/county_maps/",
                            str_replace(paste0(display_county, "-b", ".pdf"),
                                        pattern = " ", replacement = "_"))                                 
    ggsave(save_location, county_map, height = 3, width = 4)        
}


