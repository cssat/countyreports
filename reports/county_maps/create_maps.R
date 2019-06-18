# script for creatin county maps

# required pacakges

library(extrafont)
library(ggplot2)
library(Hmisc)
library(pocr)
library(dplyr)

regions <- 
    select(ref_lookup_county_region, county, region_6_cd) %>%
    filter(region_6_cd != 0) %>%
    mutate(county = tolower(county)
           , region = ifelse(region_6_cd %in% c(1, 2), 'Regions 1 and 2', region_6_cd)
           , region = ifelse(region %in% c(3, 4), 'Regions 3 and 4', region)
           , region = ifelse(region %in% c(5, 6), 'Regions 5 and 6', region)
           , highlight_county = region
           )

map_data <-
    map_data(map = "county") %>%
    filter(region == "washington") %>%
    rename(state = region, county = subregion) %>%
    left_join(regions) %>%
    mutate(county = tools::toTitleCase(county))

for (i in 1:NROW(regions)) {
    map_data$highlight_county <- ifelse(tools::toTitleCase(regions$county[[i]]) == map_data$county
                                       , paste0(map_data$county, " County ", "(Region ", map_data$region_6_cd, ")")
                                       , map_data$highlight_county
                                       )
    
    map_data$highlight_county <- factor(map_data$highlight_county, levels = c(unique(map_data$region), as.character(unique(map_data$highlight_county[str_detect(map_data$highlight_county, "County")]))))

    highlight_map <-
        ggplot(map_data, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = highlight_county), color = "white") +
        coord_map(projection = "globular") +
        labs(x = "", y = "") +
        scale_fill_manual(values = c("#6DB33F", "#B1662B", "#6E9CAE", "grey")) +
        guides(fill = guide_legend(nrow = 2)) +
        theme_minimal() +
        theme(panel.grid.major = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.title = element_blank(),
              legend.position = "bottom",
              text = element_text(family = "Open Sans"),
              legend.text = element_text(size = 20),
              legend.key.size = unit(2,"line")
        )

    map_data$highlight_county <- map_data$region

    # print(highlight_map)

    ggsave(filename = paste0(tools::toTitleCase(regions$county[[i]]), "-b.pdf"), plot = highlight_map, dpi = 320)    
}



