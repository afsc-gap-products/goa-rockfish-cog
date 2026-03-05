#' Plots of empirical center of gravity for rockfish. COG is generated in the 
#' cog.R script. 
#' 
#' There are three plot types produced in this script:
#' 1. Time series plot of depth, bottom temperature, eastings, and northings
#'    for all species to facilitate direct comparisons.
#' 2. 'Sparkle' plot - bivariate scatterplot of latitude and longitude to 
#'    demonstrate changes in relative COG over time.
#' 3. Map of COG relative to the coastline. Most useful for nearshore 
#'    species with limited distributions. May be misleading for stocks where
#'    the empirical COG is outside of the survey domain.
#'    
#' By: Sophia N. Wassermann

library(dplyr)
library(ggplot2)
library(viridis)
library(here)
library(sp)
library(sf)
library(scales)
library(reshape2)
library(rnaturalearth)
library(rnaturalearthdata)

# Set ggplot theme
# theme_sleek() from ggsidekick (github.com/seananderson/ggsidekick)
theme_sleek <- function(base_size = 11, base_family = "") {
  half_line <- base_size/2
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "grey30"),
      strip.text.y = element_text(colour = "grey30"),
      axis.text = element_text(colour = "grey30"),
      axis.title = element_text(colour = "grey30"),
      legend.title = element_text(colour = "grey30", size = rel(0.9)),
      panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 1),
      legend.key.size = unit(0.9, "lines"),
      legend.text = element_text(size = rel(0.7), colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = "grey30", size = rel(1)),
      plot.subtitle = element_text(colour = "grey30", size = rel(.85))
    )
}

theme_set(theme_sleek())

# Data import & cleaning ------------------------------------------------------
# Either read in existing COG file, or calculate empirical cog using R script.
## TODO: Set survey area
survey <- c("AI", "GOA")[1]

## TODO: Set latest GOA or AI survey year
yr <- 2024

if (file.exists(here("output", paste0("rf_cogs_", survey, "_", yr, ".csv")))) {
  cogs <- read.csv(here("output", paste0("rf_cogs_", survey, "_", yr, ".csv")))
} else {
  # TODO: make sure you've updated cog.R to pull the correct survey data!
  source(here("R", "cog.R"))
}

# Update dataframe to have common names, cleaner labels, correct axis
cogs_plot <- cogs %>%
  mutate(species_code = factor(species_code)) %>%
  mutate(species_code = case_when(
    species_code == 30020 ~ "Shortspine Thornyhead",
    species_code == 30050 ~ "Rougheye & Blackspotted",
    species_code == 30060 ~ "Pacific Ocean Perch",
    species_code == 30152 ~ "Dusky Rockfish",
    species_code == 30420 ~ "Northern Rockfish",
    species_code == 30576 ~ "Shortraker Rockfish",
  )) %>%
  mutate(metric = case_when(
    metric == "BOTTOM_TEMPERATURE_C" ~ "Bottom Temp (\u00B0C)",
    metric == "DEPTH_M" ~ "Depth (m)",
    metric == "X" ~ "Eastings (km)",
    metric == "Y" ~ "Northings (km)",
    )) %>%
  # Remove first two dusky points (1990 & 1993; data should start in 1996)
  filter(!(species_code == "Dusky Rockfish" & year <= 1996)) %>% 
  # Make depth estimates negative for inverted axis when plotting
  mutate(est = if_else(metric == "Depth (m)", -est, est),
         upr = if_else(metric == "Depth (m)", -upr, upr),
         lwr = if_else(metric == "Depth (m)", -lwr, lwr))

# Time series plot ------------------------------------------------------------
# Special colors from naturalparkcolors::park_palette("Saguaro)
pal <- c("#847CA3", "#E45A5A", "#F4A65E", "#80792B", "#F2D56F", "#1A1237")

ts_plot <- ggplot(cogs_plot, aes(x = year, y = est)) +
  geom_line(aes(color = species_code)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species_code), alpha = 0.4) +
  xlab("Year") + ylab("Weighted Mean") +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = function(est) abs(est)) +
  facet_wrap(~ metric, scales = "free_y") 
ts_plot

# Sparkleplot (bivariate scatter plot for lat & lon) --------------------------
# Transform UTM to latitude/longidue (for estimates, upper & lower bounds)
coord_transform <- function(column) {
  utm <- data.frame(Y = cogs_plot[cogs_plot$metric == "Northings (km)", column],
                    X = cogs_plot[cogs_plot$metric == "Eastings (km)", column])
  latlon <- sf::st_as_sf(utm,
                         coords = c("X", "Y"),
                         crs = "+proj=utm +zone=5 +datum=WGS84 +units=km")
  latlon <- sf::st_transform(latlon, crs = 4326)
  latlon <- data.frame(sf::st_coordinates(latlon))
  latlon_out <- cbind.data.frame(species_code = cogs_plot[cogs_plot$metric == "Eastings (km)", "species_code"],
                                 year = cogs_plot[cogs_plot$metric == "Eastings (km)", "year"],
                                 Latitude = latlon$Y,
                                 Longitude = latlon$X)
  latlon_out <- reshape2::melt(latlon_out, 
                               id.vars = c("species_code", "year"), 
                               variable.name = "metric",
                               value.name = column)
  return(latlon_out)
}

coord_out <- cbind.data.frame(coord_transform("est"),
                              se = coord_transform("se")$se,
                              lwr = coord_transform("lwr")$lwr,
                              upr = coord_transform("upr")$upr)

cog_lat <- coord_out[coord_out$metric == "Latitude", c(1:4, 6:7)]
colnames(cog_lat)[4:6] <- c("est_lat", "lwr_lat", "upr_lat")
cog_lon <- coord_out[coord_out$metric == "Longitude", c(1:4, 6:7)]
colnames(cog_lon)[4:6] <- c("est_lon", "lwr_lon", "upr_lon")

# Correct "wrapping" around the IDL
cog_lon <- cog_lon %>%
  mutate(across(c(est_lon, lwr_lon, upr_lon), 
                ~case_when(. > 0 ~ (((180 - .) + 180) * -1), 
                           TRUE  ~ .)))

# Combine with latitude and plot
cog_sparkle <- cog_lat %>% left_join(cog_lon, by = c("species_code", "year")) %>% arrange(year)
  
# List of years
years_ordered <- sort(unique(cog_sparkle$year))

# Build the base plot with facets and scales (but no data layers yet)
sparkle <- ggplot(cog_sparkle, aes(color = year)) +
  scale_color_viridis(name = "Year", option = "plasma", end = 0.9) +
  scale_x_continuous(breaks = c(-185, -180, -175), labels = c(175, 0, -175)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  facet_wrap(~species_code, ncol = 2) +
  labs(x = "Longitude (\u00B0W)", y = "Latitude (\u00B0N)")

# Create a list of error bars & points for each year
year_layers <- purrr::map(years_ordered, ~{
  year_data <- filter(cog_sparkle, year == .x)
  list(
    geom_errorbar(data = year_data, 
                  aes(x = est_lon, ymin = lwr_lat, ymax = upr_lat), 
                  alpha = 0.4, 
                  orientation = "x", 
                  width = 0),
    geom_errorbar(data = year_data, 
                  aes(y = est_lat, xmin = lwr_lon, xmax = upr_lon), 
                  alpha = 0.4, 
                  orientation = "y",
                  width = 0),
    geom_point(data = year_data, aes(x = est_lon, y = est_lat))
  )
})

# Add layers to the plot
sparkle <- sparkle + year_layers
sparkle


# Maps ------------------------------------------------------------------------
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
sf::sf_use_s2(FALSE)  # turn off spherical geometry

map <- ggplot() +
  geom_sf(data = world) +
  geom_errorbar(
    data = dplyr::filter(cog_sparkle, year ==  unique_years[1]), 
    mapping = aes(x = est_lon, ymin = lwr_lat, ymax = upr_lat, color = year), 
    alpha = 0.4,
    width = 0) +
  geom_errorbarh(
    data = dplyr::filter(cog_sparkle, year ==  unique_years[1]),
    mapping = aes(xmin = lwr_lon, xmax = upr_lon, y = est_lat, color = year), 
    alpha = 0.4,
    width = 0
  ) +
  geom_point(
    data = dplyr::filter(cog_sparkle, year ==  unique_years[1]), 
    mapping = aes(x = est_lon, y = est_lat, color = year),
    size = 1.5
  )
  

for(ii in 2:length(unique_years)) {
  map <- 
    map +
    geom_errorbar(
      data = dplyr::filter(cog_sparkle, year ==  unique_years[ii]), 
      mapping = aes(x = est_lon, ymin = lwr_lat, ymax = upr_lat, color = year), 
      alpha = 0.4,
      width = 0) +
    geom_errorbarh(
      data = dplyr::filter(cog_sparkle, year ==  unique_years[ii]),
      mapping = aes(xmin = lwr_lon, xmax = upr_lon, y = est_lat, color = year), 
      alpha = 0.4,
      width = 0
    ) +
    geom_point(
      data = dplyr::filter(cog_sparkle, year ==  unique_years[ii]), 
      mapping = aes(x = est_lon, y = est_lat, color = year),
      size = 1.5
    )
  
}

map <- map + 
  coord_sf(xlim = c(-180, -140), ylim = c(54, 60), expand = FALSE) +
  scale_color_viridis(name = "Year", option = "plasma", discrete = FALSE, end = 0.9) +
  scale_x_continuous(breaks = c(-160, -145)) +
  scale_y_continuous(breaks = c(55, 60)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~species_code, ncol = 2)
map

# Save plots ------------------------------------------------------------------
# Create a directory for latest GOA survey year if it doesn't already exist
dir <- here("output", paste0("plots ", yr_goa))
if (!dir.exists(dir)) {
  dir.create(dir)
}

ggsave(ts_plot, filename = here(dir, "rf_cog_ts.png"), 
       width = 200, height = 110, unit = "mm", dpi = 300)
ggsave(sparkle, filename = here(dir, "rf_cog_sparkle.png"), 
       width = 180, height = 150, unit = "mm", dpi = 300)
ggsave(map, filename = here(dir, "rf_cog_map.png"), 
       width = 180, height = 140, unit = "mm", dpi = 300)
