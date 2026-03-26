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
library(sf)
library(scales)
library(reshape2)
library(rnaturalearth)
library(rnaturalearthdata)

# Set up plot aesthetics (ggplot theme, color palette)
source(here("R", "aesthetics.R"))


# Set up and run empirical COG; clean up data ---------------------------------
# TODO: Set survey area
survey <- c("AI", "GOA")[1]

# Calculate empirical cog using R script.
source(here("R", "cog.R"))

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
ts_plot <- ggplot(cogs_plot, aes(x = year, y = est)) +
  geom_line(aes(color = species_code)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species_code), alpha = 0.4) +
  xlab("Year") + ylab("Weighted Mean") +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = function(est) abs(est)) +
  facet_wrap(~ metric, scales = "free_y") 
ts_plot  # view plot


# Sparkleplot (bivariate scatter plot for lat & lon) --------------------------
# Transform Alaska Albers to latitude/longidue (for estimates, upper & lower bounds)
coord_transform <- function(column, crs) {
  # Pull out geographic info and convert back to m so conversion will work
  aa <- bind_cols(Y = cogs_plot[cogs_plot$metric == "Northings (km)", column] * 1000,
                  X = cogs_plot[cogs_plot$metric == "Eastings (km)", column] * 1000)
  latlon <- sf::st_as_sf(aa,
                         coords = c("X", "Y"),
                         crs = 3338)
  latlon <- sf::st_transform(latlon, crs = crs)
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

coord_out <- cbind.data.frame(coord_transform("est", 4326),
                              se = coord_transform("se", 4326)$se,
                              lwr = coord_transform("lwr", 4326)$lwr,
                              upr = coord_transform("upr", 4326)$upr)

cog_lat <- coord_out[coord_out$metric == "Latitude", c(1:4, 6:7)]
colnames(cog_lat)[4:6] <- c("est_lat", "lwr_lat", "upr_lat")
cog_lon <- coord_out[coord_out$metric == "Longitude", c(1:4, 6:7)]
colnames(cog_lon)[4:6] <- c("est_lon", "lwr_lon", "upr_lon")

# Combine with latitude
cog_latlon <- cog_lat %>% 
  left_join(cog_lon, by = c("species_code", "year")) %>% 
  arrange(year)

if(survey == "AI") {
  # Correct "wrapping" around the IDL (only necessary for the AI)
  cog_sparkle <- cog_lon %>%
    mutate(across(c(est_lon, lwr_lon, upr_lon), 
                  ~case_when(. > 0 ~ (((180 - .) + 180) * -1), 
                             TRUE  ~ .))) %>%
    left_join(cog_lat, by = c("species_code", "year")) %>% 
    arrange(year)
}

if(survey == "GOA") {
  # No correction needed, continue with original coordinates
  cog_sparkle <- cog_latlon
}

# Build layered plot, so more recent years are on top
# Build the base plot with facets and scales (but no data layers yet)
years_ordered <- sort(unique(cog_sparkle$year)) # List of years
sparkle <- ggplot(cog_sparkle, aes(color = year)) +
  scale_color_viridis(name = "Year", option = "plasma", end = 0.9) +
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

# Set x-axis labels based on survey region
if(survey == "GOA") {
  sparkle <- sparkle + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3))
}
if(survey == "AI") {
  sparkle <- sparkle + 
    scale_x_continuous(breaks = c(-185, -180, -175), labels = c(175, 0, -175)) 
}
sparkle  # view plot


# Maps ------------------------------------------------------------------------
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
sf::sf_use_s2(FALSE) # turn off spherical geometry

if(survey == "AI") {
  # Subset world to the Aleutians (split along IDL, then stitch back together)
  western_half <- sf::st_crop(world, xmin = 170, xmax = 180, ymin = 48, ymax = 55)
  eastern_half <- sf::st_crop(world, xmin = -180, xmax = -170, ymin = 48, ymax = 55)
  map_bg <- bind_rows(western_half, eastern_half) %>%
    sf::st_shift_longitude()
  
  # Transform data to continuous scale across date line for plotting
  map_data <- cog_latlon %>%
    mutate(x_plot = ifelse(est_lon < 0, est_lon + 360, est_lon),
           xmin_plot = ifelse(lwr_lon < 0, lwr_lon + 360, lwr_lon),
           xmax_plot = ifelse(upr_lon < 0, upr_lon + 360, upr_lon),
           y_plot = est_lat)
  
  # Set x-axis scale - THIS DOESN'T REALLY WORK
  x_scale <- scale_x_continuous(breaks = c(175, 180, 185, 190))
  # Map view limits (using 360 scale for X)
  map_coord <- coord_sf(xlim = c(173, 190), ylim = c(50, 55), expand = FALSE, datum = NULL)
  
} 

if(survey == "GOA") {
  map_bg <- world
  
  # Prepare GOA data (standard coordinates)
  map_data <- cog_latlon %>%
    mutate(x_plot = est_lon, xmin_plot = lwr_lon, xmax_plot = upr_lon, y_plot = est_lat)
  
  x_scale <- scale_x_continuous(breaks = c(-160, -145))
  map_coord <- coord_sf(xlim = c(-162.5, -140), ylim = c(54, 60), expand = FALSE)
}

# Build layered plot, so more recent years are on top
years_ordered <- sort(unique(map_data$year))

year_layers <- purrr::map(years_ordered, ~{
  year_subset <- filter(map_data, year == .x)
  list(
    geom_errorbar(data = year_subset, 
                  aes(x = x_plot, ymin = lwr_lat, ymax = upr_lat, color = year), 
                  alpha = 0.4, 
                  width = 0, 
                  orientation = "x"),
    geom_errorbar(data = year_subset, 
                  aes(y = y_plot, xmin = xmin_plot, xmax = xmax_plot, color = year), 
                  alpha = 0.4,
                  width = 0, 
                  orientation = "y"),
    geom_point(data = year_subset, 
               aes(x = x_plot, y = y_plot, color = year), size = 1.5)
  )
})

# Assemble the final plot
map <- ggplot() +
  geom_sf(data = map_bg, fill = "grey90", color = "grey60") +
  year_layers + # The ordered layers from purrr
  facet_wrap(~species_code, ncol = 2) +
  scale_color_viridis(name = "Year", option = "plasma", end = 0.9) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  x_scale +
  map_coord +
  xlab("") + ylab("")

# Add landmark label (Adak Island) for the AI maps
if(survey == "AI") {
  adak <- data.frame(label = "Adak", x = (-176.66 + 360), y = 51.88)
  map <- map +
    # First, remove lat & lon labels, because they don't really help!
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    ggrepel::geom_text_repel(data = adak, aes(x = x, y = y, label = label),
                             size = 3, color = "grey40",
                             nudge_x = 1, nudge_y = -0.7, 
                             segment.color = "grey40", segment.size = 0.3, 
                             direction = "both")
}

map  # view plot

# Save plots ------------------------------------------------------------------
# Create a directory for latest GOA survey year if it doesn't already exist
dir <- here("output", paste0("plots ", yr))
if (!dir.exists(dir)) {
  dir.create(dir)
}

ggsave(ts_plot, filename = here(dir, paste0("rf_cog_ts_", survey, "_", ".png")), 
       width = 200, height = 110, unit = "mm", dpi = 300)
ggsave(sparkle, filename = here(dir, paste0("rf_cog_sparkle_", survey, "_", ".png")), 
       width = 180, height = 150, unit = "mm", dpi = 300)
ggsave(map, filename = here(dir, paste0("rf_cog_map_", survey, "_", ".png")), 
       width = 180, height = 140, unit = "mm", dpi = 300)
