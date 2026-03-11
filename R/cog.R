## Calculate Empirical Center of Gravity From Observed Data
## Zack Oyafuso

# Install required packages
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

if (!requireNamespace("gapindex", quietly = TRUE)) {
  devtools::install_github("afsc-gap-products/gapindex")
}

library(gapindex)

## Connect to Oracle
if (file.exists("Z:/Projects/ConnectToOracle.R")) {
  source("Z:/Projects/ConnectToOracle.R")
} else {
  # For those without a ConnectToOracle file
  channel <- gapindex::get_connected(check_access = FALSE)
}

## Define species and species groupings
rf_groups <- data.frame(
  GROUP_CODE   = c(30060, 30050, 30050, 30050, 30576, 30420, 30152, 30020),
  SPECIES_CODE = c(30060, 30050:30052, 30576, 30420, 30152, 30020)
)

## Pull data
if(survey == "GOA") {
  gp_data <- 
    gapindex::get_data(year_set = c(seq(from = 1990, to = 1999, by = 3),
                                    seq(from = 2003, to = yr, by = 2)),
                       survey_set = survey,
                       spp_codes = rf_groups,
                       channel = channel
    )
} 

if(survey == "AI") {
  gp_data <- 
    gapindex::get_data(year_set = c(seq(from = 1991, to = 2000, by = 3),
                                    seq(from = 2002, to = yr, by = 2)),
                       survey_set = survey,
                       spp_codes = rf_groups,
                       channel = channel
    )
}

## Calculate cpue
gp_cpue <- gapindex::calc_cpue(gapdata = gp_data) |> as.data.frame()

calc_weighted_mean <- function(x, w, lwr_p = 0.025, upr_p = 0.975) {
  ## Count the number of records that have a positive weight (w) 
  ## and a metric (some metrics are missing from some hauls) 
  n <- length(x = w[w > 0 & !is.na(x = w) & !is.na(x)])
  w <- w / sum(w, na.rm = TRUE) # weight add up to 1
  
  ## Calculate weighted mean and standard error
  weighted_mean <- weighted.mean(x = x, 
                                 w = w, 
                                 na.rm = TRUE)
  
  weighted_var <- sum(w * ((x - weighted_mean)^2), na.rm = TRUE) * (n / (n - 1))
  weighted_se <- sqrt(weighted_var / n)
  
  ## Calculate confidence interval of the weighted mean estimate
  ci <- qnorm(p = c(lwr_p, upr_p), 
              mean = weighted_mean, 
              sd = weighted_se)
  
  return(data.frame(est = weighted_mean, 
                    se = weighted_se, 
                    lwr = ci[1], 
                    upr = ci[2])
  )
}

## Translate latitude and longitdue to UTM
utm <- sf::st_as_sf(
  cbind.data.frame(X = gp_cpue$LONGITUDE_DD_START, 
                   Y = gp_cpue$LATITUDE_DD_START), 
  coords = c("X", "Y"), 
  crs = 4326)
utm <- sf::st_transform(utm, "+proj=utm +zone=5 +datum=WGS84 +units=km")
utm <- data.frame(sf::st_coordinates(utm))

gp_cpue <- cbind.data.frame(gp_cpue, X = utm$X, Y = utm$Y)

## Loop over metrics and calculate weighted means, SEs, and CIs 
## for each species and year
cogs <- data.frame()
for (imetric in c("DEPTH_M", "BOTTOM_TEMPERATURE_C", "X", "Y")){
  cogs <- 
    rbind(cogs,
          data.frame(
            metric = imetric,
            do.call(
              what = rbind,
              args = lapply(
                X = split(x = gp_cpue, 
                          f = list(gp_cpue$SPECIES_CODE, gp_cpue$YEAR)),
                FUN = function(df){
                  data.frame(cbind(species_code = unique(df$SPECIES_CODE),
                                   year = unique(df$YEAR),
                                   calc_weighted_mean(x = df[, imetric],
                                                      w = df$CPUE_KGKM2)))
                  
                }
              )
            )
          )
    ) 
}

write.csv(cogs, here::here("output", paste0("rf_cogs_", survey, "_", yr, ".csv")), row.names = FALSE)
