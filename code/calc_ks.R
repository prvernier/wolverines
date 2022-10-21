library(sf)
library(dplyr)
library(readr)

clusters=3
size=50

factors <- st_read("data/wolverines.gpkg", "survey_factors", quiet=T)
simple <- read_csv(paste0('samples/simple_random_c',clusters,'_n',size,'.csv')) %>%
    left_join(factors)
stratified <- read_csv(paste0('samples/stratified_random_c',clusters,'_n',size,'.csv')) %>%
    left_join(factors)

ks_simple <- tibble(type="simple",
    merge100_pct = ks.test(factors$merge100_pct, simple$merge100_pct)[[1]],
    merge500_pct = ks.test(factors$merge500_pct, simple$merge500_pct)[[1]],
    placer_pct = ks.test(factors$placer_pct, simple$placer_pct)[[1]],
    quartz_pct = ks.test(factors$quartz_pct, simple$quartz_pct)[[1]],
    recent_fires_pct = ks.test(factors$recent_fires_pct, simple$recent_fires_pct)[[1]],
    benchmark_pct = ks.test(factors$benchmark_pct, simple$benchmark_pct)[[1]],
    elev_median = ks.test(factors$elev_median, simple$elev_median)[[1]],
    elev_sd = ks.test(factors$elev_sd, simple$elev_sd)[[1]],
    forest_pct = ks.test(factors$forest_pct, simple$forest_pct)[[1]],
    wetland_pct = ks.test(factors$wetland_pct, simple$wetland_pct)[[1]],
    water_pct = ks.test(factors$water_pct, simple$water_pct)[[1]])

ks_stratified <- tibble(type="stratified",
    merge100_pct = ks.test(factors$merge100_pct, stratified$merge100_pct)[[1]],
    merge500_pct = ks.test(factors$merge500_pct, stratified$merge500_pct)[[1]],
    placer_pct = ks.test(factors$placer_pct, stratified$placer_pct)[[1]],
    quartz_pct = ks.test(factors$quartz_pct, stratified$quartz_pct)[[1]],
    recent_fires_pct = ks.test(factors$recent_fires_pct, stratified$recent_fires_pct)[[1]],
    benchmark_pct = ks.test(factors$benchmark_pct, stratified$benchmark_pct)[[1]],
    elev_median = ks.test(factors$elev_median, stratified$elev_median)[[1]],
    elev_sd = ks.test(factors$elev_sd, stratified$elev_sd)[[1]],
    forest_pct = ks.test(factors$forest_pct, stratified$forest_pct)[[1]],
    wetland_pct = ks.test(factors$wetland_pct, stratified$wetland_pct)[[1]],
    water_pct = ks.test(factors$water_pct, stratified$water_pct)[[1]])

ks <- rbind(ks_simple, ks_stratified)

readr::write_csv(ks, paste0('samples/ks_c',clusters,'_n',size,'.csv'))
