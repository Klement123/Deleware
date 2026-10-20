setwd("C:/Projects/Deleware")
setwd("C:/Projects/Deleware")
library(sf)
library(SSN2)
library(SSNbler)
library(parallel)
library(dplyr)
library(terra)
library(ggplot2)
# Step 1:
watershed <- st_read("C:/Projects/Deleware/drbbnd/drb_bnd_polygon.shp")
flowlines <- st_read("C:/Projects/Deleware/Deleware QGIS/Deleware Clipped.gpkg")

st_crs(watershed)
st_crs(flowlines)

plot(st_geometry(watershed), col = "lightgray", main = "Delaware River Basin & Flowlines")
plot(st_geometry(flowlines), add = TRUE, col = "blue", lwd = 0.5)



# Step 2: Pre-Processing, Building

num.cores <- max(1, detectCores() - 1)

# lsn_path1 <- "LSN_Delaware"
#flowlines <- st_cast(flowlines, "LINESTRING")
# edges <- lines_to_lsn(
# streams=flowlines,
#lsn_path=lsn_path1,
#snap_tolerance=0.0001,
# check_topology = TRUE,
#topo_tolerance = 0.01,
#overwrite=TRUE,
#verbose=TRUE,
#remove_ZM=FALSE,
#use_parallel=TRUE,
#no_cores=num.cores)

#clean_flowlines <- st_read("Deleware QGIS/cleaned_flowlines.gpkg")
#clean_flowlines <- st_cast(clean_flowlines, "LINESTRING")
#lsn_path2 <- "LSN_Deleware_CLEANED"

#edges_clean <- lines_to_lsn(
# streams = clean_flowlines,
# lsn_path = lsn_path2,
#snap_tolerance = 0.0001,
# check_topology = TRUE,
#topo_tolerance = 0.01,
# overwrite = TRUE,
# verbose = TRUE,
#remove_ZM = FALSE,
#use_parallel = TRUE,
#no_cores = num.cores

clean_flowlines <- st_read("C:/Projects/Deleware/Deleware QGIS/cleaned_flowlines_V2.gpkg")
clean_flowlines <- st_cast(clean_flowlines, "LINESTRING")
lsn_path2 <- "LSN_Deleware_CLEANED_V2.ssn"

edges_clean <- lines_to_lsn(
  streams = clean_flowlines,
  lsn_path = lsn_path2,
  snap_tolerance = 0.0001,
  check_topology = TRUE,
  topo_tolerance = 0.01,
  overwrite = TRUE,
  verbose = TRUE,
  remove_ZM = FALSE,
  use_parallel = TRUE,
  no_cores = num.cores
)


# Step 3: Import Data

#ny_dow <- read.csv(ny_path)
#ecosheds <- read.csv("EcoSHEDS_file.csv")

#print("NY DOW Columns:")
#colnames(ny_dow)
#print("EcoSHEDS Columns:")
#colnames(ecosheds)

# Step 4: Merge
#ny_clean <- ny_dow %>%
#  dplyr::select(Site_ID = SITE_CODE,
#                Latitude = LATITUDE,
#                Longitude = LONGITUDE) %>%
#  dplyr::mutate(Source = "NY_DOW") %>%
#  dplyr::distinct(Site_ID, .keep_all = TRUE) 

#eco_clean <- ecosheds %>%
#  dplyr::select(Site_ID = location_id,
#               Latitude = latitude,
#                Longitude = longitude) %>%
#  dplyr::mutate(Site_ID = as.character(Site_ID), 
#                Source = "EcoSHEDS") %>%
#  dplyr::distinct(Site_ID, .keep_all = TRUE)

#merged_sites <- dplyr::bind_rows(ny_clean, eco_clean) %>%
#  dplyr::mutate(Lat_round = round(Latitude, 4),
#                Lon_round = round(Longitude, 4)) %>%
#  dplyr::distinct(Lat_round, Lon_round, .keep_all = TRUE) %>%
# dplyr::select(-Lat_round, -Lon_round) 

#print(paste("Total unique monitoring sites:", nrow(merged_sites)))

# Step 5: Convert

#sites_sf <- st_as_sf(merged_sites, 
#                     coords = c("Longitude", "Latitude"), 
#                     crs = 4326) 

#sites_sf <- st_transform(sites_sf, st_crs(clean_flowlines))

#plot(st_geometry(clean_flowlines), col = "blue", lwd = 0.5, main = "Delaware Basin & Monitoring Sites")
#plot(st_geometry(sites_sf), add = TRUE, pch = 20, col = "red")

# Step 6: Snap points
#sites_snapped <- SSNbler::sites_to_lsn(
#  sites = sites_sf,
#  edges = edges_clean,
#  lsn_path = lsn_path2,
#  file_name = "obs_sites",
#  snap_tolerance = 500,
# overwrite = TRUE
#)


# Step 7: Calculate upstream distances for stream network

edges_updist <- SSNbler::updist_edges(
  edges = edges_clean,
  lsn_path = lsn_path2,
  calc_length = TRUE
)


# Step 8: Calculate spatial weights

# Check drainage area values
stopifnot(
  is.numeric(edges_updist$TotDASqKM),
  all(is.finite(edges_updist$TotDASqKM)),
  all(edges_updist$TotDASqKM >= 0)
)

# Calculate additive function values for stream edges
edges_afv <- SSNbler::afv_edges(
  edges = edges_updist,
  lsn_path = lsn_path2,
  infl_col = "TotDASqKM",
  segpi_col = "areaPI",
  afv_col = "afvArea",
  overwrite = TRUE
)

# Verify calculated spatial weights
summary(edges_afv$afvArea)

stopifnot(
  all(is.finite(edges_afv$afvArea))
)

# Step 9: Assemble and import SSN
#ssn_path <- file.path(getwd(), "Delaware_Model.ssn")

#my_ssn <- SSNbler::ssn_assemble(
#  edges = edges_afv,
#  lsn_path = lsn_path2,
#  obs_sites = sites_afv[["obs_sites"]],
#  ssn_path = ssn_path,
#  afv_col = "afvArea",
#  import = TRUE,
#  overwrite = TRUE
#)


# Step 10: Create distance matrices
#SSN2::ssn_create_distmat(
#  ssn.object = my_ssn,
#  overwrite = TRUE
#)

#summary(my_ssn)
# Step 11: Attach summer temperature summaries
#site_match <- match(
  #as.character(my_ssn$obs$Site_ID),
 # as.character(site_temp$SITE_CODE)
#)

#site_match[my_ssn$obs$Source != "NY_DOW"] <- NA_integer_

#my_ssn$obs$Summer_Temp_C <- site_temp$Summer_Temp_C[site_match]
#my_ssn$obs$Sampling_Days <- site_temp$Sampling_Days[site_match]

#sum(!is.na(my_ssn$obs$Summer_Temp_C))

# Step 12: Combine EcoSHEDS batches
eco_folder <- "C:/Projects/Deleware/EcoShedsTemperatureData"

zip_files <- file.path(
  eco_folder,
  paste0("Batch", 1:8, ".zip")
)

stopifnot(all(file.exists(zip_files)))

read_zip_csv <- function(zip_path, csv_name) {
  connection <- unz(zip_path, csv_name)
  on.exit(try(close(connection), silent = TRUE))
  
  read.csv(
    connection,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character"
  )
}

eco_values <- dplyr::bind_rows(
  lapply(zip_files, read_zip_csv, csv_name = "values.csv")
)

eco_stations <- dplyr::bind_rows(
  lapply(zip_files, read_zip_csv, csv_name = "stations.csv")
)

eco_values <- dplyr::distinct(eco_values)
eco_stations <- dplyr::distinct(eco_stations)

print(c(
  temperature_readings = nrow(eco_values),
  unique_stations = length(unique(eco_stations$location_id))
))

colnames(eco_values)
head(eco_values, 3)

# Step 13: Filter EcoSHEDS summer readings
eco_values$Date <- as.Date(
  eco_values$datetime,
  format = "%m/%d/%Y"
)

eco_values$Temperature_C <- suppressWarnings(
  as.numeric(eco_values[["temperature (degC)"]])
)

print(c(
  unparsed_dates = sum(is.na(eco_values$Date)),
  invalid_temperatures = sum(!is.finite(eco_values$Temperature_C))
))

eco_summer <- eco_values[
  !is.na(eco_values$Date) &
    eco_values$Date >= as.Date("2020-01-01") &
    eco_values$Date <= as.Date("2026-12-31") &
    format(eco_values$Date, "%m") %in% c("06", "07", "08") &
    is.finite(eco_values$Temperature_C),
]

print(c(
  summer_readings = nrow(eco_summer),
  summer_stations = length(unique(eco_summer$location_id))
))

table(eco_summer$flagged, useNA = "ifany")
summary(eco_summer$Temperature_C)

# Step 14: Check unflagged summer data
eco_summer_clean <- eco_summer[
  tolower(trimws(eco_summer$flagged)) == "false",
]

summary(eco_summer_clean$Temperature_C)

print(head(
  eco_summer_clean[
    order(eco_summer_clean$Temperature_C, decreasing = TRUE),
    c("location_id", "datetime", "Temperature_C", "flags")
  ],
  10
))

eco_daily <- aggregate(
  Temperature_C ~ location_id + Date,
  data = eco_summer_clean,
  FUN = mean
)

print(c(
  daily_means = nrow(eco_daily),
  stations = length(unique(eco_daily$location_id))
))

summary(as.numeric(table(eco_daily$location_id)))
# Step 15: Exclude stations needing temperature verification
suspect_station_ids <- c("15250", "15445", "15446", "15447")

eco_summer_verified <- eco_summer_clean[
  !eco_summer_clean$location_id %in% suspect_station_ids,
]

eco_daily <- aggregate(
  Temperature_C ~ location_id + Date,
  data = eco_summer_verified,
  FUN = mean
)

summary(eco_summer_verified$Temperature_C)
length(unique(eco_daily$location_id))
# Step 16: Prepare downloaded EcoSHEDS locations
eco_locations <- eco_stations[
  eco_stations$location_id %in% unique(eco_daily$location_id),
  c("location_id", "latitude", "longitude")
]

eco_locations <- unique(eco_locations)

eco_locations$latitude <- as.numeric(eco_locations$latitude)
eco_locations$longitude <- as.numeric(eco_locations$longitude)

print(c(
  station_rows = nrow(eco_locations),
  unique_stations = length(unique(eco_locations$location_id)),
  invalid_coordinates = sum(
    !is.finite(eco_locations$latitude) |
      !is.finite(eco_locations$longitude) |
      abs(eco_locations$latitude) > 90 |
      abs(eco_locations$longitude) > 180
  )
))
# Step 17: Snap new EcoSHEDS stations
eco_points <- sf::st_as_sf(
  eco_locations,
  coords = c("longitude", "latitude"),
  crs = 4326
)

eco_points <- sf::st_transform(
  eco_points,
  sf::st_crs(edges_afv)
)

eco_points$Site_ID <- as.character(eco_points$location_id)
eco_points$Source <- "EcoSHEDS"

eco_snapped <- SSNbler::sites_to_lsn(
  sites = eco_points,
  edges = edges_afv,
  lsn_path = lsn_path2,
  file_name = "eco_new_sites",
  snap_tolerance = 500,
  overwrite = TRUE
)

eco_updist <- SSNbler::updist_sites(
  sites = list(eco_new_sites = eco_snapped),
  edges = edges_afv,
  length_col = "Length",
  lsn_path = lsn_path2
)

eco_afv <- SSNbler::afv_sites(
  sites = eco_updist,
  edges = edges_afv,
  afv_col = "afvArea",
  lsn_path = lsn_path2,
  overwrite = TRUE
)

print(c(
  before_snapping = nrow(eco_points),
  after_snapping = nrow(eco_snapped)
))

summary(eco_afv[["eco_new_sites"]]$afvArea)

print(setdiff(
  eco_points$Site_ID,
  eco_snapped$Site_ID
))
# Step 18: Summarize EcoSHEDS summer temperatures
eco_daily$Year <- as.integer(format(eco_daily$Date, "%Y"))

eco_yearly <- aggregate(
  Temperature_C ~ location_id + Year,
  data = eco_daily,
  FUN = mean
)

eco_summary <- aggregate(
  Temperature_C ~ location_id,
  data = eco_yearly,
  FUN = mean
)

names(eco_summary)[2] <- "Summer_Temp_C"

eco_sites_ready <- eco_afv[["eco_new_sites"]]

eco_match <- match(
  eco_sites_ready$Site_ID,
  eco_summary$location_id
)

eco_sites_ready$Summer_Temp_C <-
  eco_summary$Summer_Temp_C[eco_match]

day_counts <- table(eco_daily$location_id)
year_counts <- table(eco_yearly$location_id)

eco_sites_ready$Sampling_Days <- as.integer(
  day_counts[eco_sites_ready$Site_ID]
)

eco_sites_ready$Sampling_Years <- as.integer(
  year_counts[eco_sites_ready$Site_ID]
)

summary(eco_sites_ready$Summer_Temp_C)
table(eco_sites_ready$Sampling_Years)

stopifnot(
  all(is.finite(eco_sites_ready$Summer_Temp_C))
)
# Step 19: Check summer coverage
eco_year_days <- aggregate(
  Date ~ location_id + Year,
  data = eco_daily,
  FUN = length
)

names(eco_year_days)[3] <- "Sampling_Days"

eco_year_days <- eco_year_days[
  eco_year_days$location_id %in% eco_sites_ready$Site_ID,
]

summary(eco_year_days$Sampling_Days)

print(c(
  sites_with_30_day_year = length(unique(
    eco_year_days$location_id[
      eco_year_days$Sampling_Days >= 30
    ]
  )),
  sites_with_60_day_year = length(unique(
    eco_year_days$location_id[
      eco_year_days$Sampling_Days >= 60
    ]
  )),
  sites_with_80_day_year = length(unique(
    eco_year_days$location_id[
      eco_year_days$Sampling_Days >= 80
    ]
  ))
))
# Step 20: Select well-covered summers
eligible_years <- eco_year_days[
  eco_year_days$Sampling_Days >= 80,
]

eco_yearly_eligible <- merge(
  eco_yearly,
  eligible_years,
  by = c("location_id", "Year")
)

eco_model_summary <- aggregate(
  Temperature_C ~ location_id,
  data = eco_yearly_eligible,
  FUN = mean
)

names(eco_model_summary)[2] <- "Summer_Temp_C"

eco_model_sites <- eco_sites_ready[
  eco_sites_ready$Site_ID %in% eco_model_summary$location_id,
]

model_match <- match(
  eco_model_sites$Site_ID,
  eco_model_summary$location_id
)

eco_model_sites$Summer_Temp_C <-
  eco_model_summary$Summer_Temp_C[model_match]

eligible_counts <- table(eco_yearly_eligible$location_id)

eco_model_sites$Sampling_Years <- as.integer(
  eligible_counts[eco_model_sites$Site_ID]
)

eligible_days <- aggregate(
  Sampling_Days ~ location_id,
  data = eco_yearly_eligible,
  FUN = sum
)

eco_model_sites$Sampling_Days <- eligible_days$Sampling_Days[
  match(eco_model_sites$Site_ID, eligible_days$location_id)
]

nrow(eco_model_sites)
summary(eco_model_sites$Summer_Temp_C)
table(eco_yearly_eligible$Year)
# Step 20.5: Add elevation to my observed EcoSHEDS sites
# Elevation helps explain stream temperature. It is not the value we predict.
elevation_path <- "C:/Projects/Deleware/ElevationData/Delaware_Elevation_Clipped.tif"
stopifnot(file.exists(elevation_path))

elevation_raster <- terra::rast(elevation_path)
stopifnot(terra::hasValues(elevation_raster))
print(elevation_raster)

# Use this same function later for the prediction points.
get_elevation <- function(points, raster) {
  points_dem <- sf::st_transform(
    points,
    terra::crs(raster)
  )
  
  values <- terra::extract(
    raster,
    terra::vect(points_dem)
  )
  
  stopifnot(nrow(values) == nrow(points))
  points$Elevation <- as.numeric(values[[2]])
  return(points)
}

eco_model_sites <- get_elevation(eco_model_sites, elevation_raster)

print(summary(eco_model_sites$Elevation))
print(c(
  missing_observed_elevation = sum(is.na(eco_model_sites$Elevation)),
  zero_observed_elevation = sum(eco_model_sites$Elevation == 0, na.rm = TRUE)
))

# All observed sites need an elevation to fit the new model.
stopifnot(all(is.finite(eco_model_sites$Elevation)))
          
# Step 21: Assemble EcoSHEDS modeling network
eco_model_path <- file.path(
  getwd(),
  "Delaware_EcoSHEDS_Model.ssn"
)

eco_ssn <- SSNbler::ssn_assemble(
  edges = edges_afv,
  lsn_path = lsn_path2,
  obs_sites = eco_model_sites,
  ssn_path = eco_model_path,
  afv_col = "afvArea",
  import = TRUE,
  overwrite = TRUE
)

# Step 22: Create modeling distance matrices
SSN2::ssn_create_distmat(
  ssn.object = eco_ssn,
  overwrite = TRUE
)

summary(eco_ssn)
# Step 23: Fit initial stream temperature model
eco_fit <- SSN2::ssn_lm(
  formula = Summer_Temp_C ~ 1,
  ssn.object = eco_ssn,
  tailup_type = "exponential",
  taildown_type = "none",
  euclid_type = "none",
  additive = "afvArea"
)

summary(eco_fit)

# Step 23.5: Fit water temperature model using elevation
eco_elevation_fit <- SSN2::ssn_lm(
  formula = Summer_Temp_C ~ Elevation,
  ssn.object = eco_ssn,
  tailup_type = "exponential",
  taildown_type = "none",
  euclid_type = "none",
  additive = "afvArea"
)

summary(eco_elevation_fit)

# Step 24: Check prediction accuracy
eco_cv <- SSN2::loocv(eco_fit)
print(eco_cv)

# Step 24.5: Compare prediction errors with and without elevation
eco_elevation_cv <- SSN2::loocv(
  eco_elevation_fit
)

print(eco_cv)
print(eco_elevation_cv)

# Step 25: Compare against a nonspatial baseline
eco_baseline <- SSN2::ssn_lm(
  formula = Summer_Temp_C ~ 1,
  ssn.object = eco_ssn,
  tailup_type = "none",
  taildown_type = "none",
  euclid_type = "none"
)

baseline_cv <- SSN2::loocv(eco_baseline)

print(baseline_cv, width = Inf)
print(eco_cv, width = Inf)
# Step 26: Inspect model diagnostics
# plot(eco_fit)
# Step 27: Inspect coldest sites
site_check <- sf::st_drop_geometry(eco_ssn$obs)

site_check$Station_Name <- eco_stations$location_name[
  match(site_check$Site_ID, eco_stations$location_id)
]

print(head(
  site_check[
    order(site_check$Summer_Temp_C),
    c("Site_ID", "Station_Name", "Summer_Temp_C",
      "Sampling_Days", "Sampling_Years", "snapdist")
  ],
  10
))
# Step 28: Inspect the coldest station
cold_site <- eco_daily[eco_daily$location_id == "9830", ]
if (nrow(cold_site) > 0) {
  cold_site$Month <- format(cold_site$Date, "%m")
  
  print(aggregate(
    Temperature_C ~ Year + Month,
    data = cold_site,
    FUN = mean
  ))
  
  plot(
    cold_site$Date,
    cold_site$Temperature_C,
    pch = 20,
    cex = 0.5,
    xlab = "Date",
    ylab = "Daily mean temperature (C)",
    main = "Station 9830: Summer daily temperatures"
  )
}
# Step 29: Map observed summer temperatures
plot(
  eco_ssn$obs["Summer_Temp_C"],
  pch = 20,
  cex = 1.2,
  main = "Observed summer temperature averages, 2020–2026"
)
# Step 30: Join measurements to station coordinates
station_coords <- unique(
  eco_stations[c("location_id", "latitude", "longitude")]
)

stopifnot(!anyDuplicated(station_coords$location_id))

station_coords$latitude <- as.numeric(station_coords$latitude)
station_coords$longitude <- as.numeric(station_coords$longitude)

eco_measurements_coords <- dplyr::left_join(
  eco_values,
  station_coords,
  by = "location_id"
)

stopifnot(
  nrow(eco_measurements_coords) == nrow(eco_values),
  all(is.finite(eco_measurements_coords$latitude)),
  all(is.finite(eco_measurements_coords$longitude))
)

write.csv(
  eco_measurements_coords,
  "C:/Projects/Deleware/EcoSHEDS_Measurements_Coordinates.csv",
  row.names = FALSE
)

# Step 31: Load pred points

pred_points <- sf::st_read(
  "C:/Projects/Deleware/mid-atlantic-preds/delaware-pred-points.shp"
)

pred_points <- sf::st_transform(
  pred_points,
  sf::st_crs(edges_afv)
)
# Step 32: Snap pred points

eco_preds <- SSNbler::sites_to_lsn(
  sites = pred_points,
  edges = edges_afv,
  lsn_path = lsn_path2,
  file_name = "eco_preds",
  snap_tolerance = 100,
  overwrite = TRUE
)

eco_preds_updist <- SSNbler::updist_sites(
  sites = list(preds = eco_preds),
  edges = edges_afv,
  length_col = "Length",
  lsn_path = lsn_path2
)

eco_preds_afv <- SSNbler::afv_sites(
  sites = eco_preds_updist,
  edges = edges_afv,
  afv_col = "afvArea",
  lsn_path = lsn_path2,
  overwrite = TRUE
)
# Step 32.5: Give each prediction point its own elevation
# These elevation values help the model predict temperatures where there are no sensors.
eco_preds_afv[["preds"]] <- get_elevation(
  eco_preds_afv[["preds"]],
  elevation_raster
)

print(summary(eco_preds_afv[["preds"]]$Elevation))
print(c(
  prediction_points = nrow(eco_preds_afv[["preds"]]),
  missing_prediction_elevation = sum(is.na(eco_preds_afv[["preds"]]$Elevation)),
  zero_prediction_elevation = sum(eco_preds_afv[["preds"]]$Elevation == 0, na.rm = TRUE),
  zero_prediction_afv = sum(eco_preds_afv[["preds"]]$afvArea == 0, na.rm = TRUE)
))

# If there are missing values, fix the DEM coverage before predicting.
stopifnot(all(is.finite(eco_preds_afv[["preds"]]$Elevation)))

# Step 33: Assemble Spatial Stream Net w/ pred points
eco_pred_ssn <- SSNbler::ssn_assemble(
  edges = edges_afv,
  lsn_path = lsn_path2,
  obs_sites = eco_model_sites,
  preds_list = eco_preds_afv["preds"],
  ssn_path = file.path(
    getwd(),
    "Delaware_EcoSHEDS_Predictions.ssn"
  ),
  afv_col = "afvArea",
  import = TRUE,
  overwrite = TRUE
)
SSN2::ssn_create_distmat(
  ssn.object = eco_pred_ssn,
  predpts = "preds",
  among_predpts = FALSE,
  overwrite = TRUE
)

# Fit the final water temperature model using elevation and stream-network relationships
eco_pred_fit <- SSN2::ssn_lm(
  formula = Summer_Temp_C ~ Elevation,
  ssn.object = eco_pred_ssn,
  tailup_type = "exponential",
  taildown_type = "none",
  euclid_type = "none",
  additive = "afvArea"
)

# Step 34: Temp Predictions
eco_predictions<-SSN2::augment(
  eco_pred_fit,
  newdata="preds",
  type.predict="response"
)
library(ggplot2)

temperature_plot<-ggplot()+
  geom_sf(data=edges_afv,color="gray70")+
  geom_sf(
    data = eco_predictions,
    aes(color = .fitted),
    size = 1
  ) +
  scale_color_viridis_c(option = "H") +
  labs(
    title = "Delaware River Watershed: Predicted Summer Stream Temperatures",
    subtitle = "Predictions using EcoSHEDS, elevation, and stream connectivity",
    color = "Temperature (°C)"
  ) +
  theme_bw()

print(temperature_plot)
print(summary(eco_predictions$.fitted))