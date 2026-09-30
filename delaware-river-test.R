# Libraries ----
library(sf)
library(SSN2)
library(SSNbler)
library(parallel)

# Session settings, file paths, data reads ----
# Set number of cores for parallel routines
num.cores<- detectCores() -1

# Import flowlines and recast feature type from 
# multistring to linestring format
clean_flowlines <- st_read("LSN_Deleware_CLEANED_V2/edges.gpkg")
clean_flowlines <- st_cast(clean_flowlines, "LINESTRING")

# Set path for processing files
lsn_path2 <- "LSN_Delaware"

# Create landscape network from flowlines ----
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

# Calculate upstream distances for lsn ----
# Here is how to calculate upstream distances to convert from
# landscape network to directional graph
edges <- updist_edges(
  edges = edges_clean,
  save_local = TRUE,
  lsn_path = lsn_path2,
  calc_length = TRUE
)

# Graphical check
ggplot() +
  geom_sf(data = edges, aes(color = upDist)) +
  coord_sf(datum = st_crs(MF_streams)) +
  scale_color_viridis_c() +
  labs(color = "Distance from ocean (km)")

# Site data ----
# . Observed site data (e.g. temperature) ----
obs_points <- st_read("observation_data/temperatures.shp")

# . Prediction points ----
# Clipped mid atlantic preds to delaware basin in QGIS bc it's faster
pred_points <- st_read("mid-atlantic-preds/delaware-pred-points.shp")

# Prepare LSN for analysis ----
# . Add observation sites to lsn ----
obs <- sites_to_lsn(
  sites = obs_points,
  edges = edges,
  snap_tolerance = 100,
  save_local = TRUE,
  lsn_path = lsn_path2,
  file_name = "sites.gpkg",
  overwrite = TRUE
)


# . Add prediction points to lsn ----
temperature_preds <- sites_to_lsn(
  sites = pred_points,
  edges = edges,
  snap_tolerance = 100, 
  save_local = TRUE,
  lsn_path = lsn_path2,
  file_name = "preds.gpkg",
  overwrite = TRUE
)


# . Generate the sites list ---- 
# This is a named list
sitelist = list(obs = obs, 
                preds = temperature_preds)


# . Compute updist on sites and preds ----
site.list <- updist_sites(
  sites = sitelist,
  edges = edges,
  length_col = "Length",
  lsn_path = lsn_path2,
  save_local = TRUE,
  overwrite = TRUE
)


# Generate additive function values ----
# . Edges ----
edges <- afv_edges(
  edges = edges,
  lsn_path = lsn_path2,
  infl_col = "TotDASqKM",
  # infl_col = "CUMDRAINAG",
  segpi_col = "areaPI",
  afv_col = "afvArea",
  save_local = TRUE,
  overwrite = TRUE
)


# . Sites ----
# using the site list created above
site.list <- afv_sites(
  sites = site.list,
  edges = edges,
  afv_col = "afvArea",
  save_local = TRUE,
  lsn_path = lsn_path2,
  overwrite = TRUE
)


# Generate the SSN object ----
# Assemble ssn
ssn_path <- "SSN"
delaware_ssn <- ssn_assemble(
  edges = edges,
  lsn_path = lsn_path2,
  obs_sites = site.list$obs, # This one is a df
  preds_list = site.list["preds"], # This one can be a list
  ssn_path = ssn_path,
  import = TRUE,
  check = TRUE,
  afv_col = "afvArea",
  overwrite = TRUE
)

# Generate hydrologic distance matrices ----
# Calculate distances between features
# ssn_create_bigdist(ssn.object = delaware_ssn,
#                    predpts = c("preds"),
#                    among_predpts = TRUE,
#                    overwrite = TRUE)

ssn_create_distmat(
  ssn.object = delaware_ssn,
  predpts = c("preds"),
  among_predpts = FALSE,
  overwrite = TRUE
)

# Fit a test model ----
# This is just saying "model average temperature (~1) as the 
# intercept for a linear predictor with specified variance covariance
# processes." We can dig into this more as we go, but we're moving from
# code to math now.
# Note temperature gets cut off in QGIS because the name is too long for
# a shapefile. We could save the data as .gpkg to avoid this
ssn_mod <- ssn_lm(
  formula = temperatur ~ 1,
  ssn.object = delaware_ssn,
  tailup_type = "exponential",
  taildown_type = "spherical",
  euclid_type = "gaussian",
  additive = "afvArea",
  random = ~ site_code
)

# . Summary ----
summary(ssn_mod)


# . Plot the residuals ----
# plot(ssn_mod, which = 1)


# . Predictions ----
# .. Watershed prediction ----
ssn_mod$ssn.object$preds$preds$site_code <- as.factor(NaN)
aug_preds <- augment(ssn_mod, newdata = "preds",
                     type.predict = "response")


# .. Plot ----
plotter <- aug_preds %>% 
  arrange(.fitted)

temperature_plot <- 
  ggplot() +
  geom_sf(data = delaware_ssn$edges) +
  geom_sf(data = plotter, aes(color = .fitted), 
          size = 1) +
  scale_color_viridis_c(limits = c(10, 30), option = "H") +
  labs(color = "") +
  ylab("Latitude") +
  xlab("Longitude") +
  theme_bw()

temperature_plot

