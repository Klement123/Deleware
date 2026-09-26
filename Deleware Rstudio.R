library(sf)
library(SSN2)
library(SSNbler)
library(parallel)
watershed <- st_read("drbbnd/drb_bnd_polygon.shp")
flowlines <- st_read("Deleware QGIS/Deleware Clipped.gpkg")

st_crs(watershed)
st_crs(flowlines)

plot(st_geometry(watershed), col = "lightgray", main = "Delaware River Basin & Flowlines")
plot(st_geometry(flowlines), add = TRUE, col = "blue", lwd = 0.5)



#Pre-Processing...

num.cores<- detectCores() -1

lsn_path1 <- "LSN_Delaware"
flowlines <- st_cast(flowlines, "LINESTRING")
edges <- lines_to_lsn(
  streams=flowlines,
  lsn_path=lsn_path1,
  snap_tolerance=0.0001,
  check_topology = TRUE,
  topo_tolerance = 0.01,
  overwrite=TRUE,
  verbose=TRUE,
  remove_ZM=FALSE,
  use_parallel=TRUE,
  no_cores=num.cores)

clean_flowlines <- st_read("Deleware QGIS/cleaned_flowlines.gpkg")
clean_flowlines <- st_cast(clean_flowlines, "LINESTRING")
lsn_path2 <- "LSN_Deleware_CLEANED"

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
clean_flowlines <- st_read("Deleware QGIS/cleaned_flowlines_V2.gpkg")
clean_flowlines <- st_cast(clean_flowlines, "LINESTRING")
lsn_path2 <- "LSN_Deleware_CLEANED_V2"

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
