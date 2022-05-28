#library(raster);
library(terra);
library(tidyverse);library(sf)
library(fields)

#Load prepared data
xyz <- st_read("data/xyz.sqlite")
lakes <- st_read("data/lake_polys.sqlite")

#Lakes to map
names <- readLines("data/lakes_to_map.txt")


#draft func

l <- names[1]

l_xyz <- xyz |> 
  filter(gml_id == l)

l_poly <- lakes |> 
  filter(gml_id == l)

l_xyz_zero <- l_poly |> 
  st_cast("MULTILINESTRING") %>% 
  st_cast("LINESTRING") %>% 
  st_line_sample(density = 0.025) %>% 
  st_cast("POINT") %>% 
  st_as_sf() %>% 
  mutate(z = 0) %>% 
  rename(GEOMETRY = x)

l_xyz_all <- rbind(l_xyz_zero, st_intersection(select(l_xyz, z), l_poly$GEOMETRY))

l_xyz_all_vect <- vect(l_xyz_all)
raster_template <- rast(l_xyz_all_vect, resolution = 2, crs = crs(l_xyz_all_vect))

tps_mod <- Tps(st_coordinates(l_xyz_all), l_xyz_all$z)
depth_interp <- interpolate(raster_template, tps_mod)
depth_interp_mask <- mask(depth_interp, vect(l_poly))
depth_interp_mask[depth_interp_mask < 0] = 0

#plot(depth_interp_mask, col=viridis::mako(25, alpha = 0.5, direction = -1))

lake_dir <- paste0("kort/", l_xyz$navn[1], "/")

if(!dir.exists(lake_dir)){
  dir.create(lake_dir)
}

background_kmz <- paste0(lake_dir, "Baggrund", ".kmz")

raster_depth <- raster::raster(project(depth_interp_mask, "epsg:4326"))
names(raster_depth) <- paste0("Baggrund.", l_xyz$navn[1])

raster::KML(raster_depth, background_kmz, col=viridis::mako(25, alpha = 1, direction = -1), overwrite=TRUE)

l_poly_stats <- l_poly |> 
  mutate(area = as.numeric(st_area(GEOMETRY)),
         zmean = mean(depth_interp_mask[], na.rm =TRUE),
         zmax = max(depth_interp_mask[], na.rm =TRUE),
         navn = l_xyz$navn[1])

contour_levels <- pretty(depth_interp_mask[])
depth_interp_contour <- as.contour(depth_interp_mask, levels=contour_levels)

contour_kml <- paste0(lake_dir, "Linjer", ".kml")

st_as_sf(depth_interp_contour) |> 
  st_cast("MULTILINESTRING") |> 
  rename(dybde = level) |> 
  st_write(contour_kml)

contour_kmz <- gsub(".kml", ".kmz", contour_kml)

zip(contour_kmz, contour_kml)
file.remove(contour_kml)

lake_kml <- paste0(lake_dir, "Sø", ".kml")

l_poly_stats |> 
  st_write(lake_kml)

lake_kmz <- gsub(".kml", ".kmz", lake_kml)

zip(lake_kmz, lake_kml)
file.remove(lake_kml)
