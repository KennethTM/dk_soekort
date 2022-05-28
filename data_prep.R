library(tidyverse);library(sf);library(readxl);library(lubridate)

#Publicly available data from plant surveys in Danish lakes (https://odaforalle.au.dk/)
plant_raw <- read_excel("data/plant_data.xlsx")

plant_sub <- plant_raw |> 
  mutate(z = parse_number(`Vanddybden i m`, locale = locale(decimal_mark = ",")),
         year = year(ymd(StartDato))) |> 
  rename(x = `Punkt X-UTM`,
         y = `Punkt Y-UTM`,
         datum = `Transekt datum...31`, 
         zone = `Geografisk zone...30`) |> 
  filter(datum == "EUREF89" & zone == 32) |> 
  select(site = ObservationsStedNr, name = Lokalitetsnavn, year, x, y, z) |> 
  distinct() |> 
  na.omit()

plant_high_n <- plant_sub |> 
  group_by(site, year) |> 
  add_tally() |> 
  group_by(site) |> 
  summarise(year = year[which.max(n)]) |> 
  left_join(plant_sub)

xyz_raw <- st_as_sf(plant_high_n, coords = c("x", "y"), crs=25832)

xyz_centroids <- xyz_raw %>% 
  group_by(site, year) %>% 
  summarise() %>% 
  st_centroid()

#Publicly available data with Danish lake polygons (https://dataforsyningen.dk/)
lakes_raw <- st_read("data/DK_StandingWater.gml")

lakes <- lakes_raw |>
  st_zm() |> 
  select(gml_id) |> 
  st_transform(25832)

xyz_centroids_gml_id <- xyz_centroids |> 
  st_join(lakes) |> 
  st_drop_geometry()

xyz_gml_id <- xyz_raw |> 
  left_join(xyz_centroids_gml_id) |> 
  na.omit() |> 
  mutate(navn = str_to_title(gsub('[[:punct:]]+','', name)))

#Save data
xyz_gml_id |> 
  st_write("data/xyz.sqlite")

lakes |> 
  filter(gml_id %in% unique(xyz_gml_id$gml_id)) |> 
  st_write("data/lake_polys.sqlite")
