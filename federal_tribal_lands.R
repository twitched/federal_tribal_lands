library(tidyverse)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)
epsg_aea = 5070 #EPSG number for CONUS Albers Equal Area Projection https://epsg.io/5070-1252

#State data from tigris
states_tigris <- states(cb = TRUE, resolution = '500k')
states_data <- states_tigris |> 
  filter(!(STATEFP %in% c(60, 66, 69, 78))) |> #remove the territories
  st_difference(st_sfc(st_polygon(list(matrix(c(-179, 14, -179, 35, -162, 35, -162, 14, -179, 14), ncol = 2, byrow = T))), crs = st_crs(states_tigris))) |> 
  shift_geometry() |> #move alaska, hawaii, and puerto rico
  st_transform(epsg_aea) #project

#Counties data from tigris
counties_tigris <- counties(cb = TRUE, resolution = '500k') 
counties_data <- counties_tigris |> 
  filter(!(STATEFP %in% c(60, 66, 69, 78))) |>#remove the territories
  #remove the outer hawaii islands
  st_difference(st_sfc(st_polygon(list(matrix(c(-179, 14, -179, 35, -162, 35, -162, 14, -179, 14), ncol = 2, byrow = T))), crs = st_crs(counties_tigris))) |> 
  shift_geometry() |> 
  st_transform(epsg_aea)

#Urban Areas from tigris
urban_area_data <- urban_areas(cb = TRUE) |> #get counties at 1:1,000,000 resolutiona
  separate(NAME10, sep = ', ', into = c("City", "State")) |>
  filter(!(State %in% c("GU", "MP", "AS", "VI"))) |> #remove the territories
  shift_geometry() |>  
  st_transform(epsg_aea) 

#https://www.sciencebase.gov/catalog/item/5d150464e4b0941bde5b7653
fed <- st_read("data/fedland/fedlanp010g.shp") |>
  filter(STATE != "VI") |> #remove the territories
  shift_geometry() |>
  st_transform(epsg_aea) #|> 

fed_all <- fed |>
  mutate(use = case_when(
    ADMIN1 == "DOD" ~ "Military",
    ADMIN1 == "FWS" ~ "Fish and Wildlife",
    ADMIN1 == "BLM" ~ "BLM",
    ADMIN1 == "NPS" ~ "National Park",
    ADMIN1 == "DOE" ~ "Department of Energy",
    ADMIN1 == "NASA" ~ "NASA",
    !(ADMIN1 %in% c("DOD", "FS", "FWS", "BLM", "NPS", "DOE", "NASA")) ~ "Other"
  )) |> filter(!is.na(use))

#Forest Service from https://data.fs.usda.gov/geodata/edw/datasets.php?dsetCategory=boundaries
fs <- st_read("data/forest/S_USA.FSCommonNames.shp") |>
  shift_geometry() |>
  st_transform(epsg_aea) |>
  mutate(use = case_when(
    ADMINTYPE == "National Forest" ~ "Forest Service",
    ADMINTYPE == "National Grassland Simplified" ~ "National Grassland (Border)",
    ADMINTYPE == "National Grassland" ~ "National Grassland",
  )) |> filter(!is.na(use))

# colors from https://www.ntc.blm.gov/krc/uploads/223/Ownership_Map_Color_Reference_Sheet.pdf
colors <- c("Indian Reservation" = rgb(253,180,108, maxColorValue = 255),
            "BLM" =  rgb(254,230,121, maxColorValue = 255),
            "BLM Wilderness" = rgb(254,204,92, maxColorValue = 255),
            "BLM National Monument (Border)" = rgb(250,195,80, maxColorValue = 255),
            "Military" =  rgb(251,180,206, maxColorValue = 255),
            "Fish and Wildlife" = rgb(127,204,16, maxColorValue = 255),
            "Fish and Wildlife Wilderness" = rgb(102,191,127, maxColorValue = 255),
            "Forest Service" = rgb(204,235,197, maxColorValue = 255),
            "Forest Service Wilderness" = rgb(153,213,148, maxColorValue = 255),
            "National Park" =  rgb(202,189,220, maxColorValue = 255),
            "National Park Wilderness" = rgb(177,137,193, maxColorValue = 255),
            "National Grassland (Border)" = rgb(230,245,177, maxColorValue = 255),
            "National Grassland" = rgb(220,225,157, maxColorValue = 255),
            "NASA" = "red",
            "Department of Energy" = "#FB9A99",
            "Lake" = "#33CCFF",
            "Dry Lake" = "#33CCFF33",
            "Urban Area" = "grey70",
            "State" = rgb(179,227,238, maxColorValue = 255),
            "Other" = rgb(228,196,159, maxColorValue = 255)
)
# from https://biamaps.doi.gov/bogs/datadownload.html
res_data <- st_read("data/reservations/BIA_National_LAR.shp") |>
  shift_geometry() |>
  st_transform(epsg_aea) #|> 

#Wilderness http://www.wilderness.net/GIS/Wilderness_Areas.zip
wild <- st_read("data/wilderness/Wilderness_Areas_071921.shp") |>
  filter(STATE != "VI") |> 
  shift_geometry() |>
  st_transform(epsg_aea) |>
  mutate(use = case_when(
    Agency == "FS" ~ "Forest Service Wilderness",
    Agency == "NPS" ~ "National Park Wilderness",
    Agency == "FWS" ~ "Fish and Wildlife Wilderness",
    Agency == "BLM" ~ "BLM Wilderness"
  )) 

# whole state database is too big for github (> 100 MB), so download, read GDB then write to SHP
# sogr2ogr data/state/state.shp data/blm/sma_wm.gdb "SurfaceMgtAgy_STATE" 
state_data <- st_read("data/state/state.shp") |>
  shift_geometry() |>
  st_transform(epsg_aea)


# hydro https://www.sciencebase.gov/catalog/item/581d0552e4b08da350d5274e
# whole hydro database is too big for github (> 100 MB), so download, read GDB then write to SHP
# lake_data <- st_read("data/hydro/hydrusm010g.gdb", layer = "Waterbody")
# st_write(lake_data_raw, "data/hyrdo/lakes.shp")
lake_data <- st_read("data/hydro/lakes.shp") |> 
  shift_geometry() |>
  st_transform(epsg_aea)

lakes <- lake_data |> filter(Feature %in% c("Lake", "Reservoir", "Stream"))
dry_lakes <- lake_data |> filter(Feature %in% c("Lake Intermittent", "Lake Dry"))

# https://arcg.is/140rvP
great_lake_raw <- st_read("data/main_lakes/main_lakes.shp") 
# great lakes data from this source has a spherical geometry problem.  
# Fixed it using this code from https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data/
great_lake_raw$geometry <- great_lake_raw$geometry %>% st_make_valid() %>%
  s2::s2_rebuild() %>%
  sf::st_as_sfc()
great_lake_data <- great_lake_raw |> 
  #remove the part of the St Lawrence seaway that is in Canada
  st_difference(
    st_polygon(list(matrix(c(-75, 45, -75, 48, -70, 48, -70, 45, -75, 45), ncol = 2, byrow = T))) |> 
      st_sfc(crs = st_crs(great_lake_raw))
  ) |>
  st_transform(epsg_aea)

#taken from github.com/kjhealy/us-fed-lands
map_theme <- theme(axis.line=element_blank(),
                   axis.text=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title=element_blank(),
                   panel.grid.major = element_line(colour = "transparent"),
                   panel.background=element_blank(),
                   panel.border=element_blank(),
                   panel.grid=element_blank(),
                   panel.spacing=unit(0, "lines"),
                   plot.background = element_rect(fill = "lightcyan"),
                   legend.justification = c(0,0),
                   legend.position = c(0,0)
)

us <- ggplot() +
  geom_sf(data = states_data, fill = "White", linetype = 0) +
  geom_sf(data = fed_all |> filter(use == "BLM"), linetype = 0, fill = colors["BLM"]) + #BLM needs to be first so reservations can go on top
  geom_sf(data = fed_all |> filter(use == "BLM", FEATURE1 == "National Monument"), size = .1, fill = colors["BLM"], colour = colors["BLM National Monument (Border)"]) +
  geom_sf(data = res_data, linetype = 0, fill = colors["Indian Reservation"]) +
  geom_sf(data = urban_area_data, fill = colors["Urban Area"], linetype = 0) +
  geom_sf(data = fs |> filter(use == "National Grassland (Border)"), linetype = 0, fill = colors["National Grassland (Border)"], ) +
  geom_sf(mapping = aes(fill = use), data = fs |> filter(use != "National Grassland (Border)"), linetype = 0) +
  geom_sf(mapping = aes(fill = use), data = fed_all |> filter(use != "BLM"), linetype = 0) + 
  geom_sf(mapping = aes(fill = use), data = wild, linetype = 0) + 
  geom_sf(data = state_data, linetype = 0, fill = colors["State"]) +
  geom_sf(data = lakes, linetype = 0, fill = colors["Lake"]) +
  geom_sf(data = dry_lakes, size = .05, color = colors["Lake"], fill = colors["Dry Lake"]) + 
  geom_sf(data = great_lake_data, linetype = 0, fill = colors["Lake"]) + 
  geom_sf(data = counties_data, fill = NA, size = .05, colour = "grey81") +
  geom_sf(data = states_data, fill = NA, size = .1, colour = "grey61") +
  scale_fill_manual(values = colors) +
  map_theme


ggsave("figures/us_use_map.pdf", us, width = 24, height = 18, units = "in")
ggsave("figures/us_use_map.png", us, width = 17, height = 11, units = "in")
