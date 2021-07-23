library(sf)
library(leaflet)
library(tidyverse)
library(vroom)
library(lubridate)

phu_map <- st_read(dsn = "Map",
                   layer = "Ministry_of_Health_Public_Health_Unit_Boundary")

ind_data <- vroom ("https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv",
                   col_types = cols(.default = "c")
) 

cases <- ind_data %>% 
  group_by (Reporting_PHU_ID) %>% 
  count (Outcome1) %>% 
  pivot_wider(names_from = Outcome1, 
              values_from = n, 
              values_fill = 0) %>% 
  mutate (Reporting_PHU_ID = parse_integer(Reporting_PHU_ID)
          )

cases_map <- phu_map %>% 
  left_join(cases, by = c ("PHU_ID" = "Reporting_PHU_ID"))

active_pal <- colorNumeric(
  palette = "YlOrRd",
  domain = cases_map$`Not Resolved`
  )

resolved_pal <- colorNumeric(
  palette = "YlOrRd",
  domain = cases_map$Resolved
)

map_phu <-  leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addPolygons(
    data = cases_map,
    weight = 1,
    smoothFactor = 0.9,
    color = "grey50",
    opacity = 0.5,
    fillOpacity = 0.25,
    fillColor = ~ active_pal(cases_map$`Not Resolved`),
    popup = ~ paste0(NAME_ENG,"<br>", "<b>", "Active Cases = ", `Not Resolved`, "</b><br>"),
    group = "Active Cases"
  ) %>% 
  addPolygons(
    data = cases_map,
    weight = 1,
    smoothFactor = 0.9,
    color = "grey50",
    opacity = 0.5,
    fillOpacity = 0.25,
    fillColor = ~ resolved_pal(cases_map$Resolved),
    popup = ~ paste0(NAME_ENG,"<br>","<b>", "Resolved Cases = ", `Resolved`, "</b><br>"),
    group = "Resolved Cases"
  ) %>% 
  addLayersControl(
    baseGroups = c("Active Cases",
                   "Resolved Cases"),
    options = layersControlOptions(collapsed = FALSE)
  )





# var <- "Neither"
# 
# if (var == "Resolved"){
#   map_resolved
# } else if (var == "Active"){
#   map_active
# } else {
#     print ("Error: Select either Active or Resolved")
#   }
