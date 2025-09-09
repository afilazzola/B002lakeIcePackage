## Get lakes within study area
lakes <- st_read("data/lakes/HydroLAKES_polys_v10.shp")
lakes

studyArea <- st_read("data/studyArea/studyArea.shp") %>%
  st_transform(crs(lakes))
studyArea

lakesStudyArea <- lakes[st_intersects(lakes, studyArea, sparse = FALSE), ]

climateRasters <- list.files(
  "data/climate/",
  full.names = T,
  pattern = ".nc",
  recursive = T
)

getClimateData <- function(data, latColumn, lonColumn, idColumn) {
  tmp <- rast(grep("tmp.dat.nc", climateRasters, value = T))
  pre <- rast(grep("pre.dat.nc", climateRasters, value = T))

  coords <- data %>% select(!!idColumn, !!lonColumn, !!latColumn) %>% distinct()

  ext <- ext(
    min(coords[[lonColumn]]) - 1,
    max(coords[[lonColumn]]) + 1,
    min(coords[[latColumn]]) - 1,
    max(coords[[latColumn]]) + 1
  )
  tmp <- crop(tmp, ext)
  pre <- crop(pre, ext)

  ## extract climate values
  tmpValues <- extract(tmp, as.matrix(coords[, c(lonColumn, latColumn)]))
  tmpValuesCoords <- cbind(coords, tmpValues[, 1:1476])
  tmpValuesLong <- tmpValuesCoords %>%
    gather(4:ncol(tmpValuesCoords), key = "timestep", value = "tmp") %>%
    mutate(
      date = rep(
        seq.Date(as.Date("1901-01-01"), as.Date("2023-12-31"), by = "month"),
        nrow(tmpValuesCoords)
      )
    )
  preValues <- extract(pre, as.matrix(coords[, c(lonColumn, latColumn)]))
  preValuesCoords <- cbind(coords, preValues[, 1:1476])
  preValuesLong <- preValuesCoords %>%
    gather(4:ncol(preValuesCoords), key = "timestep", value = "pre") %>%
    mutate(
      date = rep(
        seq.Date(as.Date("1901-01-01"), as.Date("2023-12-31"), by = "month"),
        nrow(preValuesCoords)
      )
    )
  climateValues <- cbind(tmpValuesLong, pre = preValuesLong$pre) %>%
    select(-timestep)

  climateValues[, "year"] <- as.numeric(format(climateValues$date, "%Y"))
  climateValues[, "month"] <- as.numeric(format(climateValues$date, "%m"))

  return(climateValues)
}


### Predict Random Forest Model
rlbhLakes <- lakesStudyArea %>%
  data.frame() %>%
  select(
    LAKEID = Hylak_id,
    latitude = Pour_lat,
    longitude = Pour_long,
    Depth_avg,
    Elevation,
    Vol_total,
    Lake_area,
    Wshd_area,
    Shore_dev,
    Shore_len
  )
iceOnModel <- readRDS("models/iceOnModel.rds")
iceOffModel <- readRDS("models/iceOffModel.rds")


rlbhLakesClimate <- getClimateData(rlbhLakes, "latitude", "longitude", "LAKEID")
rlbhLakesIceOn <- rlbhLakesClimate %>%
  mutate(winterYear = ifelse(month %in% 1:2, year - 1, year)) %>%
  filter(month %in% c(1, 2, 9:12)) %>%
  group_by(year, LAKEID) %>%
  summarise(tmpWinter = mean(tmp), preWinter = sum(pre)) %>%
  filter(year > 1990)
rlbhLakesIceOff <- rlbhLakesClimate %>%
  mutate(winterYear = ifelse(month %in% 1:5, year - 1, year)) %>%
  filter(month %in% c(12, 1:5)) %>%
  group_by(year, LAKEID) %>%
  summarise(tmpSpring = mean(tmp), preSpring = sum(pre))


rlbhLakesIceOnMorpho <- rlbhLakesIceOn %>%
  left_join(rlbhLakes) %>%
  filter(!is.na(tmpWinter))
rlbhLakesIceOffMorpho <- rlbhLakesIceOff %>%
  left_join(rlbhLakes) %>%
  filter(!is.na(tmpSpring))

rlbhLakesIceOnMorpho[, "ice_on_doy"] <- predict(
  iceOnModel,
  rlbhLakesIceOnMorpho,
  type = "response"
)
rlbhLakesIceOffMorpho[, "ice_off_doy"] <- predict(
  iceOffModel,
  rlbhLakesIceOffMorpho,
  type = "response"
)


ggplot(rlbhLakesIceOnMorpho, aes(x = ice_on_doy)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "Predicted Ice On Day of Year")


ggplot(rlbhLakesIceOffMorpho, aes(x = ice_off_doy)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "Predicted Ice Off Day of Year")
