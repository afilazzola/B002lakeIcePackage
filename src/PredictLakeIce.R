# predictLakeIce.R
# Simplified Lake Ice Prediction for SyncroSim - Generates 4 plots only

# Load required libraries
library(rsyncrosim)
library(sf)
library(terra)
library(tidyverse)
library(lubridate)
library(randomForest)
sf_use_s2(FALSE)

# Get the SyncroSim Scenario
myScenario <- scenario()

# Get transfer directory for outputs
e <- ssimEnvironment()
transferDir <- e$TransferDirectory

# Load user inputs
runControl <- datasheet(myScenario, "lakeIcePrediction_RunControl")
startYear <- runControl$MinTimestep
endYear <- runControl$MaxTimestep
studyAreaPath <- runControl$StudyAreaShapefile

# Hard-coded paths to data and models (adjust these paths as needed)
packageDir <- (Sys.getenv("ssim_package_directory"))
LAKES_DATA_PATH <- paste0(packageDir, "/../data/HydroLAKES_polys_v10.shp")
CLIMATE_DATA_PATH <- paste0(packageDir, "/../data/climate/")
ICE_ON_MODEL_PATH <- paste0(packageDir, "/../models/iceOnModel.rds")
ICE_OFF_MODEL_PATH <- paste0(packageDir, "/../models/iceOffModel.rds")

# Function to extract climate data
getClimateData <- function(data, climateRasters) {
  tmp <- rast(grep("tmp.dat.nc", climateRasters, value = TRUE))
  pre <- rast(grep("pre.dat.nc", climateRasters, value = TRUE))

  coords <- data %>% select("LAKEID", "longitude", "latitude") %>% distinct()

  ext <- ext(
    min(coords[["longitude"]]) - 1,
    max(coords[["longitude"]]) + 1,
    min(coords[["latitude"]]) - 1,
    max(coords[["latitude"]]) + 1
  )
  tmp <- crop(tmp, ext)
  pre <- crop(pre, ext)

  tmpValues <- terra::extract(
    tmp,
    as.matrix(coords[, c("longitude", "latitude")])
  )
  tmpValuesCoords <- cbind(coords, tmpValues[, 1:1476])
  tmpValuesLong <- tmpValuesCoords %>%
    gather(4:ncol(tmpValuesCoords), key = "timestep", value = "tmp") %>%
    mutate(
      date = rep(
        seq.Date(as.Date("1901-01-01"), as.Date("2023-12-31"), by = "month"),
        nrow(tmpValuesCoords)
      )
    )

  preValues <- terra::extract(
    pre,
    as.matrix(coords[, c("longitude", "latitude")])
  )
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

# Main processing
tryCatch(
  {
    # Load data
    studyArea <- st_read(studyAreaPath)
    lakes <- st_read(LAKES_DATA_PATH)
    studyArea <- st_transform(studyArea, crs(lakes))

    # Get lakes within study area
    lakesStudyArea <- lakes[st_intersects(lakes, studyArea, sparse = FALSE), ]

    if (nrow(lakesStudyArea) == 0) {
      stop("No lakes found within the study area")
    }

    # Prepare lake data
    lakesData <- lakesStudyArea %>%
      as.data.frame() %>%
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

    # Get climate data
    climateRasters <- list.files(
      CLIMATE_DATA_PATH,
      full.names = TRUE,
      pattern = ".nc",
      recursive = TRUE
    )
    lakesClimate <- getClimateData(lakesData, climateRasters)
    lakesClimate <- lakesClimate %>% filter(year >= startYear & year <= endYear)

    # Predict ice on
    iceOnModel <- readRDS(ICE_ON_MODEL_PATH)
    lakesIceOn <- lakesClimate %>%
      mutate(winterYear = ifelse(month %in% 1:2, year - 1, year)) %>%
      filter(month %in% c(1, 2, 9:12)) %>%
      group_by(year, LAKEID) %>%
      summarise(
        tmpWinter = mean(tmp),
        preWinter = sum(pre),
        .groups = 'drop'
      ) %>%
      filter(year >= startYear)

    lakesIceOnMorpho <- lakesIceOn %>%
      left_join(lakesData, by = "LAKEID") %>%
      filter(!is.na(tmpWinter))

    lakesIceOnMorpho$iceOnDoy <- predict(
      iceOnModel,
      lakesIceOnMorpho,
      type = "response"
    )

    # Predict ice off
    iceOffModel <- readRDS(ICE_OFF_MODEL_PATH)
    lakesIceOff <- lakesClimate %>%
      mutate(winterYear = ifelse(month %in% 1:5, year - 1, year)) %>%
      filter(month %in% c(12, 1:5)) %>%
      group_by(year, LAKEID) %>%
      summarise(tmpSpring = mean(tmp), preSpring = sum(pre), .groups = 'drop')

    lakesIceOffMorpho <- lakesIceOff %>%
      left_join(lakesData, by = "LAKEID") %>%
      filter(!is.na(tmpSpring))

    lakesIceOffMorpho$iceOffDoy <- predict(
      iceOffModel,
      lakesIceOffMorpho,
      type = "response"
    )

    # Initialize output dataframe
    plotOutputs <- data.frame()

    # Create Plot 1: Ice On Histogram
    p1 <- ggplot(lakesIceOnMorpho, aes(x = iceOnDoy)) +
      geom_histogram(
        bins = 30,
        fill = "steelblue",
        alpha = 0.7,
        color = "white"
      ) +
      theme_classic() +
      labs(
        title = "Ice On Day Distribution",
        x = "Day of Year",
        y = "Frequency"
      ) +
      theme(plot.title = element_text(size = 14, face = "bold"))

    iceOnHistFile <- file.path(transferDir, "IceOnHistogram.png")
    ggsave(iceOnHistFile, p1, width = 8, height = 6, dpi = 300)
    plotOutputs <- rbind(
      plotOutputs,
      data.frame(PlotType = "IceOnHistogram", PlotFile = iceOnHistFile)
    )

    # Create Plot 2: Ice Off Histogram
    p2 <- ggplot(lakesIceOffMorpho, aes(x = iceOffDoy)) +
      geom_histogram(bins = 30, fill = "coral", alpha = 0.7, color = "white") +
      theme_classic() +
      labs(
        title = "Ice Off Day Distribution",
        x = "Day of Year",
        y = "Frequency"
      ) +
      theme(plot.title = element_text(size = 14, face = "bold"))

    iceOffHistFile <- file.path(transferDir, "IceOffHistogram.png")
    ggsave(iceOffHistFile, p2, width = 8, height = 6, dpi = 300)
    plotOutputs <- rbind(
      plotOutputs,
      data.frame(PlotType = "IceOffHistogram", PlotFile = iceOffHistFile)
    )

    # Create Plot 3: Map of Average Ice On
    avgIceOnSummary <- lakesIceOnMorpho %>%
      group_by(Hylak_id = LAKEID) %>%
      summarise(avgIceOn = mean(iceOnDoy, na.rm = TRUE), .groups = 'drop')

    avgIceOn <- lakesStudyArea %>%
      left_join(avgIceOnSummary, by = "Hylak_id") %>%
      st_as_sf()

    p3 <- ggplot(avgIceOn) +
      geom_sf(aes(fill = avgIceOn), size = 3, alpha = 0.8) +
      scale_fill_gradient2(
        low = "blue",
        mid = "yellow",
        high = "red",
        midpoint = mean(avgIceOn$avgIceOn),
        name = "Avg Ice On\n(Day of Year)"
      ) +
      theme_classic() +
      labs(
        title = "Average Ice On Day by Lake",
        x = "Longitude",
        y = "Latitude"
      ) +
      theme(plot.title = element_text(size = 14, face = "bold"))

    mapIceOnFile <- file.path(transferDir, "MapIceOn.png")
    ggsave(mapIceOnFile, p3, width = 10, height = 8, dpi = 300)
    plotOutputs <- rbind(
      plotOutputs,
      data.frame(PlotType = "MapIceOn", PlotFile = mapIceOnFile)
    )

    # Create Plot 4: Map of Average Ice Off
    avgIceOffSummary <- lakesIceOffMorpho %>%
      group_by(Hylak_id = LAKEID) %>%
      summarise(avgIceOff = mean(iceOffDoy, na.rm = TRUE), .groups = 'drop')

    avgIceOff <- lakesStudyArea %>%
      left_join(avgIceOffSummary, by = "Hylak_id") %>%
      st_as_sf()

    p4 <- ggplot(avgIceOff) +
      geom_sf(aes(fill = avgIceOff), size = 3, alpha = 0.8) +
      scale_fill_gradient2(
        low = "blue",
        mid = "yellow",
        high = "red",
        midpoint = mean(avgIceOff$avgIceOff),
        name = "Avg Ice Off\n(Day of Year)"
      ) +
      theme_classic() +
      labs(
        title = "Average Ice Off Day by Lake",
        x = "Longitude",
        y = "Latitude"
      ) +
      theme(plot.title = element_text(size = 14, face = "bold"))

    mapIceOffFile <- file.path(transferDir, "MapIceOff.png")
    ggsave(mapIceOffFile, p4, width = 10, height = 8, dpi = 300)
    plotOutputs <- rbind(
      plotOutputs,
      data.frame(PlotType = "MapIceOff", PlotFile = mapIceOffFile)
    )

    # Save outputs to SyncroSim
    saveDatasheet(myScenario, plotOutputs, "lakeIcePrediction_PlotOutputs")

    message("Lake ice prediction plots generated successfully!")
  },
  error = function(e) {
    stop(paste("Error:", e$message))
  }
)
