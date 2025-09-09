# predict-lake-ice.R
# Lake Ice Prediction Transformer for SyncroSim

# Load required libraries
library(rsyncrosim)
library(sf)
library(terra)
library(tidyverse)
library(lubridate)

# Initialize SyncroSim environment
myScenario <- scenario()

# Load input data
settings <- datasheet(
  myScenario,
  "Settings",
  optional = TRUE,
  includeKey = TRUE
)
studyAreaInput <- datasheet(
  myScenario,
  "StudyAreaInput",
  optional = TRUE,
  includeKey = TRUE
)
modelOptions <- datasheet(
  myScenario,
  "ModelOptions",
  optional = TRUE,
  includeKey = TRUE
)

# Set default values if not provided
if (is.null(modelOptions$predictIceOn)) {
  modelOptions$predictIceOn <- TRUE
}
if (is.null(modelOptions$predictIceOff)) {
  modelOptions$predictIceOff <- TRUE
}
if (is.null(modelOptions$outputFormat)) {
  modelOptions$outputFormat <- 2
}
if (is.null(modelOptions$createPlots)) {
  modelOptions$createPlots <- TRUE
}

# Function to extract climate data
getClimateData <- function(
  data,
  latColumn,
  lonColumn,
  idColumn,
  climateRasters
) {
  tmp <- rast(grep("tmp.dat.nc", climateRasters, value = TRUE))
  pre <- rast(grep("pre.dat.nc", climateRasters, value = TRUE))

  coords <- data %>% select(!!idColumn, !!lonColumn, !!latColumn) %>% distinct()

  ext <- ext(
    min(coords[[lonColumn]]) - 1,
    max(coords[[lonColumn]]) + 1,
    min(coords[[latColumn]]) - 1,
    max(coords[[latColumn]]) + 1
  )
  tmp <- crop(tmp, ext)
  pre <- crop(pre, ext)

  # Extract climate values
  tmpValues <- extract(tmp, as.matrix(coords[, c(lonColumn, latColumn)]))
  tmpValuesCoords <- cbind(coords, tmpValues[, 2:ncol(tmpValues)])
  tmpValuesLong <- tmpValuesCoords %>%
    pivot_longer(
      cols = 4:ncol(tmpValuesCoords),
      names_to = "timestep",
      values_to = "tmp"
    ) %>%
    mutate(
      date = rep(
        seq.Date(as.Date("1901-01-01"), as.Date("2023-12-31"), by = "month"),
        nrow(tmpValuesCoords)
      )
    )

  preValues <- extract(pre, as.matrix(coords[, c(lonColumn, latColumn)]))
  preValuesCoords <- cbind(coords, preValues[, 2:ncol(preValues)])
  preValuesLong <- preValuesCoords %>%
    pivot_longer(
      cols = 4:ncol(preValuesCoords),
      names_to = "timestep",
      values_to = "pre"
    ) %>%
    mutate(
      date = rep(
        seq.Date(as.Date("1901-01-01"), as.Date("2023-12-31"), by = "month"),
        nrow(preValuesCoords)
      )
    )

  climateValues <- cbind(tmpValuesLong, pre = preValuesLong$pre) %>%
    select(-timestep)

  climateValues$year <- as.numeric(format(climateValues$date, "%Y"))
  climateValues$month <- as.numeric(format(climateValues$date, "%m"))

  return(climateValues)
}

# Main processing
tryCatch(
  {
    # Load study area
    studyArea <- st_read(studyAreaInput$studyAreaShapefile)

    # Load lakes data
    lakes <- st_read(settings$lakesDataPath)
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
      settings$climateDataPath,
      full.names = TRUE,
      pattern = ".nc",
      recursive = TRUE
    )

    lakesClimate <- getClimateData(
      lakesData,
      "latitude",
      "longitude",
      "LAKEID",
      climateRasters
    )

    # Filter by year range
    lakesClimate <- lakesClimate %>%
      filter(year >= studyAreaInput$startYear & year <= studyAreaInput$endYear)

    # Initialize output dataframes
    iceOnOutput <- data.frame()
    iceOffOutput <- data.frame()
    summaryStats <- data.frame()

    # Predict ice on
    if (modelOptions$predictIceOn) {
      iceOnModel <- readRDS(settings$iceOnModelPath)

      lakesIceOn <- lakesClimate %>%
        mutate(winterYear = ifelse(month %in% 1:2, year - 1, year)) %>%
        filter(month %in% c(1, 2, 9:12)) %>%
        group_by(year, LAKEID) %>%
        summarise(
          tmpWinter = mean(tmp),
          preWinter = sum(pre),
          .groups = 'drop'
        ) %>%
        filter(year >= studyAreaInput$startYear)

      lakesIceOnMorpho <- lakesIceOn %>%
        left_join(lakesData, by = "LAKEID") %>%
        filter(!is.na(tmpWinter))

      lakesIceOnMorpho$ice_on_doy <- predict(
        iceOnModel,
        lakesIceOnMorpho,
        type = "response"
      )

      iceOnOutput <- lakesIceOnMorpho %>%
        select(
          LAKEID,
          year,
          latitude,
          longitude,
          ice_on_doy,
          tmpWinter,
          preWinter,
          Lake_area,
          Depth_avg
        )
    }

    # Predict ice off
    if (modelOptions$predictIceOff) {
      iceOffModel <- readRDS(settings$iceOffModelPath)

      lakesIceOff <- lakesClimate %>%
        mutate(winterYear = ifelse(month %in% 1:5, year - 1, year)) %>%
        filter(month %in% c(12, 1:5)) %>%
        group_by(year, LAKEID) %>%
        summarise(tmpSpring = mean(tmp), preSpring = sum(pre), .groups = 'drop')

      lakesIceOffMorpho <- lakesIceOff %>%
        left_join(lakesData, by = "LAKEID") %>%
        filter(!is.na(tmpSpring))

      lakesIceOffMorpho$ice_off_doy <- predict(
        iceOffModel,
        lakesIceOffMorpho,
        type = "response"
      )

      iceOffOutput <- lakesIceOffMorpho %>%
        select(
          LAKEID,
          year,
          latitude,
          longitude,
          ice_off_doy,
          tmpSpring,
          preSpring,
          Lake_area,
          Depth_avg
        )
    }

    # Calculate summary statistics
    if (nrow(iceOnOutput) > 0) {
      iceOnStats <- iceOnOutput %>%
        group_by(year) %>%
        summarise(
          mean_ice_on_doy = mean(ice_on_doy, na.rm = TRUE),
          sd_ice_on_doy = sd(ice_on_doy, na.rm = TRUE),
          min_ice_on_doy = min(ice_on_doy, na.rm = TRUE),
          max_ice_on_doy = max(ice_on_doy, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        pivot_longer(cols = -year, names_to = "metric", values_to = "value")

      summaryStats <- bind_rows(summaryStats, iceOnStats)
    }

    if (nrow(iceOffOutput) > 0) {
      iceOffStats <- iceOffOutput %>%
        group_by(year) %>%
        summarise(
          mean_ice_off_doy = mean(ice_off_doy, na.rm = TRUE),
          sd_ice_off_doy = sd(ice_off_doy, na.rm = TRUE),
          min_ice_off_doy = min(ice_off_doy, na.rm = TRUE),
          max_ice_off_doy = max(ice_off_doy, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        pivot_longer(cols = -year, names_to = "metric", values_to = "value")

      summaryStats <- bind_rows(summaryStats, iceOffStats)
    }

    # Calculate ice duration if both on and off are predicted
    if (nrow(iceOnOutput) > 0 & nrow(iceOffOutput) > 0) {
      iceDuration <- iceOnOutput %>%
        select(LAKEID, year, ice_on_doy) %>%
        inner_join(
          iceOffOutput %>% select(LAKEID, year, ice_off_doy),
          by = c("LAKEID", "year")
        ) %>%
        mutate(ice_duration = ice_off_doy - ice_on_doy) %>%
        group_by(year) %>%
        summarise(
          mean_ice_duration = mean(ice_duration, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        pivot_longer(cols = -year, names_to = "metric", values_to = "value")

      summaryStats <- bind_rows(summaryStats, iceDuration)
    }

    # Create plots if requested
    plotOutputs <- data.frame()
    if (modelOptions$createPlots) {
      # Ice on histogram
      if (nrow(iceOnOutput) > 0) {
        p1 <- ggplot(iceOnOutput, aes(x = ice_on_doy)) +
          geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
          theme_minimal() +
          labs(
            title = "Predicted Ice On Day of Year Distribution",
            x = "Day of Year",
            y = "Frequency"
          )

        ice_on_hist_file <- file.path(tempdir(), "ice_on_histogram.png")
        ggsave(ice_on_hist_file, p1, width = 8, height = 6)
        plotOutputs <- rbind(
          plotOutputs,
          data.frame(plotType = "ice_on_histogram", plotFile = ice_on_hist_file)
        )
      }

      # Ice off histogram
      if (nrow(iceOffOutput) > 0) {
        p2 <- ggplot(iceOffOutput, aes(x = ice_off_doy)) +
          geom_histogram(bins = 30, fill = "coral", alpha = 0.7) +
          theme_minimal() +
          labs(
            title = "Predicted Ice Off Day of Year Distribution",
            x = "Day of Year",
            y = "Frequency"
          )

        ice_off_hist_file <- file.path(tempdir(), "ice_off_histogram.png")
        ggsave(ice_off_hist_file, p2, width = 8, height = 6)
        plotOutputs <- rbind(
          plotOutputs,
          data.frame(
            plotType = "ice_off_histogram",
            plotFile = ice_off_hist_file
          )
        )
      }

      # Temporal trends plot
      if (nrow(summaryStats) > 0) {
        trendsData <- summaryStats %>%
          filter(metric %in% c("mean_ice_on_doy", "mean_ice_off_doy"))

        if (nrow(trendsData) > 0) {
          p3 <- ggplot(trendsData, aes(x = year, y = value, color = metric)) +
            geom_line(size = 1.2) +
            geom_point(size = 2) +
            scale_color_manual(
              values = c(
                "mean_ice_on_doy" = "steelblue",
                "mean_ice_off_doy" = "coral"
              ),
              labels = c("Ice On", "Ice Off")
            ) +
            theme_minimal() +
            labs(
              title = "Temporal Trends in Lake Ice Phenology",
              x = "Year",
              y = "Day of Year",
              color = "Event"
            )

          trends_file <- file.path(tempdir(), "temporal_trends.png")
          ggsave(trends_file, p3, width = 10, height = 6)
          plotOutputs <- rbind(
            plotOutputs,
            data.frame(plotType = "temporal_trends", plotFile = trends_file)
          )
        }
      }

      # Spatial plot
      if (nrow(iceOnOutput) > 0 | nrow(iceOffOutput) > 0) {
        # Create a spatial visualization (simplified example)
        if (nrow(iceOnOutput) > 0) {
          avgIceOn <- iceOnOutput %>%
            group_by(LAKEID, latitude, longitude) %>%
            summarise(
              avg_ice_on = mean(ice_on_doy, na.rm = TRUE),
              .groups = 'drop'
            )

          p4 <- ggplot(
            avgIceOn,
            aes(x = longitude, y = latitude, color = avg_ice_on)
          ) +
            geom_point(size = 3) +
            scale_color_gradient2(
              low = "blue",
              mid = "yellow",
              high = "red",
              midpoint = mean(avgIceOn$avg_ice_on)
            ) +
            theme_minimal() +
            labs(
              title = "Spatial Distribution of Average Ice On Day",
              x = "Longitude",
              y = "Latitude",
              color = "Avg Ice On\n(Day of Year)"
            )

          spatial_file <- file.path(tempdir(), "spatial_ice_patterns.png")
          ggsave(spatial_file, p4, width = 10, height = 8)
          plotOutputs <- rbind(
            plotOutputs,
            data.frame(
              plotType = "spatial_ice_patterns",
              plotFile = spatial_file
            )
          )
        }
      }
    }

    # Create spatial outputs if requested
    spatialOutputs <- data.frame()
    if (modelOptions$outputFormat %in% c(1, 2)) {
      # Create shapefiles
      if (nrow(iceOnOutput) > 0) {
        iceOnSpatial <- lakesStudyArea %>%
          select(Hylak_id) %>%
          left_join(iceOnOutput %>% rename(Hylak_id = LAKEID), by = "Hylak_id")

        ice_on_shp <- file.path(tempdir(), "ice_on_predictions.shp")
        st_write(iceOnSpatial, ice_on_shp, quiet = TRUE)
        spatialOutputs <- rbind(
          spatialOutputs,
          data.frame(outputType = "ice_on_shapefile", outputFile = ice_on_shp)
        )
      }

      if (nrow(iceOffOutput) > 0) {
        iceOffSpatial <- lakesStudyArea %>%
          select(Hylak_id) %>%
          left_join(iceOffOutput %>% rename(Hylak_id = LAKEID), by = "Hylak_id")

        ice_off_shp <- file.path(tempdir(), "ice_off_predictions.shp")
        st_write(iceOffSpatial, ice_off_shp, quiet = TRUE)
        spatialOutputs <- rbind(
          spatialOutputs,
          data.frame(outputType = "ice_off_shapefile", outputFile = ice_off_shp)
        )
      }
    }

    if (modelOptions$outputFormat %in% c(0, 2)) {
      # Create CSV outputs
      if (nrow(iceOnOutput) > 0) {
        ice_on_csv <- file.path(tempdir(), "ice_on_predictions.csv")
        write.csv(iceOnOutput, ice_on_csv, row.names = FALSE)
        spatialOutputs <- rbind(
          spatialOutputs,
          data.frame(outputType = "ice_on_csv", outputFile = ice_on_csv)
        )
      }

      if (nrow(iceOffOutput) > 0) {
        ice_off_csv <- file.path(tempdir(), "ice_off_predictions.csv")
        write.csv(iceOffOutput, ice_off_csv, row.names = FALSE)
        spatialOutputs <- rbind(
          spatialOutputs,
          data.frame(outputType = "ice_off_csv", outputFile = ice_off_csv)
        )
      }
    }

    # Save outputs to SyncroSim
    if (nrow(iceOnOutput) > 0) {
      saveDatasheet(myScenario, iceOnOutput, "IceOnPredictions")
    }

    if (nrow(iceOffOutput) > 0) {
      saveDatasheet(myScenario, iceOffOutput, "IceOffPredictions")
    }

    if (nrow(summaryStats) > 0) {
      saveDatasheet(myScenario, summaryStats, "SummaryStatistics")
    }

    if (nrow(plotOutputs) > 0) {
      saveDatasheet(myScenario, plotOutputs, "PlotOutputs")
    }

    if (nrow(spatialOutputs) > 0) {
      saveDatasheet(myScenario, spatialOutputs, "SpatialOutputs")
    }

    message("Lake ice prediction completed successfully!")
  },
  error = function(e) {
    stop(paste("Error in lake ice prediction:", e$message))
  }
)
