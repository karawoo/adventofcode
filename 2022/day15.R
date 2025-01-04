library("tidyverse")

dat <- readLines("input15.txt")

coords <- str_extract_all(dat, "-?\\d+") %>%
  map(as.numeric)

# Part 1

no_beacon <- function(row, coords) {
  sensors <- map(coords, `[`, 1:2)
  beacons <- map(coords, `[`, 3:4)
  dists <- map(coords, function(x) abs(x[1] - x[3]) + abs(x[2] - x[4]))
  end <- map_dbl(coords, max) %>% max()
  start <- map_dbl(coords, min) %>% min()
  beacon_possible <- vector(mode = "logical", length = length(start:end))
  for (i in start:end) {
    beacon_possible_i <- NA
    for (sensor in seq_along(sensors)) {
      dist_to_sensor <- abs(sensors[[sensor]][1] - i) + abs(sensors[[sensor]][2] - row)
      if (dist_to_sensor <= dists[[sensor]] ) {
        beacon_possible_i <- FALSE
        for (beacon in beacons) {
          if (identical(beacon, c(i, row))) {
            beacon_possible_i <- TRUE
          }
        }
        break
      }
    }
    beacon_possible[i] <- beacon_possible_i
  }
  beacon_possible
}

results <- no_beacon(2000000, coords)
sum(!no_beacon(2000000, coords), na.rm = TRUE)
