
###################################################################################################

#' Searches for a species by a name, returning the first ID in the search results
#'
#' @param species_name A name related to a species
#' @return The ID of the first search result for `species_name`
get_species_id <- function(species_name) search_species(species_name)$species_id[1]

#' Gets the English name for a species given its ID, falling back to the scientific name
#'
#' @param id The ID of the species
#' @return The English or scientific name of the given species
get_species_name <- function(id) {
  species <- meta_species_taxonomy()[meta_species_taxonomy()$species_id == id,]
  return(if (!is.na(species$english_name)) species$english_name else species$scientific_name)
}

###################################################################################################

#' Plots a limited number of sightings of the given data
#'
#' @param species Species data with longitude and latitude info
#' @param limit_points The max number of sightings to plot
#' @return A plot of at most `limit_points` sightings from `species`
plot_species_limited <- function(species, limit_points = 500) {
  len <- nrow(species)
  locations <- data.frame(species[10], species[11])[seq(1, len, ceiling(len / limit_points)),]

  plot_mapbox(locations, x = ~ longitude, y = ~ latitude, mode = "markers") %>%
    layout(mapbox = list(
      style = "dark",
      zoom = 1.5,
      center = list(lat = ~ median(latitude), lon = ~ median(longitude))))
}

###################################################################################################

#' Gets the province or state containing the most sightings of the species in the given data
#'
#' @param species Data for a single species
#' @return A 2-element vector containing the state/province code (i.e. "ON") and the number of
#'         sightings of the species in that area
species_max_statprov <- function(species)
  unname(unlist(group_by(species, statprov_code) %>%
                  group_map(~ c(.y, nrow(.x))) %>%
                  reduce(function(acc, cur) if (acc[[2]] < cur[[2]]) cur else acc)))

###################################################################################################

#' Gets (or plots) the area containing the most sightings of a species
#'
#' @param species Data for a single species
#' @param granularity The side length of areas to check (in degrees)
#' @param plot Whether to plot or just return the data
#' @param limit_points The max number of points to plot (if `plot` is true)
#' @return If `plot` is false, a 5-element vector with the starting and ending longitude/latitude
#'         coordinates for the area containing the most sightings of the species, as well as the
#'         number of sightings (`c(lon_1, lat_1, lon_2, lat_2, n)`)
#'         If `plot` is true, a plot of at most `limit_points` sightings and a highlighted box
#'         representing the area with the most sightings
species_max_location <- function(species, granularity = 4, plot = F, limit_points = 500) {
  max_location <- group_by(species, longitude %/% granularity, latitude %/% granularity) %>%
    group_map(~ c(.y, nrow(.x))) %>%
    reduce(function(acc, cur) if (acc[[3]] < cur[[3]]) cur else acc)

  lonlat_start <- lapply(max_location[1:2], function(c) c * granularity)
  lonlat_end <- lapply(lonlat_start, function(c) c + granularity)
  n <- max_location[3]

  location <- c(lonlat_start, lonlat_end, n)
  names(location) <- c("lon_start", "lat_start", "lon_end", "lat_end", "n")

  return(if (plot) {
    len <- nrow(species)
    species_locations <- species[seq(1, len, max(1, len %/% limit_points)),]
    l <- unlist(location)

    species_locations %>%
      plot_ly(
        x = ~ longitude, y = ~ latitude,
        type = "scattermapbox", marker = list(size = 8, color = "orange"), mode = "markers") %>%
      add_trace(
        x = c(l[1], l[3], l[3], l[1]), y = c(l[2], l[2], l[4], l[4]),
        marker = list(size = 1, color = "yellow"),
        fill = "toself", fillcolor = "color") %>%
      layout(mapbox = list(
        style = "stamen-terrain",
        center = list(lon = mean(c(l[1], l[3])), lat = mean(c(l[2], l[4]))),
        zoom = 4),
        showlegend = F)
  } else {
    location
  })
}

###################################################################################################

#' Gets (or plots) the most common species in each area of the provided data
#'
#' @param species Any species data
#' @param granularity The side length of areas to analyze (in degrees)
#' @param plot Whether to plot or just return the data
#' @return If `plot` is false, a list of info for each area; the info is a named 2-element list,
#'         containing `region`, which has the starting and ending longitude/latitude coordinates
#'         for that area (`c(lon_1, lat_1, lon_2, lat_2)`), and `max_species`, which is itself a
#'         named 2-element list containing the ID of the most common species in the area, and a
#'         tibble of all the sightings of that species in the area
#'         If `plot` is true, a plot of all the areas in the data which contain sightings, colored
#'         according to the most common species in the area
most_common_across_regions <- function(species, granularity = 4, plot = F) {
  region_maxes <- group_by(species, longitude %/% granularity, latitude %/% granularity) %>%
    group_map(~ list(
      coords = .y,
      species = group_by(.x, species_id) %>% group_map(~ list(id = .y, sightings = .x)))) %>%
    lapply(function(l) {
      region_start <- lapply(l$coords, function(c) c * granularity)
      region_coords <- c(region_start, lapply(region_start, function(c) c + granularity))
      names(region_coords) <- c("lon_start", "lat_start", "lon_end", "lat_end")

      max_species <- reduce(
        l$species,
        function(acc, g) if (length(g$sightings) > length(acc$sightings)) g else acc)
      max_species$id <- max_species$id[[1]]

      return(list(region = region_coords, max_species = max_species))})

  return(if (plot) {
    species_ids <- unique(lapply(region_maxes, function(m) m$max_species$id))
    n_unique <- length(species_ids)
    palette <- brewer.pal(n_unique, "Spectral")
    legend_added <- unlist(lapply(1:n_unique, function(x) F))

    lons <- as.numeric(lapply(region_maxes, function(m) m$region$lon_start))
    lats <- as.numeric(lapply(region_maxes, function(m) m$region$lat_start))

    reduce(
      region_maxes,
      .init = plot_ly(type = "scattermapbox", mode = "markers"),
      function(acc, region) {
        c <- unlist(region$region)
        cur_id <- region$max_species$id

        species_idx <- match(cur_id, species_ids)
        added <- legend_added[species_idx]

        acc <- acc %>% add_trace(
          x = c(c[1], c[3], c[3], c[1]), y = c(c[2], c[2], c[4], c[4]),
          marker = list(size = 1, color = palette[species_idx]),
          fill = "toself", fillcolor = "color", mode = "markers",
          name = if (added) "" else get_species_name(cur_id), showlegend = !added)

        legend_added[species_idx] <<- T
        return(acc)}) %>%
      layout(
        mapbox = list(
          style = "stamen-terrain",
          center = list(lon = median(lons, na.rm = T), lat = median(lats, na.rm = T)),
          zoom = 2),
        showlegend = T)
  } else {
    region_maxes
  })
}

###################################################################################################

