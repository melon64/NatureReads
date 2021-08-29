#' Finds observations in a region defined by a polygon
#'
#' @param region list with numeric matrices with points in rows
#' @param dataset data.frame of observations
#' @return `dataset` of observations inside `region`
observations_in_region <- function(region, dataset) {
  st_intersection(st_as_sf(dataset, coords = c("longitude", "latitude"), dim = "XY"), st_polygon(region))
}

#' Calculates the relative observation trend for a species
#'
#' @param species_id species_id of the chosen species
#' @param dataset data.frame of observations
#' @return relative observation trends for the `species_id` in the `dataset`
relative_observation_trends <- function(species_id, dataset) {
  order_taxon = meta_species_taxonomy()[meta_species_taxonomy()$species_id == species_id, ]$order_taxon[[1]]

  species_count = length(which(dataset$species_id == species_id))
  order_count = length(which(dataset$species_id %in% meta_species_taxonomy()[meta_species_taxonomy()$order_taxon == order_taxon, ]$species_id))
  species_count/order_count
}

#' Plots the line graph of the number of sightings per year
#'
#' @param dataset data.frame of observations
#' @return Line graph of the number of sightings per year for `dataset`
plot_sightings_per_year <- function(dataset) {
  sightings_per_year = as.data.frame(table(dataset$survey_year))
  plot_ly(x = ~sightings_per_year$Var1, y=~sightings_per_year$Freq, type = 'scatter', mode = 'lines') %>%
    layout(title = 'Number of Sightings Per Year', xaxis = list(title = 'Year'), yaxis = list(title = 'Number of Sightings'))
}

#' Plots the bar graph of the number of sightings per province
#'
#' @param dataset data.frame of observations
#' @return Bar graph of the number of sightings per province for `dataset`
plot_sight_per_prov <- function(dataset) {
  sightings_per_province = as.data.frame(table(dataset$statprov_code))
  plot_ly(x=~sightings_per_province$Var1, y=~sightings_per_province$Freq, type = "bar") %>%
    layout(title = 'Number of Sightings Per Province', xaxis = list(title = 'Province'), yaxis = list(title = 'Number of Sightings'))
}
