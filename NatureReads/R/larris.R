###################################################################################################

#' Generates a named list of the region data in a specific provided region
#'
#' @param type The type of region
#' @param name The name of the regional code or area
#' @return The named list containing the information of the specified region
get_region <- function(type, name) {
  if (type == "country") {
    type =  list(country = name)
  } else if (type == "province") {
    type =  list(statprov = name)
  } else if (type == "subnational") {
    type =  list(subnational2 = name)
  } else if (type == "iba") {
    type =  list(iba = name)
  } else if (type == "bcr") {
    type =  list(bcr = name)
  } else if (type == "utm") {
    type =  list(utm = name)
  }
  return (type)
}

###################################################################################################

#' Plots a limited number of the locations of the observations of the given data which can be filtered by a specific species
#'
#' @param build Dataset that will be built with the names of the species
#' @param data Species data with longitude and latitude info
#' @param limit The max number of sightings to plot
#' @return A plot of at most `limit` sightings from `data` showing the name of each species
plot_region <- function(build, data, limit) {
  num <- ifelse(length(data$species_id)>limit, limit, length(data$species_id))
  for (i in 1:num) {
    build<- rbind(build, data[i,])
    build$species_id[i] = get_species_name(data$species_id[i])
  }

  fig <- plot_mapbox(build, lon = ~longitude, lat = ~latitude, split = ~build$species_id, mode = 'scattermapbox', hoverinfo='name')
  fig <- fig %>% layout(mapbox = list(zoom = 10, center = list(lat = ~median(build$latitude), lon = ~median(build$longitude))))
  fig
}

###################################################################################################
