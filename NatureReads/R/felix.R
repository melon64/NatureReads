
###################################################################################################

#' Plots the estimated migration path of a species filtered by year.
#'
#' @param speciesData Data for a single species
#' @return A plotly mapbox figure of segments making up the path of a species with a togglable year filter.
plot_path <- function(speciesData){

  speciesLocation <- data.frame(speciesData[10],speciesData[11], speciesData[16])

  speciesNextLocation <- speciesLocation[-c(1), ]
  speciesNextLocation$survey_year <- NULL
  speciesLocation <- speciesLocation[-nrow(speciesLocation), ]

  colnames(speciesNextLocation)[1] <- "nextlat"
  colnames(speciesNextLocation)[2] <- "nextlong"

  traceSpecies <- data.frame(speciesLocation, speciesNextLocation)

  fig <- plot_mapbox(traceSpecies, lon = ~longitude, lat = ~latitude, split = ~survey_year, mode = 'scattermapbox', hoverinfo='name')
  fig <- fig %>% add_paths(size = I(1))
  fig <- fig %>% add_segments(traceSpecies, x = ~longitude, xend = ~nextlong, y = ~latitude, yend= ~nextlat)
  fig <- fig %>% layout(
    plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
    mapbox = list(style = 'dark',
                  zoom = 1.5,
                  center = list(lat = ~median(traceSpecies$latitude),
                                lon = ~median(traceSpecies$longitude))),
    margin = list(l = 0, r = 0,
                  b = 0, t = 0,
                  pad = 0),
    showlegend=TRUE)
  return(fig)
}

###################################################################################################

#' Plots the population density of a species.
#'
#' @param speciesData Data for a single species
#' @return A plotly densitymapbox figure of the species location data.

plot_density <- function(speciesData){
  fig <- speciesData
  fig <- fig %>%
    plot_ly(
      type = 'densitymapbox',
      lat = ~latitude,
      lon = ~longitude,
      coloraxis = 'coloraxis',
      radius = 10)
  fig <- fig %>%
    layout(
      mapbox = list(
        style="stamen-terrain",
        zoom = 1.5,
        center = list(lat = median(speciesData$latitude),
                      lon = median(speciesData$longitude)), coloraxis = list(colorscale = "Jet")))
  return(fig)
}

###################################################################################################

#' Gets (or plots the distribution of) the top n most frequent species in an area.
#'
#' @param areaData Data for a specific area.
#' @param ranking Number for the top n most common species in the area.
#' @param plot Whether to plot or just return the data.
#' @return If `plot` is false, a n-row dataframe containing the top n most common species
#'         with columns containing species_id, frequency, and species_name is returned.
#'         If `plot` is true, a plotly pie plot of the population distribution based on the generated
#'         dataframe is returned.
get_hierarchy <- function(areaData, ranking, plot=FALSE) {
  rankings = as.data.frame(sort(table(areaData$species_id),decreasing=TRUE)[1:ranking])
  names <- character(ranking)
  ids <- rankings[['Var1']]
  metaspecies = meta_species_taxonomy()
  for(i in 1:nrow(rankings)){
    names[i] <- metaspecies[metaspecies$species_id == rankings[i,"Var1"], ]$english_name[[1]]
  }
  rankings$Names <- mapvalues(rankings$Var1,
                              from = ids,
                              to = names)
  if(plot){
    fig <- plot_ly(rankings, labels = ~Names, values = ~Freq, type = 'pie')
    fig <- fig %>% layout(title = 'Species Population Distribution',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    return(fig)
  }
  return(rankings)
}

###################################################################################################
