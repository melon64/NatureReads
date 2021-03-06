% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/larris.R
\name{plot_region}
\alias{plot_region}
\title{Plots a limited number of the locations of the observations of the given data which can be filtered by a specific species}
\usage{
plot_region(build, data, limit)
}
\arguments{
\item{build}{Dataset that will be built with the names of the species}

\item{data}{Species data with longitude and latitude info}

\item{limit}{The max number of sightings to plot}
}
\value{
A plot of at most `limit` sightings from `data` showing the name of each species
}
\description{
Plots a limited number of the locations of the observations of the given data which can be filtered by a specific species
}
