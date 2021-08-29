# NatureReads
An R package for data processing and visualization of the naturecounts dataset.

## Installation

Install the NatureReads package with remotes library in RStudio using this command:

```bash
remotes::install_github("melon64/NatureReads", subdir = "NatureReads")
```
Some plotting features require a [mapbox token](https://www.mapbox.com/) that is free to obtain. Add it to your environment in RStudio as so:

```bash
mapboxToken = "{your token}"
Sys.setenv("MAPBOX_TOKEN" = mapboxToken)
```
