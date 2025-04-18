# dismapr: NOAA DisMAP data retrieval R package

R package that provides functions to easily interact with NOAA's DisMAP (Distribution Mapping and Analysis Portal) APIs

## Overview

DisMapR is an R package that provides functions to easily interact with NOAA's NOAA's [DisMAP - Distribution Mapping and Analysis Portal](https://apps-st.fisheries.noaa.gov/dismap/) APIs listed here:

- [NOAA Distribution Mapping and Analysis Portal (DisMAP) | InPort](https://www.fisheries.noaa.gov/inport/item/66799)

This package allows users to:

1. Download species indicators data, overall shifts in location and depth over time
2. Access and download interpolated biomass raster layers
3. Retrieve survey location points

## Installation

```r
# install required R packages
install.packages("RcppSimdJson")
install.packages("arcgis", repos = "https://r-arcgis.r-universe.dev")

# install dismapr R package from Github
devtools::install_github("bbest/dismapr")
```

## Usage

See [Getting Started](articles/dismapr.html)

## Disclaimers

URLs for the DisMAP services may change as new data releases become available. Check the [DisMAP InPort page](https://www.fisheries.noaa.gov/inport/item/66799) for the most up-to-date URLs.

## Developer

The package is currently in development and may not include all features or be fully tested.

### History

The initial script 
[DisMAP_Data_Download_API.R](https://github.com/bbest/dismapr/blob/9da16bab6a3361c97d61384de08f208aab3fe065/inst/DisMAP_Data_Download_API.R)
was written by Melissa Karp, adapted by [Claude.ai](https://claude.ai/) into an 
initial R package (see [chat.md](https://github.com/bbest/dismapr/blob/9da16bab6a3361c97d61384de08f208aab3fe065/inst/claude.ai/chat.md)), 
then modified by Ben Best for expanded and working functionality.

### Commands

Common tasks for package development include:

```r
# update documentation after modifying any R/*.R files
devtools::document()

# preview website
pkgdown::build_site()
```

### Documentation

Note that the documentation website <https://bbest.github.io/dismapr> is rendered from source by this Github Action:

- [.github/workflows/pkgdown.yaml](https://github.com/bbest/dismapr/blob/main/.github/workflows/pkgdown.yaml)
  - [pkgdown.yaml · Workflow runs](https://github.com/bbest/dismapr/actions/workflows/pkgdown.yaml)

### Next steps

- [ ] Consider `get_dm_raster_series()` to combine layers years and/or species
- [ ] Explore [`arcgislayers::arc_raster()`](https://developers.arcgis.com/r-bridge/api-reference/arcgislayers/arc_raster.html) for `get_dm_raster()`
- [ ] Use [jsonld](https://docs.ropensci.org/jsonld/) to extract from InPort, eg:
  - [Survey Catch-per-unit-effort CURRENT | InPort](https://www.fisheries.noaa.gov/inport/item/69743)
- [ ] Generate animated GIFs, per Claude.ai suggestion in [example_script.r](https://github.com/bbest/dismapr/blob/9da16bab6a3361c97d61384de08f208aab3fe065/inst/claude.ai/example_script.r#L101-L119)

### Contributing

Contributions are welcome! If you have suggestions for improvements or new features, please open an issue or submit a pull request.

## References

- NOAA DisMAP portal: https://apps-st.fisheries.noaa.gov/dismap/DisMAP.html
- Technical documentation: https://www.fisheries.noaa.gov/inport/item/66799

## Citation

If using this package for research, please cite both the package and the NOAA DisMAP data sources.
