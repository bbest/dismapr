# dismapr: NOAA DisMAP Data Retrieval Package
R package that provides functions to easily interact with NOAA's DisMAP (Distribution Mapping and Analysis Portal) APIs

## Overview

DisMapR is an R package that provides functions to easily interact with NOAA's NOAA's [DisMAP - Distribution Mapping and Analysis Portal](https://apps-st.fisheries.noaa.gov/dismap/) APIs listed here:

- [NOAA Distribution Mapping and Analysis Portal (DisMAP) | InPort](https://www.fisheries.noaa.gov/inport/item/66799)

This package allows users to:

1. Download species indicators data (Center of Gravity, depth metrics, etc.)
2. Access and download interpolated biomass raster layers
3. Retrieve survey location points
4. Create visualizations of species distributions

## Installation

```r
# install required R packages
install.packages("RcppSimdJson")
install.packages("arcgis", repos = "https://r-arcgis.r-universe.dev")

# install dismapr R package from Github
devtools::install_github("bbest/dismapr")
```

## Basic Usage

See [Getting Started](articles/dismapr.html)

## Important Notes

- URLs for the DisMAP services may change as new data releases become available. Check the [DisMAP InPort page](https://www.fisheries.noaa.gov/inport/item/66799) for the most up-to-date URLs.

## Developer Notes

The package is currently in development and may not include all features or be fully tested.

Common tasks for package development include:

```r
# update documentation after modifying any R/*.R files
devtools::document()

# build website
pkgdown::build_site()
```

### Contributing

Contributions are welcome! If you have suggestions for improvements or new features, please open an issue or submit a pull request.

To contribute to the package, please fork the repository and submit a pull request with your changes.

## References

- NOAA DisMAP portal: https://apps-st.fisheries.noaa.gov/dismap/DisMAP.html
- Technical documentation: https://www.fisheries.noaa.gov/inport/item/66799

## Citation

If using this package for research, please cite both the package and the NOAA DisMAP data sources.
