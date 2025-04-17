## code to prepare `regions` dataset goes here

# read linear boundaries
dm_regions <- arcgislayers::arc_read(
  "https://services2.arcgis.com/C8EMgrsFcRFL6LrL/ArcGIS/rest/services/DisMAP_Regions_CURRENT/FeatureServer/1") |>
  janitor::clean_names() |>
  dplyr::group_by(dataset_code, region, season, distribution_project_code) |>
  dplyr::summarize(.groups = "drop")

stopifnot(all(!duplicated(dm_regions$dataset_code)))
s
# mapView(dm_regions, zcol = "dataset_code")
# dm_regions
# A tibble: 14 × 5
# Groups:   dataset_code, region, season [14]
#    dataset_code region                          season      distribution_project_code     n
#    <chr>        <chr>                           <chr>       <chr>                     <int>
#  1 AI           Aleutian Islands                ""          IDW                         266
#  2 EBS          Eastern Bering Sea              ""          IDW                           5
#  3 ENBS         Eastern and Northern Bering Sea ""          IDW                           7
#  4 GMEX         Gulf of Mexico                  ""          IDW                           9
#  5 GOA          Gulf of Alaska                  ""          IDW                       19414
#  6 HI           Hawai'i Islands                 ""          IDW                         137
#  7 NBS          Northern Bering Sea             ""          IDW                           3
#  8 NEUS_FAL     Northeast US                    "Fall"      IDW                           1
#  9 NEUS_SPR     Northeast US                    "Spring"    IDW                           1
# 10 SEUS_FAL     Southeast US                    "Fall"      IDW                           1
# 11 SEUS_SPR     Southeast US                    "Spring"    IDW                           1
# 12 SEUS_SUM     Southeast US                    "Summer"    IDW                           1
# 13 WC_ANN       West Coast                      "Annual"    IDW                          35
# 14 WC_TRI       West Coast                      "Triennial" IDW                           5

usethis::use_data(dm_regions, overwrite = TRUE)
