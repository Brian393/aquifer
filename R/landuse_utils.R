# ============================================================
# START module: R/landuse_utils.R
# Land-use categories, colors, labels, and weights
# ============================================================

suppressPackageStartupMessages({
  library(tibble)
})

landuse_colors <- c(
  "0"="#000000","1"="#A25332","2"="#F081E9","3"="#B1355F",
  "4"="#FFA42A","5"="#D2B18D","6"="#E6E539","7"="#A022EA",
  "8"="#4D4D4D","9"="#0000FA","10"="#FF170D","11"="#D2D2D2",
  "12"="#00FF3E","13"="#00BE2E","14"="#007E1F","15"="#C4D5A0",
  "16"="#E7FFC1","17"="#009797","18"="#00FEFF"
)

landuse_labels <- c(
  "0"="Unclassified","1"="Corn","2"="Cotton","3"="Sorghum",
  "4"="Soybeans","5"="Wheat/Small Grains","6"="Alfalfa/Hay",
  "7"="Other Crops","8"="Fallow","9"="Open Water","10"="Developed",
  "11"="Barren","12"="Deciduous Forest","13"="Mixed Forest",
  "14"="Evergreen Forest","15"="Shrubland","16"="Grassland",
  "17"="Woody Wetland","18"="Herbaceous Wetland"
)

irr_weights_default <- tibble::tibble(
  label = c("Corn","Alfalfa/Hay","Cotton","Soybeans",
            "Sorghum","Wheat/Small Grains","Other Crops"),
  w     = c(1.0,     0.9,          1.0,      0.7,
            0.5,     0.2,                0.5)
)

format_area_ticks <- function(x) {
  vapply(x, function(v) {
    if (abs(v) >= 1e5)
      paste0(format(round(v/1e6, 1), nsmall = 1, trim = TRUE), " million")
    else
      formatC(v, format = "f", digits = 0, big.mark = ",")
  }, character(1))
}

# ============================================================
# END module: R/landuse_utils.R
# ============================================================

