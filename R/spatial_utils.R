# ============================================================
# START module: R/spatial_utils.R
# Spatial helpers for planar CRS operations and simplification
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(raster)
})

st_intersects_planar <- function(a, b, sparse = FALSE) {
  b  <- tryCatch(sf::st_make_valid(b), error = function(e) b)
  cr <- sf::st_crs(5070)

  aa <- tryCatch(sf::st_transform(a, cr), error = function(e) a)
  bb <- tryCatch(sf::st_transform(b, cr), error = function(e) b)

  suppressWarnings(sf::st_intersects(aa, bb, sparse = sparse))
}

simplify_for <- function(sf_poly_in_crs_of_raster, r) {
  if (raster::isLonLat(r))
    return(sf::st_make_valid(sf_poly_in_crs_of_raster))

  dx  <- suppressWarnings(raster::res(r)[1])
  tol <- as.numeric(dx) * 0.35
  if (!is.finite(tol) || tol <= 0) tol <- 10

  sf_poly_in_crs_of_raster |>
    sf::st_make_valid() |>
    sf::st_simplify(dTolerance = tol, preserveTopology = TRUE)
}

# ============================================================
# END module: R/spatial_utils.R
# ============================================================

