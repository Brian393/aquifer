# ============================================================
# START module: R/irrigation_utils.R
# Irrigation mask loading, irrigated area calculations,
# empirical weight derivation
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(raster)
  library(exactextractr)
  library(dplyr)
  library(tibble)
})

# -----------------------------
# Load irrigation mask from a file path
# -----------------------------
load_irrigation_mask <- function(irr_mask_path) {
  if (is.null(irr_mask_path) || !file.exists(irr_mask_path)) {
    message("[irr_mask] Not found: ", irr_mask_path)
    return(NULL)
  }
  r <- tryCatch(raster::raster(irr_mask_path), error = function(e) NULL)
  if (is.null(r)) return(NULL)

  raster::NAvalue(r) <- 0
  r
}

# -----------------------------
# Compute irrigated area (km²)
# -----------------------------
compute_irrigated_area_km2 <- function(sf_poly, irr_mask) {
  if (is.null(irr_mask)) return(NA_real_)
  poly_aea <- tryCatch(sf::st_transform(sf_poly, raster::crs(irr_mask)),
                       error = function(e) NULL)
  if (is.null(poly_aea)) return(NA_real_)

  ex <- tryCatch(exactextractr::exact_extract(irr_mask, poly_aea)[[1]],
                 error = function(e) NULL)
  if (is.null(ex) || !"coverage_fraction" %in% names(ex) || !"value" %in% names(ex))
    return(NA_real_)

  cell_area_km2 <- prod(raster::res(irr_mask)) / 1e6
  is_irrig <- ex$value == 1
  is_irrig[is.na(is_irrig)] <- FALSE

  sum(ex$coverage_fraction[is_irrig]) * cell_area_km2
}

get_kansas_calib_path <- function(lu_dir) {
  cand  <- c(2008, 2007, 2009, 2010, 2011, 2012)
  files <- list.files(lu_dir, pattern = "blulc\\d{4}_5070_cog\\.tif$", full.names = TRUE)
  yrs   <- as.integer(stringr::str_extract(basename(files), "\\d{4}"))
  i     <- match(cand, yrs, nomatch = 0)
  if (any(i > 0)) files[i[i > 0][1]] else NA_character_
}

compute_empirical_weights_kansas <- function(sf_poly, lu_dir, irr_mask) {
  if (is.null(irr_mask)) return(NULL)

  calib_path <- get_kansas_calib_path(lu_dir)
  if (is.na(calib_path) || !file.exists(calib_path)) return(NULL)

  r_lu <- get_lu_raster(calib_path)
  if (is.null(r_lu)) return(NULL)

  poly_aea <- tryCatch(sf::st_transform(sf_poly, sf::st_crs(r_lu)),
                       error = function(e) NULL)
  if (is.null(poly_aea)) return(NULL)

  win <- raster::extent(sf::st_bbox(poly_aea)) + 2000
  r_lu_c   <- tryCatch(raster::crop(r_lu,     win), error = function(e) NULL)
  r_mask_c <- tryCatch(raster::crop(irr_mask, win), error = function(e) NULL)
  if (is.null(r_lu_c) || is.null(r_mask_c)) return(NULL)

  same <- tryCatch(
    isTRUE(raster::compareRaster(
      r_lu_c, r_mask_c,
      extent=TRUE, res=TRUE, crs=TRUE, orig=TRUE, rotation=TRUE)),
    error = function(e) FALSE
  )
  if (!same) return(NULL)

  stk <- raster::stack(r_lu_c, r_mask_c)
  names(stk) <- c("lu", "irr")

  ex <- tryCatch(exactextractr::exact_extract(stk, poly_aea)[[1]],
                 error = function(e) NULL)
  if (is.null(ex) || !all(c("coverage_fraction","lu","irr") %in% names(ex)))
    return(NULL)

  cell_km2 <- prod(raster::res(r_lu_c)) / 1e6
  keep_codes <- as.numeric(setdiff(names(landuse_labels), "0"))

  d <- tibble::as_tibble(ex) %>%
    dplyr::filter(!is.na(lu), !is.na(irr), lu %in% keep_codes) %>%
    dplyr::mutate(is_irrig = irr == 1) %>%
    dplyr::group_by(lu, is_irrig) %>%
    dplyr::summarise(area_km2 = sum(coverage_fraction) * cell_km2, .groups = "drop") %>%
    dplyr::group_by(lu) %>%
    dplyr::summarise(
      w_emp = sum(area_km2[is_irrig], na.rm=TRUE) /
              sum(area_km2, na.rm=TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(label = landuse_labels[as.character(lu)]) %>%
    dplyr::select(label, w_emp) %>%
    dplyr::filter(label %in% irr_weights_default$label) %>%
    dplyr::mutate(w_emp = pmax(0, pmin(1, w_emp)))

  if (!nrow(d)) NULL else d
}

# ============================================================
# END module: R/irrigation_utils.R
# ============================================================

