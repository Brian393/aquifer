# ============================================================
# START module: R/compute_result.R
# BLOCK 1: PRISM, land-use, irrigated area
# BLOCK 2: wells, precip AF, aquifer decline
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(raster)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(exactextractr)
  library(parallel)
})

compute_result <- function(sf_poly,
                           lu_files, lu_years,
                           tif_dir,
                           irr_mask,
                           wells_sf,
                           aquifer_yearly,
                           cache_dir = NULL) {

  # ---------------------------
  # BLOCK 1: area, PRISM, land-use, irrigated area
  # ---------------------------

  area_val <- suppressWarnings(
    tryCatch(as.numeric(sf::st_area(sf::st_transform(sf_poly, 5070))) / 1e6,
             error = function(e) NA_real_)
  )

  ppt_years <- 1990:2024

  ppt_df <- lapply(ppt_years, function(y) {
    r <- get_prism_raster(y, tif_dir)
    if (is.null(r)) return(NULL)

    poly_r <- tryCatch(sf::st_transform(sf_poly, sf::st_crs(r)),
                       error = function(e) NULL)
    if (is.null(poly_r)) return(NULL)

    poly_sr <- simplify_for(poly_r, r)
    v <- tryCatch(exactextractr::exact_extract(r, poly_sr, "mean"),
                  error = function(e) NA_real_)
    if (is.na(v)) return(NULL)

    mm_total <- if (identical(SUMMER_STAT, "avg")) v * SUMMER_MONTHS else v
    tibble::tibble(year = y, ppt_mm = mm_total)
  })

  ppt_df <- Filter(Negate(is.null), ppt_df)
  ppt_df <- if (length(ppt_df)) bind_rows(ppt_df) else tibble(year = integer(), ppt_mm = numeric())
  ppt_df <- filter(ppt_df, !is.na(ppt_mm))

  plapply <- function(X, FUN) {
    ncores <- getOption("shiny.hp_cores", 1L)
    safeFUN <- function(...) tryCatch(FUN(...), error = function(e) NULL)
    if (.Platform$OS.type == "windows" || ncores <= 1)
      lapply(X, safeFUN)
    else
      parallel::mclapply(X, safeFUN, mc.cores = ncores, mc.preschedule = FALSE)
  }

  comp_list <- plapply(seq_along(lu_files), function(i) {
    rlu <- get_lu_raster(lu_files[i])
    if (is.null(rlu)) return(NULL)
    if (raster::isLonLat(rlu)) return(NULL)

    poly_pr <- tryCatch(sf::st_transform(sf_poly, sf::st_crs(rlu)),
                        error = function(e) NULL)
    if (is.null(poly_pr)) return(NULL)

    poly_sr <- simplify_for(poly_pr, rlu)
    rlu_crop <- tryCatch(
      raster::crop(rlu, raster::extent(sf::st_bbox(poly_sr)) + 2000),
      error = function(e) NULL
    )
    if (is.null(rlu_crop)) return(NULL)

    ex <- tryCatch(exactextractr::exact_extract(rlu_crop, poly_sr)[[1]],
                   error = function(e) NULL)
    if (is.null(ex)) return(NULL)

    keep_codes    <- as.numeric(setdiff(names(landuse_labels), "0"))
    cell_area_km2 <- prod(raster::res(rlu_crop)) / 1e6

    df <- ex %>%
      as_tibble() %>%
      filter(!is.na(value), value %in% keep_codes) %>%
      group_by(value) %>%
      summarise(area_km2 = sum(coverage_fraction) * cell_area_km2, .groups = "drop") %>%
      mutate(
        year  = lu_years[i],
        label = landuse_labels[as.character(value)]
      ) %>%
      select(year, label, area_km2)

    if (!nrow(df)) return(NULL)

    total <- sum(df$area_km2)
    df$perc  <- if (total > 0) df$area_km2 / total * 100 else 0
    df$label <- factor(df$label, levels = unname(landuse_labels))
    df
  })

  comp_list <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, comp_list)
  lu_comp   <- if (length(comp_list)) bind_rows(comp_list) else NULL

  years <- 1949:2024

  if (is.null(lu_comp) || !nrow(lu_comp)) {
    irr_area_by_year <- tibble(year = years, irrigated_area_m2 = NA_real_)
  } else {
    mask_km2 <- compute_irrigated_area_km2(sf_poly, irr_mask)
    mask_m2  <- if (is.finite(mask_km2) && mask_km2 > 0) mask_km2 * 1e6 else NA_real_

    w_emp <- tryCatch(
      compute_empirical_weights_kansas(sf_poly, lu_dir = dirname(lu_files[1]), irr_mask),
      error = function(e) NULL
    )

    weights_used <- irr_weights_default
    if (!is.null(w_emp)) {
      weights_used <- irr_weights_default %>%
        left_join(w_emp, by = "label") %>%
        mutate(w = coalesce(w_emp, w)) %>%
        select(label, w)
    }

    irr_area_by_year <- lu_comp %>%
      group_by(year, label) %>%
      summarise(area_km2 = sum(area_km2, na.rm = TRUE), .groups = "drop") %>%
      left_join(weights_used, by = "label") %>%
      mutate(w = coalesce(w, 0)) %>%
      group_by(year) %>%
      summarise(irrigated_area_m2 = sum(area_km2 * 1e6 * w), .groups = "drop")

    override_years <- intersect(2003:2012, irr_area_by_year$year)
    if (is.finite(mask_m2)) {
      irr_area_by_year$irrigated_area_m2[
        irr_area_by_year$year %in% override_years
      ] <- mask_m2
    }
  }

  # ---------------------------
  # BLOCK 2: wells, precip AF, aquifer decline
  # ---------------------------

  wells_sel <- {
    M <- st_intersects_planar(wells_sf, sf_poly, sparse = FALSE)
    rows <- if (is.matrix(M)) rowSums(M) > 0 else as.vector(M)
    wells_sf[rows, , drop = FALSE]
  }

  irr_cols <- grep("^AF_USED_\\d{4}$", names(wells_sel), value = TRUE)
  if (length(irr_cols) && nrow(wells_sel)) {
    sums <- sapply(irr_cols, function(col) sum(as.numeric(wells_sel[[col]]), na.rm = TRUE))
    irr_af_df <- tibble(
      year          = as.numeric(sub("AF_USED_", "", irr_cols)),
      irrigation_af = as.numeric(sums)
    )
  } else {
    irr_af_df <- tibble(year = years, irrigation_af = NA_real_)
  }

  precip_af_df <- ppt_df %>%
    left_join(irr_area_by_year, by = "year") %>%
    mutate(
      precip_af = if_else(
        is.finite(irrigated_area_m2) & !is.na(ppt_mm),
        (ppt_mm / 1000) * irrigated_area_m2 / 1233.48,
        NA_real_
      )
    ) %>%
    select(year, precip_af)

  combined <- irr_af_df %>%
    full_join(precip_af_df, by = "year") %>%
    filter(year >= 1990) %>%
    arrange(year)

  well <- aquifer_paired_diff(aquifer_yearly, sf_poly)

  list(
    area_km2 = if (is.finite(area_val)) round(area_val, 1) else NA_real_,
    lu_comp  = lu_comp,
    combined = combined,
    well     = well
  )
}

# ============================================================
# END module: R/compute_result.R
# ============================================================

