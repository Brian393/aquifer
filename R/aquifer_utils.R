# ============================================================
# START module: R/aquifer_utils.R
# Aquifer (paired-difference) time series + decline slope
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
})

aquifer_paired_diff <- function(aquifer_yearly, sf_poly) {
  M <- st_intersects_planar(aquifer_yearly, sf_poly, sparse = FALSE)
  rows <- if (is.matrix(M)) rowSums(M) > 0 else as.vector(M)
  aq_sel <- aquifer_yearly[rows, , drop = FALSE]
  if (!nrow(aq_sel)) return(NULL)

  pd <- aq_sel %>%
    sf::st_drop_geometry() %>%
    arrange(site_code, year) %>%
    group_by(site_code) %>%
    mutate(d_depth = depth_ft - lag(depth_ft)) %>%
    ungroup() %>%
    group_by(year) %>%
    summarize(
      annual_decline_ft = mean(d_depth, na.rm = TRUE),
      n_wells = n(), .groups = "drop"
    ) %>%
    tidyr::complete(year = 1950:2024) %>%
    arrange(year) %>%
    mutate(cum_change_ft = cumsum(replace_na(annual_decline_ft, 0)))

  slope <- if (sum(!is.na(pd$cum_change_ft)) >= 3) {
    coef(lm(cum_change_ft ~ year, data = pd))[2]
  } else NA_real_

  list(series = pd, slope = as.numeric(slope))
}

# ============================================================
# END module: R/aquifer_utils.R
# ============================================================

