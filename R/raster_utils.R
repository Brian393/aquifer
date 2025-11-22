# ============================================================
# START module: R/raster_utils.R
# PRISM and land-use raster memoization
# ============================================================

suppressPackageStartupMessages({
  library(raster)
})

.prism_cache <- new.env(parent = emptyenv())
SUMMER_STAT   <- Sys.getenv("SUMMER_STAT", unset = "total")  # "avg" or "total"
SUMMER_MONTHS <- 6L

get_prism_raster <- function(y, tif_dir) {
  key <- paste0(SUMMER_STAT, ":", y)
  if (exists(key, envir = .prism_cache, inherits = FALSE))
    return(get(key, envir = .prism_cache))

  f <- file.path(tif_dir, sprintf("prism_ppt_summer_%s_%d.tif", SUMMER_STAT, y))
  if (!file.exists(f)) return(NULL)

  r <- raster::raster(f)
  assign(key, r, envir = .prism_cache)
  r
}

.lu_cache <- new.env(parent = emptyenv())

get_lu_raster <- function(path) {
  if (exists(path, envir = .lu_cache, inherits = FALSE))
    return(get(path, envir = .lu_cache))

  if (!file.exists(path)) return(NULL)

  r <- raster::raster(path)
  assign(path, r, envir = .lu_cache)
  r
}

# ============================================================
# END module: R/raster_utils.R
# ============================================================

