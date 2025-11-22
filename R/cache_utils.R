# ============================================================
# START module: R/cache_utils.R
# Disk + RAM cache for results; key helpers
# ============================================================

suppressPackageStartupMessages({
  library(digest)
})

# Key constructor
KEY <- function(kind, id) paste0("v1:", kind, ":", id)

.ram_cache <- new.env(parent = emptyenv())

.cache_fname <- function(key) {
  paste0(gsub("[^A-Za-z0-9:_-]", "_", key), ".rds")
}

.cache_path <- function(key, cache_dir) {
  file.path(cache_dir, .cache_fname(key))
}

cache_get <- function(key, cache_dir = getOption("hp_cache_dir", NULL),
                      prefer_ram = TRUE) {
  if (prefer_ram && exists(key, envir = .ram_cache, inherits = FALSE)) {
    return(get(key, envir = .ram_cache))
  }
  if (is.null(cache_dir)) return(NULL)
  p <- .cache_path(key, cache_dir)
  if (!file.exists(p)) return(NULL)
  tryCatch(readRDS(p), error = function(e) NULL)
}

cache_set <- function(key, value,
                      cache_dir = getOption("hp_cache_dir", NULL),
                      write_disk = TRUE) {
  assign(key, value, envir = .ram_cache)
  if (write_disk && !is.null(cache_dir)) {
    tryCatch(saveRDS(value, .cache_path(key, cache_dir)),
             error = function(e) NULL)
  }
  invisible(TRUE)
}

# ============================================================
# END module: R/cache_utils.R
# ============================================================

