# ============================================================
# START module: R/preset_utils.R
# Preset polygon loading, preset matching, UI helpers, prewarm
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

# ------------------------------
# Load preset layers
# ------------------------------

load_counties <- function() {
  ks <- tryCatch(readRDS("counties_sf.rds"), error = function(e) NULL)
  if (is.null(ks)) return(NULL)

  if (is.na(sf::st_crs(ks))) {
    sf::st_crs(ks) <- 4326
  }

  ks <- sf::st_make_valid(ks)

  if (!("row_id" %in% names(ks))) {
    ks$row_id <- seq_len(nrow(ks))
  }

  ks
}

load_gmds <- function() {
  g <- tryCatch(readRDS("gmds_sf.rds"), error = function(e) NULL)
  if (is.null(g)) return(NULL)

  g <- tryCatch(sf::st_transform(g, 4326), error = function(e) g)
  g <- sf::st_make_valid(g)

  if (!("row_id" %in% names(g))) {
    g$row_id <- seq_len(nrow(g))
  }

  g
}

# ------------------------------
# Preset matching
# ------------------------------

attempt_preset <- function(qs, ks_counties, gmds_sf) {
  # Expect qs$preset of the form "GMD:<id>" or "COUNTY:<id>"
  if (is.null(qs$preset) || !nzchar(qs$preset)) {
    return(NULL)
  }

  preset_key <- toupper(qs$preset)
  parts      <- strsplit(preset_key, ":", fixed = TRUE)[[1]]

  if (length(parts) != 2) return(NULL)

  kind <- parts[1]
  idx  <- suppressWarnings(as.integer(parts[2]))
  if (!is.finite(idx)) return(NULL)

  # Helper to find a "name" column in an sf row
  get_name_from_poly <- function(poly, fallback) {
    name_col <- setdiff(names(poly), c("row_id", attr(poly, "sf_column")))
    if (length(name_col) >= 1) {
      as.character(poly[[name_col[1]]])
    } else {
      fallback
    }
  }

  # --- GMD preset ---
  if (kind == "GMD" && !is.null(gmds_sf) && idx %in% gmds_sf$row_id) {
    poly <- gmds_sf[gmds_sf$row_id == idx, , drop = FALSE]

    gmd_name <- get_name_from_poly(poly, paste("GMD", idx))

    return(list(
      poly     = poly,
      disk_key = KEY("RES", paste0("GMD:", idx)),
      label    = gmd_name
    ))
  }

  # --- COUNTY preset ---
  if (kind == "COUNTY" && !is.null(ks_counties) && idx %in% ks_counties$row_id) {
    poly <- ks_counties[ks_counties$row_id == idx, , drop = FALSE]

    county_name <- get_name_from_poly(poly, "Selected Area")

    return(list(
      poly     = poly,
      disk_key = KEY("RES", paste0("COUNTY:", idx)),
      label    = county_name
    ))
  }

  # No matching preset
  NULL
}

# ------------------------------
# UI helpers
# ------------------------------

missing_geometry_ui <- function() {
  div(
    style = "margin-top:12px;color:#7f1d1d;background:#fef2f2;
             border:1px solid #fecaca;padding:10px;border-radius:8px;",
    HTML("<b>No polygon provided.</b> Pass one of:<br>
         • <code>?wkt=POLYGON((lon lat,...))</code><br>
         • <code>?geojson={...}</code> (Polygon/MultiPolygon/Feature...)<br>
         • <code>?bbox=minx,miny,maxx,maxy</code>")
  )
}

format_area <- function(km2) {
  if (!is.finite(km2)) return("Area unavailable")
  paste0(
    "Approx area: ",
    formatC(km2, format = "f", digits = 1, big.mark = ","),
    " km²"
  )
}

select_key_source <- function(qs) {
  if (!is.null(qs$wkt)    && nzchar(qs$wkt))    return(qs$wkt)
  if (!is.null(qs$geojson) && nzchar(qs$geojson)) return(qs$geojson)
  if (!is.null(qs$bbox)   && nzchar(qs$bbox))   return(qs$bbox)
  "anon"
}

# ------------------------------
# Prewarm driver
# ------------------------------

prewarm_main <- function(kind,
                         ks_counties,
                         gmds_sf,
                         lu_files, lu_years, tif_dir,
                         irr_mask, wells_sf, aquifer_yearly) {

  kind <- match.arg(kind, c("GMD", "COUNTY", "ALL"))
  message("[Prewarm] Starting (", kind, ") …")

  do_gmd    <- kind %in% c("GMD", "ALL")
  do_county <- kind %in% c("COUNTY", "ALL")

  old_s2 <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)

  # --- GMD prewarm ---
  if (do_gmd && !is.null(gmds_sf)) {
    for (i in seq_len(nrow(gmds_sf))) {
      idx      <- gmds_sf$row_id[i]
      disk_key <- KEY("RES", paste0("GMD:", idx))

      if (!is.null(cache_get(disk_key))) {
        message("[Prewarm] GMD ", idx, " cached — skip")
        next
      }

      poly <- tryCatch(
        sf::st_make_valid(gmds_sf[i, , drop = FALSE]),
        error = function(e) gmds_sf[i, , drop = FALSE]
      )

      tryCatch({
        res <- compute_result(
          poly,
          lu_files, lu_years, tif_dir,
          irr_mask, wells_sf, aquifer_yearly,
          cache_dir = NULL
        )
        cache_set(disk_key, res)
        message("[Prewarm] GMD ", idx, " done")
      }, error = function(e) {
        message("[Prewarm] GMD ", idx, " failed: ", e$message)
      })
    }
  }

  # --- COUNTY prewarm ---
  if (do_county && !is.null(ks_counties)) {
    n <- nrow(ks_counties)

    for (i in seq_len(n)) {
      idx      <- ks_counties$row_id[i]
      disk_key <- KEY("RES", paste0("COUNTY:", idx))

      if (!is.null(cache_get(disk_key))) {
        next
      }

      poly <- tryCatch(
        sf::st_make_valid(ks_counties[i, , drop = FALSE]),
        error = function(e) ks_counties[i, , drop = FALSE]
      )

      tryCatch({
        res <- compute_result(
          poly,
          lu_files, lu_years, tif_dir,
          irr_mask, wells_sf, aquifer_yearly,
          cache_dir = NULL
        )
        cache_set(disk_key, res)
        if (i %% 10 == 0) {
          message("[Prewarm] … ", i, "/", n)
        }
      }, error = function(e) {
        message("[Prewarm] COUNTY row ", i, " failed: ", e$message)
      })
    }
  }

  message("[Prewarm] Finished. Cache dir: ",
          normalizePath(getOption("hp_cache_dir", ".")))
}

# ============================================================
# END module: R/preset_utils.R
# ============================================================

