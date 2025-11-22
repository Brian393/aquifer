# ============================================================
# START module: R/query_parser.R
# Query-string → sf polygon conversion (WKT, GeoJSON, BBOX)
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(jsonlite)
})

ensure_closed_ring <- function(mat) {
  if (!all(mat[1, ] == mat[nrow(mat), ]))
    rbind(mat, mat[1, ])
  else
    mat
}

geojson_to_sf <- function(obj) {
  if (is.list(obj) && !is.null(obj$type) && obj$type == "FeatureCollection") {
    if (!length(obj$features)) stop("Empty FeatureCollection")
    f <- obj$features[[1]]
    if (!is.null(f$geometry)) obj <- f$geometry else stop("No geometry in first feature")
  }

  if (is.list(obj) && !is.null(obj$type) && obj$type == "Feature") {
    if (is.null(obj$geometry)) stop("Feature missing geometry")
    obj <- obj$geometry
  }

  if (is.null(obj$type)) stop("Invalid GeoJSON (no type)")

  if (obj$type == "Polygon") {
    ring <- obj$coordinates[[1]]
    mat <- do.call(rbind, lapply(ring, function(pt) c(as.numeric(pt[[1]]), as.numeric(pt[[2]]))))
    mat <- ensure_closed_ring(mat)
    return(st_sf(geometry = st_sfc(st_polygon(list(mat)), crs = 4326)))
  }

  if (obj$type == "MultiPolygon") {
    polys <- lapply(obj$coordinates, function(poly) {
      ring <- poly[[1]]
      mat <- do.call(rbind, lapply(ring, function(pt) c(as.numeric(pt[[1]]), as.numeric(pt[[2]]))))
      mat <- ensure_closed_ring(mat)
      st_polygon(list(mat))
    })
    return(st_sf(geometry = st_sfc(st_multipolygon(polys), crs = 4326)))
  }

  stop(sprintf("Unsupported GeoJSON type: %s", obj$type))
}

parse_query_polygon <- function(qs) {
  if (!is.null(qs$wkt) && nzchar(qs$wkt)) {
    sfc <- tryCatch(sf::st_as_sfc(qs$wkt, crs = 4326),
                    error = function(e) NULL)
    if (!is.null(sfc)) return(st_sf(geometry = sfc))
  }

  if (!is.null(qs$geojson) && nzchar(qs$geojson)) {
    obj <- tryCatch(jsonlite::fromJSON(qs$geojson, simplifyVector = FALSE),
                    error = function(e) NULL)
    if (!is.null(obj)) {
      sf <- tryCatch(geojson_to_sf(obj), error = function(e) NULL)
      if (!is.null(sf)) return(sf)
    }
  }

  if (!is.null(qs$bbox) && nzchar(qs$bbox)) {
    v <- suppressWarnings(as.numeric(strsplit(qs$bbox, ",")[[1]]))
    if (length(v) == 4 && all(is.finite(v))) {
      mat <- rbind(
        c(v[1], v[2]),
        c(v[3], v[2]),
        c(v[3], v[4]),
        c(v[1], v[4]),
        c(v[1], v[2])
      )
      return(st_sf(geometry = st_sfc(st_polygon(list(mat)), crs = 4326)))
    }
  }

  NULL
}

# ============================================================
# END module: R/query_parser.R
# ============================================================

