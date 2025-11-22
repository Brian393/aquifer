# ============================================================
# START module: R/plot_landuse.R
# Plotly: Stacked bar land-use area
# ============================================================

suppressPackageStartupMessages({
  library(plotly)
  library(dplyr)
})

plot_landuse <- function(res_rv) {
  res <- res_rv()
  req(!is.null(res))

  comp <- res$lu_comp
  req(is.data.frame(comp), nrow(comp) > 0)

  need_cols <- c("year","label","area_km2","perc")
  missing   <- setdiff(need_cols, names(comp))
  req(length(missing) == 0)

  comp$year     <- as.integer(comp$year)
  comp$label    <- as.character(comp$label)
  comp$area_km2 <- as.numeric(comp$area_km2)
  comp$perc     <- as.numeric(comp$perc)

  labs_present   <- sort(unique(comp$label))
  label_to_code  <- function(lbl) names(landuse_labels)[landuse_labels == lbl][1]
  cols_present   <- unname(landuse_colors[vapply(labs_present, label_to_code,
                                                 FUN.VALUE = character(1))])
  cols_present[is.na(cols_present)] <- "#888888"

  totals_by_year <- comp %>%
    group_by(year) %>%
    summarise(total_km2 = sum(area_km2, na.rm = TRUE), .groups = "drop")

  ymax <- suppressWarnings(max(totals_by_year$total_km2, na.rm = TRUE))
  if (!is.finite(ymax) || ymax <= 0) ymax <- max(comp$area_km2, na.rm=TRUE)
  if (!is.finite(ymax) || ymax <= 0) ymax <- 1

  raw_step <- ymax / 8
  pow10 <- 10 ^ floor(log10(raw_step))
  unit <- raw_step / pow10
  step <- if      (unit <= 1)  1*pow10
          else if (unit <= 2)  2*pow10
          else if (unit <= 5)  5*pow10
          else                10*pow10

  top_tick  <- floor(ymax / step) * step
  tick_vals <- seq(0, max(top_tick, step), by = step)
  tick_texts <- format_area_ticks(tick_vals)

  # NOTE: source = "landuse" so we can use event_data("plotly_click", source = "landuse")
  plot_ly(
    source = "landuse",
    data   = comp,
    x      = ~year,
    y      = ~area_km2,
    color  = ~label,
    colors = cols_present,
    type   = "bar",
    text   = ~paste0(year, " - ", label, ": ", round(perc,1), "%"),
    hoverinfo = "text",
    showlegend = FALSE
  ) %>%
    layout(
      title   = "Land Use by Category",
      barmode = "stack",
      xaxis   = list(title = "", tickfont = list(size = 10)),
      yaxis   = list(
        title = "Area (km²)",
        tickmode = "array", tickvals = tick_vals, ticktext = tick_texts,
        rangemode = "tozero", range = c(0, ymax),
        automargin = TRUE
      ),
      margin  = list(t = 60, b = 40, l = 10, r = 10),
      shapes = list(list(type = "line", xref = "x", yref = "paper",
                         x0 = 2007.5, x1 = 2007.5, y0 = 0, y1 = 1,
                         line = list(dash = "dot", width = 2, color = "#666"))),
      annotations = list(
        list(x = 0, y = 1.01, xref = "paper", yref = "paper",
             text = "Backcasted data (modeled)", showarrow = FALSE,
             xanchor = "left", yanchor = "bottom",
             font = list(size = 12, color="#666")),
        list(x = 1, y = 1.01, xref = "paper", yref = "paper",
             text = "Cropland data (observed)", showarrow = FALSE,
             xanchor = "right", yanchor = "bottom",
             font = list(size = 12, color="#666"))
      )
    )
}

# ============================================================
# END module: R/plot_landuse.R
# ============================================================

