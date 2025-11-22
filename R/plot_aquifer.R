# ============================================================
# START module: R/plot_aquifer.R
# Plotly: Aquifer annual + cumulative decline
# ============================================================

suppressPackageStartupMessages({
  library(plotly)
  library(dplyr)
})

plot_aquifer <- function(res_rv) {
  res <- res_rv()
  req(!is.null(res))

  store <- res$well
  req(!is.null(store))

  pd    <- store$series
  slope <- store$slope

  pd_bar  <- dplyr::filter(pd, !is.na(annual_decline_ft))
  pd_line <- dplyr::filter(pd, !is.na(cum_change_ft))

  bar_y   <- -pd_bar$annual_decline_ft
  y_annot <- suppressWarnings(min(bar_y, na.rm = TRUE) * 1.05)
  rate_txt <- if (is.na(slope)) "rate: n/a" else sprintf("rate: -%.2f ft/yr", slope)

  plot_ly(source = "aquifer") %>%
    add_bars(
      data = pd_bar, x = ~year, y = ~(-annual_decline_ft),
      name = "Annual change (ft)",
      hoverinfo = "text",
      text = ~paste0("Year: ", year,
                     "<br>Annual change: ", round(-annual_decline_ft, 2), " ft",
                     "<br>Paired wells: ", n_wells)
    ) %>%
    add_lines(
      data = pd_line, x = ~year, y = ~cum_change_ft,
      name = "Cumulative change (ft)",
      yaxis = "y2", hoverinfo = "text",
      line = list(width = 2),
      text = ~paste0("Year: ", year,
                     "<br>Cumulative Δ: ", round(-cum_change_ft, 1), " ft")
    ) %>%
    layout(
      title = "Aquifer decline",
      xaxis = list(title = "", range = c(1950, 2024)),
      yaxis = list(title = "Annual change in depth (ft)",
                   autorange = "reversed", tickformat = "+.1f"),
      yaxis2 = list(overlaying = "y", side = "right",
                    title = "Cumulative change (ft)",
                    automargin = TRUE),
      legend = list(x = 0.02, y = 0.98),
      margin = list(l = 70, r = 70, t = 60, b = 60),
      annotations = list(list(
        x = 2024, y = y_annot, xref = "x", yref = "y",
        text = rate_txt, showarrow = FALSE,
        xanchor = "right", yanchor = "bottom"
      ))
    )
}

# ============================================================
# END module: R/plot_aquifer.R
# ============================================================

