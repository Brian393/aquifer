# ============================================================
# START module: R/plot_ppt.R
# Plotly: Irrigation AF vs Rainfall AF
# ============================================================

suppressPackageStartupMessages({
  library(plotly)
  library(dplyr)
})

plot_ppt <- function(res_rv) {
  res <- res_rv()
  req(!is.null(res))

  df <- res$combined
  req(is.data.frame(df), nrow(df) > 0)

  fmt <- function(x) ifelse(is.na(x), "NA",
                            formatC(x, format="f", digits=0, big.mark=","))

  df_rain <- filter(df, !is.na(precip_af))
  df_irr  <- filter(df, !is.na(irrigation_af))

  ymax <- suppressWarnings(max(df$irrigation_af, df$precip_af, na.rm = TRUE))
  if (!is.finite(ymax)) ymax <- 1

  plot_ly() %>%
    add_lines(
      data = df_rain, x = ~year, y = ~precip_af,
      name = "Rainfall over irrigated land (acre-feet)",
      mode = "lines+markers",
      text = ~paste0("Year: ", year,
                     "<br>Rainfall over irrigated land: ", fmt(precip_af), " AF"),
      hoverinfo = "text"
    ) %>%
    add_lines(
      data = df_irr, x = ~year, y = ~irrigation_af,
      name = "Irrigation (acre-feet)",
      mode = "lines+markers",
      text = ~paste0("Year: ", year,
                     "<br>Irrigation: ", fmt(irrigation_af), " AF"),
      hoverinfo = "text"
    ) %>%
    layout(
      title = "Irrigation (Kansas only)",
      xaxis = list(title = "",
                   range = c(1990, max(df$year, na.rm = TRUE))),
      yaxis = list(title = "Volume (acre-feet / year)",
                   rangemode = "tozero",
                   range = c(0, ymax * 1.05)),
      legend = list(
      x = 0,           # flush to left edge
      xanchor = "left",
      y = 1,           # top
      yanchor = "top",
      orientation = "v"  # vertical legend (optional, but cleaner)
    )
  )
}

# ============================================================
# END module: R/plot_ppt.R
# ============================================================

