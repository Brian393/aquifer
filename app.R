# ============================
# app.R — Hydro-Social Plots API (modular)
# ============================

suppressPackageStartupMessages({
  library(shiny); library(sf); library(jsonlite); library(raster)
  library(exactextractr); library(dplyr); library(tidyr); library(plotly)
  library(stringr); library(tibble); library(parallel); library(digest)
})

Sys.setenv(GDAL_CACHEMAX = "512")

# Prefer planar ops
sf::sf_use_s2(FALSE)

# Debug options for server deployment
options(shiny.sanitize.errors = FALSE)
options(shiny.fullstacktrace = TRUE)

# Force CPU cores
options(shiny.hp_cores = 3)

# ----------- Load modules (order matters) -----------
source("R/cache_utils.R")
source("R/spatial_utils.R")
source("R/raster_utils.R")
source("R/landuse_utils.R")
source("R/irrigation_utils.R")
source("R/aquifer_utils.R")
source("R/preset_utils.R")
source("R/query_parser.R")
source("R/compute_result.R")
source("R/plot_ppt.R")
source("R/plot_landuse.R")
source("R/plot_aquifer.R")

# ----------- Global data paths -----------
data_dir <- "data"

tif_dir <- file.path(data_dir, "prism_summer")
lu_dir  <- file.path(data_dir, "landuse")

wells_sf        <- readRDS(file.path(data_dir, "wells", "wells_sf.rds"))
aquifer_yearly  <- readRDS(file.path(data_dir, "aquifer", "wells_HP_clean.rds")) |> st_transform(4326)

ks_counties <- readRDS(file.path(data_dir, "boundaries", "counties_sf.rds"))
gmds_sf     <- readRDS(file.path(data_dir, "boundaries", "gmds_sf.rds"))

lu_files  <- list.files(lu_dir, pattern = "blulc\\d{4}_5070_cog\\.tif$", full.names = TRUE)
lu_years  <- as.integer(stringr::str_extract(basename(lu_files), "\\d{4}"))

irr_mask_path <- file.path(data_dir, "irrigation", "irr_mask_kars_2007_to_2012_aea_250m.tif")
irr_mask      <- load_irrigation_mask(irr_mask_path)

years <- 1949:2024
ppt_years <- 1990:2024

cache_dir <- "data/hp_cache"
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
options(hp_cache_dir = cache_dir)

# ----------- UI -----------
ui <- fluidPage(
  tags$head(
    tags$title("Hydro-Social Plots"),
    tags$style(HTML("
      body { font-family: system-ui, -apple-system, Segoe UI, Roboto, sans-serif; }
      .wrap { max-width: 1200px; margin: 12px auto; padding: 0 12px; }
      h2 { margin: 8px 0 16px; }
      .meta { color:#374151; margin-bottom: 8px; }

      /* Legacy grid (kept for safety) */
      .grid { display: grid; grid-template-columns: 1fr; gap: 16px; }
      @media (min-width: 1100px){
        .grid { grid-template-columns: 1fr 1fr; }
        #aquifer { grid-column: 1 / span 2; }
      }

      .card {
        border:1px solid #e5e7eb;
        border-radius: 10px;
        padding: 8px;
        background:#fff;
      }

      /* ============================================================
         FULL-WIDTH ROWS (aquifer + precipitation)
         ============================================================ */
      .grid-fullwidth {
        display: grid;
        grid-template-columns: 1fr;
        gap: 16px;
      }

      /* ============================================================
         LAND-USE ROW (PLOT + LEGEND)
         ============================================================ */
      .grid-legendrow {
        display: grid;
        grid-template-columns: 4fr 1fr;   /* ~80% / 20% split */
        gap: 16px;
      }

      .sidecard {
        background: #fafafa;
        border: 1px dashed #ccc;
      }
      
      /* Reduce wasted side padding only for the land-use plot card */
#landuse.card.plot-card {
        padding-left: 20;
        padding-right: 20;
      }

      /* Tighten the plotly plot inside the card */
      #landuse .plot-container {
        padding-left: 0 !important;
        padding-right: 0 !important;
      }


      /* ============================================================
         LEGEND STYLING
         ============================================================ */
      .legend-item {
        display: flex;
        align-items: center;
        margin-bottom: 4px;
        font-size: 12px;
      }
      .legend-color {
        width: 12px;
        height: 12px;
        display: inline-block;
        margin-right: 6px;
        border-radius: 2px;
      }

      /* FIX FOR LANDUSE PLOT: remove vertical padding so bars can be taller */
      .card.plot-card {
        padding-top: 0;
        padding-bottom: 0;
      }

      /* Vertically center legend in land-use sidecard when it's shorter */
      #landuse_side {
        display: flex;
        justify-content: flex-start;  /* left-align horizontally */
        align-items: center;          /* center vertically */
      }

      #landuse_side > div {
        max-height: 270px;            /* match plot height */
        overflow-y: auto;             /* scroll if legend gets tall */
      }
    "))
  ),

  div(class="wrap",

      h2(textOutput("titleTxt")),
      div(class="meta", textOutput("areaTxt")),

      # -------------------------
      # Row 1: Aquifer (FULL WIDTH)
      # -------------------------
      div(class = "grid-fullwidth",
          div(id = "aquifer", class = "card",
              plotlyOutput("wellPlotHydro", height = "300px")
          )
      ),

      # -------------------------
      # Row 2: Land Use (PLOT + LEGEND)
      # -------------------------
      div(class = "grid-legendrow",
          div(id = "landuse", class = "card plot-card",
              plotlyOutput("landusePlot", height = "270px")
          ),
          div(id = "landuse_side", class = "card sidecard",
              uiOutput("landuseLegend")   # dynamic legend only, no extra wrapper
          )
      ),

      # -------------------------
      # Row 3: Precipitation (FULL WIDTH)
      # -------------------------
      div(class = "grid-fullwidth",
          div(id = "combined", class = "card",
              plotlyOutput("pptPlot", height = "270px")
          )
      ),

      uiOutput("errBox")
  )
)

# ----------- SERVER -----------
server <- function(input, output, session) {

  res_rv   <- reactiveVal(NULL)
  area_txt <- reactiveVal("—")
  err_ui   <- reactiveVal(NULL)

  # Year selected for the land-use legend (NULL = use latest year)
  clicked_year <- reactiveVal(NULL)

  observeEvent(session$clientData$url_search, {
    qs <- shiny::getQueryString(session)
    message("[aquifer] query: ", paste(capture.output(str(qs)), collapse = " "))

    sf_poly <- NULL
    disk_key <- NULL

    # 0) Try preset=GMD:x or preset=COUNTY:y
    preset_res <- attempt_preset(qs, ks_counties, gmds_sf)
    if (!is.null(preset_res)) {
      sf_poly  <- preset_res$poly
      disk_key <- preset_res$disk_key
    }

    # Dynamic title (uses preset_res, not preset)
    output$titleTxt <- renderText({
      if (!is.null(preset_res) && !is.null(preset_res$label)) {
        paste0("Anthropocene Landscape: ", preset_res$label)
      } else {
        "Anthropocene Landscape: Selected Area"
      }
    })

    # 1) If not preset, try explicit geometry params
    if (is.null(sf_poly)) {
      sf_poly <- parse_query_polygon(qs)
      if (is.null(sf_poly)) {
        err_ui(missing_geometry_ui())
        area_txt("—")
        res_rv(NULL)
        return()
      }
      key_src <- select_key_source(qs)
      poly_key <- paste0("DRAWN:", digest::digest(key_src, algo = "xxhash64"))
      disk_key <- KEY("RES", poly_key)
    }

    # ---- Cache lookup / compute ----
    res <- cache_get(disk_key)
    if (is.null(res)) {
      message("[aquifer] cache MISS → computing: ", disk_key)
      res <- compute_result(
        sf_poly        = sf_poly,
        lu_files       = lu_files,
        lu_years       = lu_years,
        tif_dir        = tif_dir,
        irr_mask       = irr_mask,
        wells_sf       = wells_sf,
        aquifer_yearly = aquifer_yearly,
        cache_dir      = cache_dir
      )
      cache_set(disk_key, res)
    } else {
      message("[aquifer] cache HIT: ", disk_key)
    }

    res_rv(res)
    err_ui(NULL)
    area_txt(format_area(res$area_km2))

    # reset clicked year when area changes
    clicked_year(NULL)
  }, ignoreInit = FALSE, once = TRUE)

  # ---- Legend year: either clicked year or latest year available ----
  legend_year <- reactive({
    res <- res_rv()
    req(res)

    comp <- res$lu_comp
    req(is.data.frame(comp), nrow(comp) > 0)

    year_click <- clicked_year()
    if (!is.null(year_click) && !is.na(year_click)) {
      as.integer(year_click)
    } else {
      suppressWarnings(max(comp$year, na.rm = TRUE))
    }
  })

  # ---- Listen for clicks on the land-use plot ----
  observeEvent(plotly::event_data("plotly_click", source = "landuse"), {
    info <- plotly::event_data("plotly_click", source = "landuse")
    if (!is.null(info) && !is.null(info$x)) {
      clicked_year(info$x)
    }
  }, ignoreNULL = TRUE)

  # ---- Dynamic legend for land use ----
  output$landuseLegend <- renderUI({
  res <- res_rv()
  req(res)

  comp <- res$lu_comp
  req(is.data.frame(comp), nrow(comp) > 0)

  year_sel <- legend_year()

  df_year <- dplyr::filter(comp, year == !!year_sel)

  # Exclude very small categories (≤ 0.1%)
  df_year <- dplyr::filter(df_year, perc > 0.1)

  # ---- SORT from largest to smallest percent ----
  df_year <- dplyr::arrange(df_year, dplyr::desc(perc))

  req(nrow(df_year) > 0)

  # mapping from label -> color
  label_to_code <- function(lbl) names(landuse_labels)[landuse_labels == lbl][1]

  items <- lapply(seq_len(nrow(df_year)), function(i) {
    lbl <- df_year$label[i]
    code <- label_to_code(lbl)
    col  <- landuse_colors[[code]]
    if (is.null(col) || is.na(col)) col <- "#888888"

    perc_txt  <- sprintf("%.1f%%", df_year$perc[i])
    text_line <- paste0(lbl, ": ", perc_txt)

    shiny::tags$div(
      class = "legend-item",
      shiny::tags$span(
        class = "legend-color",
        style = paste0("background:", col, ";")
      ),
      shiny::tags$span(text_line)
    )
  })

  shiny::tagList(
    shiny::tags$div(
      style = "max-height:270px; overflow-y:auto;",
      shiny::tags$div(
        style = "font-weight:bold; margin-bottom:6px;",
        paste0("Land use in ", year_sel)
      ),
      items
    )
  )
})

  # --- Plots and other outputs ---
  output$errBox        <- renderUI(err_ui())
  output$areaTxt       <- renderText(area_txt())
  output$pptPlot       <- renderPlotly(plot_ppt(res_rv))
  output$landusePlot   <- renderPlotly(plot_landuse(res_rv))
  output$wellPlotHydro <- renderPlotly(plot_aquifer(res_rv))
}

# ----------- PREWARM CLI ----------
if (!interactive()) {
  kind <- toupper(Sys.getenv("HP_PREWARM", ""))
  if (nzchar(kind)) {
    prewarm_main(kind, ks_counties, gmds_sf,
                 lu_files, lu_years, tif_dir,
                 irr_mask, wells_sf, aquifer_yearly)
    quit(save = "no")
  }
}

# ----------- Launch ----------
shinyApp(ui, server)

