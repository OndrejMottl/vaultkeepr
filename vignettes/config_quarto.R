#----------------------------------------------------------#
#
#
#                     vaultkeepr
#
#         Config file for Quarto vignettes / articles
#
#
#----------------------------------------------------------#
# Source this at the start of every vignette to get a consistent
# ggplot2 theme and colour palettes that match the pkgdown website.
#
# Inspired by VegVault R/03_Visualisation/00_Config_quarto.R.
# The green / blue roles are SWAPPED vs VegVault:
#   VegVault : bg=amber  navbar=blue  links=olive
#   vaultkeepr: bg=amber  navbar=olive links=blue
#----------------------------------------------------------#

# ── Colours (mirror VegVault colors.json) ─────────────────
col_black <- "#242531"
col_white <- "#ffffff"
col_grey <- "#999999"

# Background / structural
col_beige_light <- "#e6b482" # warm amber  — plot background (= body bg)
col_beige_dark <- "#ae8a7b" # warm tan
col_brown_light <- "#bc7052" # terra cotta — panel background
col_brown_dark <- "#8a554e" # earth brown

# Accent — SWAPPED vs VegVault
col_blue_light <- "#52758f" # slate blue  — primary (links in pkgdown)
col_green_dark <- "#5f634f" # olive green — navbar in pkgdown

# Additional palette entries
col_blue_dark <- "#156064"
col_green_light <- "#3ddc47"
col_purple_dark <- "#4b2f5a"
col_purple_light <- "#aa6da3"

# ── Global ggplot2 theme ───────────────────────────────────
# Mirrors VegVault's theme but with olive strip backgrounds
# (olive = our navbar colour, blue-light = our link colour)
ggplot2::theme_set(
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(
        colour = col_black
      ),
      line = ggplot2::element_line(
        linewidth = 0.1,
        colour    = col_black
      ),
      axis.text = ggplot2::element_text(
        colour = col_black
      ),
      axis.title = ggplot2::element_text(
        colour = col_black
      ),
      panel.grid.major = ggplot2::element_line(
        colour    = col_white,
        linewidth = 0.1
      ),
      panel.grid.minor = ggplot2::element_blank(),
      # Warm amber outer background — same as body bg
      plot.background = ggplot2::element_rect(
        fill   = col_beige_light,
        colour = col_beige_light
      ),
      # Terra cotta panel — same as VegVault
      panel.background = ggplot2::element_rect(
        fill   = col_brown_light,
        colour = col_brown_light
      ),
      # Olive strip — swapped (VegVault uses brown_dark here)
      strip.background = ggplot2::element_rect(
        fill   = col_green_dark,
        colour = col_black
      ),
      strip.text = ggplot2::element_text(
        colour = col_white
      ),
      legend.background = ggplot2::element_rect(
        fill   = col_beige_light,
        colour = NA
      ),
      legend.key = ggplot2::element_rect(
        fill   = col_beige_light,
        colour = NA
      ),
      legend.title = ggplot2::element_text(
        colour = col_black
      ),
      legend.text = ggplot2::element_text(
        colour = col_black
      )
    )
)

# ── knitr chunk defaults ───────────────────────────────────
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.align = "center",
  out.width = "100%"
)

# ── Discrete colour palettes ───────────────────────────────
# For dataset_type — blue/olive swapped vs VegVault
# (VegVault uses green_light for vegetation_plot, blue_dark for traits)
palette_dataset_type <-
  c(
    "vegetation_plot"       = col_blue_light,
    "fossil_pollen_archive" = col_green_dark,
    "traits"                = col_brown_dark,
    "gridpoints"            = col_blue_dark
  )
