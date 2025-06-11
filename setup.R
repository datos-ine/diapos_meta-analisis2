# opciones globales
knitr::opts_chunk$set(
  fig.align = "center",
  out.width = "85%",
  echo = TRUE,
  dpi = 300
)


# paquetes
pacman::p_load(
  patchwork,
  scico,
  janitor,
  meta,
  easystats,
  # flextable,
  kableExtra,
  tidyverse
)

# paleta colorblind-friendly
pal <- scico::scico(n = 9, palette = "tokyo")

# Formato tabla
kbl_format <- function(x) {
 kbl(x) |> 
  kable_styling(bootstrap_options = c("stripped", "condensed", "hover"),
                fixed_thead = TRUE,
                html_font = "Calibri") |>
  row_spec(0, background = "#2f2e4e", color = "white")
}