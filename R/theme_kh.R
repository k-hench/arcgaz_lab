fnt_sans <- "Josefin Sans Light"
fnt_serif <- "TeX Gyre Pagella"

theme_set(
  theme_minimal(base_family = fnt_serif,
                base_size = 14) +
    theme(title = element_text(family = fnt_sans,
                               face = "bold"),
          plot.caption =  element_text(family = fnt_serif,
                                       face = "italic"),
          panel.grid = element_blank(),
          plot.tag = element_text(family = fnt_serif,
                                  face = "italic"),
          axis.ticks = element_line(size = .2),
          axis.ticks.length = unit(-4, 'pt'),
          panel.spacing = unit(0,"pt"),
          plot.background = element_blank(),
          panel.background = element_blank())
)

clr_light <- "gray85"
clr_def <- "gray50"
fll_def <- "gray95"
sz_def <- .4

clr1 <- "#db4a32"
clr2 <- "gray30"
clr3 <- "gray60"
clr4 <- "gray90"

def_list <- list(colour = clr_def,
                 fill = fll_def,
                 size = sz_def)

c("rect", "point",
  "line", "boxplot") |> 
  walk(update_geom_defaults,
       new = def_list)