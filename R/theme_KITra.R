#' @importFrom monochromeR generate_palette
#' @import ggplot2

#' KITra_colours
#'
#' A list of color palettes for use in ggplot2 plots.
#'
#' @field blues A palette of shades of blue.
#' @field reds A palette of shades of red.
#' @field gradient A gradient palette that goes from red to white to blue.
#' @field cat A palette of 6 colors suitable for categorical data.
KITra_colours = list(
  blues = \(n)monochromeR::generate_palette('#01284F', "go_lighter", n_colours = n),
  reds = \(n)monochromeR::generate_palette('#D60D4C', "go_lighter", n_colours = n),
  gradient = \(n)c(monochromeR::generate_palette('#D60D4C', "go_lighter", n_colours = floor(n/2)),
                     'white',
                     rev(monochromeR::generate_palette('#01284F', "go_lighter", n_colours = floor(n/2)))),
  cat = \(n)c(monochromeR::generate_palette('#D60D4C', "go_lighter", n_colours = ceiling(n/4) + 1)[-1],
              monochromeR::generate_palette('#01284F', "go_lighter", n_colours = ceiling(n/4) + 1)[-1],
              monochromeR::generate_palette('#D60D4C', "go_darker", n_colours = ceiling(n/4)),
              monochromeR::generate_palette('#01284F', "go_darker", n_colours = ceiling(n/4)))[order(1:(4 * ceiling(n/4)) %% 4, decreasing = T)]
)



#' KITra_palettes
#'
#' Extracts a specific palette from the `KITra_colours` list and applies it as a color or fill scale in a ggplot2 plot.
#'
#' @param name The name of the palette to extract.
#' @param n The number of colors to extract from the palette. If not specified, the entire palette is used.
#' @param all_palettes The list of palettes to extract the palette from. Default is `KITra_colours`.
#' @param type The type of palette to return. Can be "discrete" or "continuous". Default is "discrete".
#' @return A palette object, which can be used as an argument in ggplot2's `scale_color_manual` or `scale_fill_manual` functions.
KITra_palettes = function(name, n, all_palettes = KITra_colours, type = c("discrete", "continuous")) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette(n))(n),
               discrete = palette(n)
  )
  structure(out, name = name, class = "palette")
}

#' scale_colour_KITra_discrete
#'
#' A convenience function that applies a specific discrete palette to the color scale of a ggplot2 plot.
#'
#' @param name The name of the palette to use. Default is "cat".
#' @return A scale_color_manual function with the specified palette.
scale_colour_KITra_discrete = function(name = 'cat', n) {
  ggplot2::scale_colour_manual(values = KITra_palettes(name,
                                                       type = "discrete", n = n))
}

#' scale_color_KITra_discrete
#'
#' A convenience function that applies a specific discrete palette to the color scale of a ggplot2 plot.
#'
#' @param name The name of the palette to use. Default is "cat".
#' @return A scale_color_manual function with the specified palette.
scale_color_KITra_discrete = scale_colour_KITra_discrete


#' scale_fill_KITra_discrete
#'
#' A convenience function that applies a specific discrete palette to the fill scale of a ggplot2 plot.
#'
#' @param name The name of the palette to use. Default is "cat".
#' @return A scale_fill_manual function with the specified palette.
scale_fill_KITra_discrete = function(name = 'cat', n) {
  ggplot2::scale_fill_manual(values = KITra_palettes(name,
                                                   type = "discrete",
                                                   n = n))
}

#' scale_colour_KITra_continuous
#'
#' A convenience function that applies a specific continuous palette to the color scale of a ggplot2 plot.
#'
#' @param name The name of the palette to use. Default is "gradient".
#' @return A scale_color_manual function with the specified palette.
scale_colour_KITra_continuous = function(name = 'gradient', n) {
  ggplot2::scale_colour_gradientn(colours = KITra_palettes(name = name,
                                                         type = "continuous",
                                                         n = n))
}

#' scale_color_KITra_continuous
#'
#' A convenience function that applies a specific continuous palette to the color scale of a ggplot2 plot.
#'
#' @param name The name of the palette to use. Default is "gradient".
#' @return A scale_color_manual function with the specified palette.
scale_color_KITra_continuous = scale_colour_KITra_continuous

#' scale_fill_KITra_continuous
#'
#' A convenience function that applies a specific continuous palette to the fill scale of a ggplot2 plot.
#'
#' @param name The name of the palette to use. Default is "gradient".
#' @return A scale_fill_manual function with the specified palette.
scale_fill_KITra_continuous = function(name = 'gradient', n) {
  ggplot2::scale_fill_gradientn(colours = KITra_palettes(name = name,
                                                       type = "continuous", n=n))
}

#' theme_KITra
#'
#' Applies a custom theme to a ggplot2 plot, with custom colors for text and grid elements.
#'
#' @param base_size The base text size to use in the theme. Default is 12.
#' @return A ggplot2 theme object that can be added to a plot using the `+` operator.
theme_KITra <- function(base_size = 12) {
  dark_text <-  '#01284F'
  text <- monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)
  mid_text <-  text[2]
  light_text <-  text[3]

  theme_minimal(base_size = base_size) +
    theme(text = element_text(colour = mid_text, family = "DejaVu Sans", lineheight = 1.1),
          plot.title = element_text(colour = dark_text, family = "DejaVu Sans", size = rel(1.6), margin = margin(12, 0, 8, 0)),
          plot.subtitle = element_text(size = rel(1.1), margin = margin(4, 0, 0, 0)),
          axis.text.y = element_text(colour = light_text, size = rel(0.8)),
          axis.title.y = element_text(size = rel(1), margin = margin(0, 4, 0, 0)),
          axis.text.x = element_text(colour = light_text, size = rel(0.8)),
          axis.title.x = element_text(size = rel(1), margin = margin(0, 4, 0, 0)),
          legend.position = "top",
          legend.justification = 1,
          panel.grid = element_line(colour = "#F3F4F5"),
          plot.caption = element_text(size = rel(0.8), margin = margin(8, 0, 0, 0)),
          plot.margin = margin(0.25, 0.25, 0.25, 0.25,"cm"))
}

#' theme_KITra_void
#'
#' Applies a custom theme to a ggplot2 plot, with custom colors for text and without grid elements
#'
#' @param base_size The base text size to use in the theme. Default is 12.
#' @return A ggplot2 theme object that can be added to a plot using the `+` operator.
theme_KITra_void <- function(base_size = 12) {
  dark_text <-  '#01284F'
  text <- monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)
  mid_text <-  text[2]
  light_text <-  text[3]

  theme_void(base_size = base_size) +
    theme(text = element_text(colour = mid_text, family = "DejaVu Sans", lineheight = 1.1),
          plot.title = element_text(colour = dark_text, family = "DejaVu Sans", size = rel(1.6), margin = margin(12, 0, 8, 0)),
          plot.subtitle = element_text(size = rel(1.1), margin = margin(4, 0, 0, 0)),
          legend.position = "bottom",
          legend.justification = 0,
          plot.caption = element_text(size = rel(0.8), margin = margin(8, 0, 0, 0)),
          plot.margin = margin(0.25, 0.25, 0.25, 0.25,"cm"))
}

