# KITra: Custom Color Palettes and Themes for ggplot2

The KITra package provides custom color palettes and themes for use in ggplot2 plots. The package contains a list of color palettes, as well as functions for extracting and applying the palettes in ggplot2 plots.

## Installation

You can install the development version of KITra from GitHub with:

```
install.packages("devtools")

devtools::install_github("yourusername/KITra")
```

## Usage

The package provides several color palettes that can be used in ggplot2 plots, such as "blues", "reds", "gradient" and "cat". To use a palette, you can use the `KITra_palettes()` function to extract it, and then use the `scale_color_*()` or `scale_fill_*()` functions to apply it to a plot.

For example, to create a scatter plot with the "blues" palette, you can use the following code:

```
library(ggplot2)
ggplot(data = mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
geom_point() +
scale_color_KITra_discrete(name = "blues")
```

The package also provides a custom theme `theme_KITra()` that can be added to a plot using the `+` operator. This theme provides custom colors for text and grid elements.

## Inspiration 

This package's theme function was inspired by the [ggplot themes talk](https://www.cararthompson.com/talks/nhsr2022-ggplot-themes/) by Cara Thompson and the custom color palettes were inspired by the [blog post](https://www.jumpingrivers.com/blog/custom-colour-palettes-for-ggplot2/) by Nicola Rennie.

## Issues

If you encounter any problems or bugs, please file an issue on the [GitHub issue tracker](https://github.com/yourusername/KITra/issues).

## License

This package is licensed under the GPL
