#' @title 'hchinamap': Mapping China and Its Provinces, Municipalities and Autonomous Regions using R and 'Highmaps'
#' @description By binding R functions and the 'Highmaps' <https://www.highcharts.com.cn/products/highmaps> chart library,'hchinamap' package provides a simple way to map China and its provinces. The map of China drawn by this package contains complete Chinese territory, especially the Nine-dotted line, South Tibet, Hong Kong, Macao and Taiwan.
#' @note Because the map data of Taiwan have not been collated yet, it is impossible to draw provincial map of Taiwan Province for the time being.
#' @param name Chinese name vector of provinces or prefecture-level cities in China.
#' @param value Value vector;
#' @param region Region name in English, Such as "China", "Anhui" ...;
#' @param theme Chart theme, you can choose one from:
#' darkgreen/darkblue/avocado/darkunica/gray/
#' gridlight/grid/sandsignika/sunset;
#' @param mapNavigation show map navigation or not, 1 means show, 0 means not;
#' @param mapNavigationVerticalAlign Map navigation vertical align, top/bottom;
#' @param mapNavigationAlign map navigation align, center/left/right;
#' @param width Chart width;
#' @param height Chart height;
#' @param itermName Data attributes in tooltip;
#' @param title Chart title;
#' @param titleAlign The horizontal position of the title, such as "center";
#' @param titleSize The size of the title, such as "20px";
#' @param titleColor The color of the title, such as "#3333";
#' @param subtitle Subtitle of chart;
#' @param subtitleAlign The horizontal position of subtitles, such as "center";
#' @param subtitleSize The size of the subtitle, such as "16px";
#' @param subtitleColor The color of the subtitle, such as "#666666";
#' @param min The minimum value of legend, 0 by default.
#' @param minColor The color corresponding to the minimum of the legend, such as "white";
#' @param maxColor The color corresponding to the maximum value of the legend, such as "#006cee";
#' @param legendLayout Legend, horizontal or vertical;
#' @param legendAlign Horizontal position of legend, center/left/right;
#' @param legendTitle The title of the legend;
#' @param legendVerticalAlign The vertical position of legends, top/center/bottom;
#' @param hoverColor The color of the area when the mouse is hovering.
#' @param elementId NULL
#' @import htmlwidgets
#'
#' @examples
#' library(hchinamap)
#' library(dplyr)
#' library(magrittr)
#' dir <- tempdir()
#' download.file('https://czxb.github.io/br/chinadf.rda', file.path(dir, 'chinadf.rda'))
#' load(file.path(dir, 'chinadf.rda'), verbose = TRUE)
#' china <- chinadf %>%
#'   dplyr::filter(region == "China")
#' if(interactive()) {
#'    hchinamap(name = china$name, value = china$value, region = "China")
#' }
#'
#' @export
hchinamap <- function(name, value,
                       region = "China",
                       width = NULL, height = NULL,
                       elementId = NULL,
                       itermName = "Random data",
                       title = "",
                       titleAlign = "center",
                       titleSize = "20px",
                       titleColor = "#333333",
                       subtitle = "",
                       subtitleAlign = 'center',
                       subtitleSize = "",
                       subtitleColor = "#666666",
                       min = 0,
                       minColor = "rgb(255,255,255)",
                       maxColor = "#006cee",
                       legendLayout = "horizontal",
                       legendAlign = "center",
                       legendTitle = "",
                       legendVerticalAlign = "bottom",
                       hoverColor = '#a4edba',
                       mapNavigation = 1,
                       mapNavigationVerticalAlign = "bottom",
                       mapNavigationAlign = "left",
                       theme = "sunset") {

  # forward options using x
  x = list(
    name = name,
    value = value,
    itermName = itermName,
    title = title,
    titleAlign = titleAlign,
    titleSize = titleSize,
    titleColor = titleColor,
    subtitle = subtitle,
    subtitleAlign = subtitleAlign,
    subtitleSize = subtitleSize,
    subtitleColor = subtitleColor,
    min = min,
    minColor = minColor,
    maxColor = maxColor,
    legendLayout = legendLayout,
    legendAlign = legendAlign,
    legendTitle = legendTitle,
    legendVerticalAlign = legendVerticalAlign,
    region = region,
    hoverColor = hoverColor,
    mapNavigation = mapNavigation,
    mapNavigationVerticalAlign = mapNavigationVerticalAlign,
    mapNavigationAlign = mapNavigationAlign,
    theme = theme
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'hchinamap',
    x,
    width = width,
    height = height,
    package = 'hchinamap',
    elementId = elementId
  )
}

#' Shiny bindings for hchinamap
#'
#' Output and render functions for using hchinamap within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a hchinamap
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name hchinamap-shiny
#'
#' @export
hchinamapOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'hchinamap', width, height, package = 'hchinamap')
}

#' @rdname hchinamap-shiny
#' @export
renderHchinamap <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, hchinamapOutput, env, quoted = TRUE)
}
