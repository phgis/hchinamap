#' Mark points on Chinese province maps
#'
#' @description provincepoint function provides a simple way to mark points on Chinese province map.
#' @param cat category vector;
#' @param name name vector;
#' @param lat latitude vector;
#' @param lon longitude vector;
#' @param region A pinyi character, "anhui", "aomen", "beijing", "chongqing",
#'      "fujian", "gansu", "guangdong", "guangxi", "guizhou", "hainan", "hebei",
#'      "heilongjiang", "henan", "hubei", "hunan", "jiangsu", "jiangxi", "jilin",
#'      "liaoning", "neimenggu", "ningxia", "qinghai", "shandong", "shanghai",
#'      "shanxi", "shanxi2", "sichuan", "tianjin", "xianggang", "xinjiang",
#'      "xizang", "yunnan", "zhejiang".
#' @param theme Chart theme, you can choose one from:
#' darkgreen/darkblue/avocado/darkunica/gray/
#' gridlight/grid/sandsignika/sunset;
#' @param title Chart title;
#' @param titleAlign The horizontal position of the title, such as "center";
#' @param titleSize The size of the title, such as "20px";
#' @param titleColor The color of the title, such as "#3333";
#' @param subtitle Subtitle of chart;
#' @param subtitleAlign The horizontal position of subtitles, such as "center";
#' @param subtitleSize The size of the subtitle, such as "16px";
#' @param subtitleColor The color of the subtitle, such as "#666666";
#' @param itermName Data attributes in tooltip;
#' @param headerFormat Header format, default value is '<small>{point.key}</small><table>';
#' @param pointFormat Point format, default value is "<tr><td>Catagory: </td><td>: {series.name}
#'    </td></tr><tr><td>Location: </td><td>({point.lon:.2f}, {point.lat:.2f})</td></tr>";
#' @param footerFormat Footer format, default value is '</table>';
#' @param hoverColor The color of the area when the mouse is hovering.
#' @param legendLayout Legend, horizontal or vertical;
#' @param legendAlign Horizontal position of legend, center/left/right;
#' @param legendTitle The title of the legend;
#' @param legendVerticalAlign The vertical position of legends, top/center/bottom;
#' @param markerRadius Marker Radius;
#' @param width Chart width;
#' @param height Chart height;
#' @param mapNavigation show map navigation or not, 1 means show, 0 means not;
#' @param mapNavigationVerticalAlign Map navigation vertical align, top/bottom;
#' @param mapNavigationAlign map navigation align, center/left/right;
#' @param elementId NULL.
#'
#' @import htmlwidgets
#' @examples
#' library(hchinamap)
#' library(dplyr)
#' library(magrittr)
#' dir <- tempdir()
#' download.file('https://czxb.github.io/br/chinamappointdf.rda', file.path(dir, 'chinamappointdf.rda'))
#' load(file.path(dir, 'chinamappointdf.rda'), verbose = TRUE)
#' df <- chinamappointdf %>% dplyr::filter(province == "Shandong")
#' if(interactive()) {
#'   provincepoint(cat = df$cat, name = df$name,
#'       lat = df$lat, lon = df$lon, region = "shandong",
#'       title = "Urban distribution in Shandong Province",
#'       subtitle = "Data source: People's daily")
#' }
#'
#' @export
provincepoint <- function(
  cat,
  name,
  lat,
  lon,
  region = "anhui",
  theme = "sandsignika",
  title = "Example Map",
  titleAlign = "center",
  titleSize = "20px",
  titleColor = "#333333",
  subtitle = "",
  subtitleAlign = 'center',
  subtitleSize = "",
  subtitleColor = "#666666",
  itermName = "Random data",
  headerFormat = "<small>{point.key}</small><table>",
  pointFormat = "<tr><td>Catagory: </td><td>: {series.name}</td></tr><tr><td>Location: </td><td>({point.lon:.2f}, {point.lat:.2f})</td></tr>",
  footerFormat = '</table>',
  hoverColor = '#a4edba',
  legendLayout = "horizontal",
  legendAlign = "center",
  legendTitle = "",
  legendVerticalAlign = "bottom",
  markerRadius = 3,
  mapNavigation = 1,
  mapNavigationVerticalAlign = "bottom",
  mapNavigationAlign = "left",
  width = NULL, height = NULL, elementId = NULL) {

  # forward options using x
  x = list(
    cat = cat,
    name = name,
    lat = lat,
    lon = lon,
    region = region,
    title = title,
    titleAlign = titleAlign,
    titleSize = titleSize,
    titleColor = titleColor,
    subtitle = subtitle,
    subtitleAlign = subtitleAlign,
    subtitleSize = subtitleSize,
    subtitleColor = subtitleColor,
    headerFormat = headerFormat,
    pointFormat = pointFormat,
    footerFormat = footerFormat,
    theme = theme,
    hoverColor = hoverColor,
    mapNavigation = mapNavigation,
    mapNavigationVerticalAlign = mapNavigationVerticalAlign,
    mapNavigationAlign = mapNavigationAlign,
    itermName = itermName,
    legendLayout = legendLayout,
    legendAlign = legendAlign,
    legendTitle = legendTitle,
    legendVerticalAlign = legendVerticalAlign,
    markerRadius = markerRadius
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'provincepoint',
    x,
    width = width,
    height = height,
    package = 'hchinamap',
    elementId = elementId
  )
}

#' Shiny bindings for provincepoint
#'
#' Output and render functions for using provincepoint within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a provincepoint
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name provincepoint-shiny
#'
#' @export
provincepointOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'provincepoint', width, height, package = 'hchinamap')
}

#' @rdname provincepoint-shiny
#' @export
renderProvincepoint <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, provincepointOutput, env, quoted = TRUE)
}
