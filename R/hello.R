# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' ggpower: package for power transformations, useful e.g. in ggplot
#'
#' The foo package provides three categories of important functions:
#' foo, bar and baz.
#'
#' @section scale_color_sqrt:
#' You can add this directly to a ggplot2 command.
#' @section Useful functions:
#' power_trans, power_breaks and semi_scientific_formatting can be useful in
#' many scenarios.
#'
#' @docType package
#' @name ggpower
NULL




#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param power PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#' # without, with mild and with strong transformation:
#'  ggplot(data=tibble(x=rgamma(1000, c(.5, 5, 50))), aes(x))+geom_histogram(bins=50)
#'  ggplot(data=tibble(x=rgamma(1000, c(.5, 5, 50))), aes(x))+geom_histogram(bins=50) + scale_x_continuous(trans=power_trans(1/2))
#'  ggplot(data=tibble(x=rgamma(1000, c(.5, 5, 50))), aes(x))+geom_histogram(bins=50) + scale_x_continuous(trans=power_trans(1/10))
#'
#'  # last plot, but with nicer x-axis labels:
#'  ggplot(data=tibble(x=rgamma(1000, c(.5, 5, 50))), aes(x))+geom_histogram(bins=50) + scale_x_continuous(trans=power_trans(1/2), labels = semi_scientific_formatting)
#'  }
#' }
#' @seealso
#'  \code{\link[scales]{trans_new}}
#' @rdname power_trans
#' @export
#' @importFrom scales trans_new
power_trans <- function(power){
# define scale_color_sqrt (and functions it requires):
  # returns transformation object that can be used in ggplot's scale_*_continuous
  scales::trans_new(
    name = "tmp",
    trans = function(x)   x^(power),
    inverse = function(x) x^(1/power),
    breaks = function(lims, p) power_breaks(lims, p=power) )
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param lims PARAM_DESCRIPTION
#' @param power PARAM_DESCRIPTION
#' @param n_breaks PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname power_breaks
#' @export
power_breaks <- function(lims, power, n_breaks=5){
  # Return vector of breaks that span the lims range evenly _after_ power transformation:
  lims[1] <- base::max(0, lims[1]) # non-integer exponents are not defined for negative values
  x <- base::seq(lims[1]^power, lims[2]^(power), length.out = n_breaks)^(1/power)
  # make human-readable by rounding to the closest integer power of 2. Smallest
  # and largest ticks are not strictly rounded - instead they are moved within
  # the range of values, since ggplot would not display them otherwise:
  x <- dplyr::case_when(
    x == base::max(x) ~ 2^(base::floor(base::log2(x))),
    x == base::min(x) ~ 2^(base::ceiling(base::log2(x))),
    TRUE ~ (2^(base::round(base::log2(x))))
  )
  return(x)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#' # without, with mild and with strong transformation:
#'  ggplot(data=tibble(x=rgamma(1000, c(.5, 5, 50))), aes(x))+geom_histogram(bins=50)
#'  ggplot(data=tibble(x=rgamma(1000, c(.5, 5, 50))), aes(x))+geom_histogram(bins=50) + scale_x_continuous(trans=power_trans(1/2))
#'  ggplot(data=tibble(x=rgamma(1000, c(.5, 5, 50))), aes(x))+geom_histogram(bins=50) + scale_x_continuous(trans=power_trans(1/10))
#'
#' # last plot, but with nicer x-axis labels:
#'  ggplot(data=tibble(x=rgamma(1000, c(.5, 5, 50))), aes(x))+geom_histogram(bins=50) + scale_x_continuous(trans=power_trans(1/2), labels = semi_scientific_formatting)
#'  }
#' }
#' @seealso
#'  \code{\link[scales]{scientific_format}}
#' @rdname semi_scientific_formatting
#' @export
#' @importFrom scales scientific
semi_scientific_formatting <- function(x) {
  # takes numeric vector x and returns character vector where extremely large / small
  # numbers are in scientific notation (e.g. 1e-30) while others are untouched:
  x <- dplyr::case_when(
    x == 0 ~ as.character(0),
    base::abs(x) < .01 | base::abs(x) >= 1000 ~ scales::scientific(x,  digits = 0),
    TRUE ~ as.character(x))}
# if there are NAs in the vector by which you color, make sure to supply na.value.
# Otherwise it defaults to dark grey, which looks like the high values of cubeHelix (green/black)



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  ggplot(data=mtcars, aes(wt, mpg, col = disp)) + geom_point() + scale_color_sqrt()
#'  }
#' }
#' @seealso
#'  \code{\link[ggplot2]{scale_colour_gradient}}
#'  \code{\link[rje]{cubeHelix}}
#' @rdname scale_color_sqrt
#' @export
#' @importFrom ggplot2 scale_color_gradientn
#' @importFrom rje cubeHelix
scale_color_sqrt <- function(...){ggplot2::scale_color_gradientn(
  colours = rev(rje::cubeHelix(100))[5:100],
  trans = ggpower::power_trans(1/2),
  labels = ggpower::semi_scientific_formatting,
  ...)}
