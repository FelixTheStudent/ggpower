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
#' You can add scale_color_sqrt directly to a ggplot2 command.
#' @section Useful functions:
#' power_trans, power_breaks and semi_scientific_formatting can be useful in
#' many scenarios.
#'
#' @docType package
#' @name ggpower
NULL




#' @title Log-spaced sequence
#' @description Generates logarithmically spaced sequence covering the
#' entire range of \code{x} (plus/minus ten percent) and avoiding
#' numbers smaller or equal to zero.
#' @param x numeric vector
#' @param length PARAM_DESCRIPTION, Default: 100
#' @param abs_min PARAM_DESCRIPTION, Default: 1e-05
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname logseq
#' @export
logseq <- function(x, length = 100, abs_min=1e-5){
  # exponentially spaces sequence covering the range of x (+/- ten percent)
  pmax(abs_min, exp(seq(log(.9*min(x)), log(1.1*max(x)), length.out = length)))
}


#' @title Histogram with logged x-axis
#' @description Useful e.g. for log-normal distributions, etc.. Wraps
#' ggplot+geom_histogram with all the things I usually get wrong (see details)
#' Type `lhist` to see code for copy-pasting.
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details
#' Currently, this visualization is the one I find the most useful:
#' * x-axis is log-transformed (coord_trans, !not! scale_x - otherwise, adding
#' theoretical distributions does not work)
#' * log-spaced bins, otherwise coord_trans throws error and/or bin widths look
#' extremely uneven
#' * y-axis is sqrt-transformed, otherwise you don't see higher values so well
#'   (play around with the example below and you'll see)
#' * add theoretical distributions with
#' \code{geom_line(aes(logseq(x), dlnorm(x, meanlog, sdlog)))}
#' @md
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  x <- rlnorm(100000, meanlog = c(-6,.4), sdlog = 1)
#'  xseq <- pmax(1e-5, exp(seq(log(.9*min(x)), log(1.1*max(x)), length.out = 100)))
#'
#'  ggplot()+
#'    geom_histogram(data=data.frame(x=x), aes(x, stat(density)), breaks=xseq) +
#'    geom_line(aes(xseq, .5*dlnorm(xseq, -6, 1)), color="blue") +
#'    geom_line(aes(xseq, .5*dlnorm(xseq, .4, 1)), color="red") +
#'    coord_trans(x="log", y="sqrt")

#'  }
#' }
#' @rdname lhist
#' @export
lhist <- function(x) {
  p <- ggplot()+
  geom_histogram(data=data.frame(x=x), aes(x, stat(density)), breaks=logseq(x)) +
  coord_trans(x="log", y="sqrt") +
  scale_x_continuous(breaks = function(lims) power_breaks(lims, 1/2),
                     labels = semi_scientific_formatting)
  return(p)
}


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
# Notes on coord_trans:
#   power_trans is designed to work with scale_x_continuous(trans=power_trans(1/2)).
#   You can do something like coord_trans(x=power_trans(1/5)) as well, but this
#   has two currently unsolved problems:
#   1. coord_trans does not use the breaks function, so you get linear breaks
#   2. for histograms this will certainly throw an error, because the first bin
#      (on the very left) sits at 0, so the x-value of the bin's left border is
#      a negative number, which throws an error.
#      You could fix this with pmax(x, 0) in the trans/inverse functions above.


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
power_breaks <- function(lims, power, n_breaks=100){
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
  return(unique(x))
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
#'  \code{\link[ggpower]{power_breaks}}
#'  \code{\link[ggpower]{semi_scientific_formatting}}
#' @rdname scale_color_sqrt
#' @export
#' @importFrom ggplot2 scale_color_gradientn
#' @importFrom rje cubeHelix
scale_color_sqrt <- function(n_breaks=5, ...){
  scale_color_gradientn(
    colours = rev(rje::cubeHelix(100))[5:100],
    trans = scales::trans_new(
      name = "tmp",
      trans = function(x)   x^(.5),
      inverse = function(x) x^(1/.5),
      breaks = function(lims) power_breaks(lims, p=.5, n_breaks=n_breaks) ),
    labels = ggpower::semi_scientific_formatting,
    ...)
}




#' @title Power transformations for ggplot2's axis.
#' @description FUNCTION_DESCRIPTION
#' @param power A value with \code{1/n}, where \code{n} must be an integer number. Default: 1/2
#' @param ... Other parameters passed to \code{\link[ggplot2]{scale_x_continuous}}.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  p <- tibble(a=rgamma(1000, shape = c(.1,3, 16), rate=1)) %>% ggplot(aes(a))+
#'  geom_histogram(bins=50)
#'  p + scale_x_sqrt()      #  no ticks below 10
#'  p + scale_x_power(1/2)  #  reasonable ticks (powers of two with human-readable rounding)
#'  p + scale_x_power(1/4)  #  other powers than .5 are possible
#'  }
#' }
#' @seealso
#'  \code{\link[ggplot2]{scale_continuous}}
#'  \code{\link[ggpower]{power_trans}},\code{\link[ggpower]{semi_scientific_formatting}}
#' @export
#' @describeIn scale_x_power same for y-axis.
#' @importFrom ggplot2 scale_x_continuous
scale_x_power <- function(power=1/2, ...){ggplot2::scale_x_continuous(
  trans = ggpower::power_trans(power),
  labels = ggpower::semi_scientific_formatting,
  ...)}


#' @describeIn scale_x_power same for y-axis.
scale_y_power <- function(power=1/2, ...){ggplot2::scale_y_continuous(
  trans = ggpower::power_trans(power),
  labels = ggpower::semi_scientific_formatting,
  ...)}

