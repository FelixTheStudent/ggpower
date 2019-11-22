# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


# define scale_color_sqrt (and functions it requires):
power_trans <- function(power){
  # returns transformation object that can be used in ggplot's scale_*_continuous
  scales::trans_new(
    name = "tmp",
    trans = function(x)   x^(power),
    inverse = function(x) x^(1/power),
    breaks = function(lims, p) power_breaks(lims, p=power) )
}


power_breaks <- function(lims, power, n_breaks=5){
  # Return vector of breaks that span the lims range evenly _after_ power transformation:
  lims[1] <- max(0, lims[1]) # non-integer exponents are not defined for negative values
  x <- seq(lims[1]^power, lims[2]^(power), length.out = n_breaks)^(1/power)
  # make human-readable by rounding to the closest integer power of 2. Smallest
  # and largest ticks are not strictly rounded - instead they are moved within
  # the range of values, since ggplot would not display them otherwise:
  x <- case_when(
    x == max(x) ~ 2^(floor(log2(x))),
    x == min(x) ~ 2^(ceiling(log2(x))),
    TRUE ~ (2^(round(log2(x))))
  )
  return(x)
}
semi_scientific_formatting <- function(x) {
  # takes numeric vector x and returns character vector where extremely large / small
  # numbers are in scientific notation (e.g. 1e-30) while others are untouched:
  x <- case_when(
    x == 0 ~ as.character(0),
    abs(x) < .01 | abs(x) >= 1000 ~ scales::scientific(x,  digits = 0),
    TRUE ~ as.character(x))}
# if there are NAs in the vector by which you color, make sure to supply na.value.
# Otherwise it defaults to dark grey, which looks like the high values of cubeHelix (green/black)
scale_color_sqrt <- function(...){scale_color_gradientn(
  colours = rev(rje::cubeHelix(100))[5:100],
  trans = power_trans(1/2),
  labels = semi_scientific_formatting,
  ...)}
