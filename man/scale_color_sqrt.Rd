% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hello.R
\name{scale_color_sqrt}
\alias{scale_color_sqrt}
\alias{scale_fill_sqrt}
\title{FUNCTION_TITLE}
\usage{
scale_color_sqrt(...)

scale_fill_sqrt(...)
}
\arguments{
\item{...}{PARAM_DESCRIPTION}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
FUNCTION_DESCRIPTION
}
\details{
DETAILS
}
\examples{
\dontrun{
if(interactive()){
 ggplot(data=mtcars, aes(wt, mpg, col = disp)) + geom_point() + scale_color_sqrt()
 }
}
}
\seealso{
\code{\link[ggplot2]{scale_colour_gradient}}
 \code{\link[rje]{cubeHelix}}
 \code{\link[ggpower]{power_breaks}}
 \code{\link[ggpower]{semi_scientific_formatting}}
}
