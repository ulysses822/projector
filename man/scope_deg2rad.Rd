% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{scope_deg2rad}
\alias{scope_deg2rad}
\title{Convert from degrees to radians in scope}
\usage{
scope_deg2rad(x, envir = rlang::caller_env())
}
\arguments{
\item{x}{(list) Named vector of angles in degrees. Names are will be used to
assign objects.}

\item{envir}{Environment to assign objects into.}
}
\value{
Nothing, called for side effect.
}
\description{
Converts a named vector of angles from degrees to radians.
}
\examples{
\dontrun{
}
}
