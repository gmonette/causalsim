% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coefx.R
\name{lines.coefx}
\alias{lines.coefx}
\alias{lines_}
\title{Plotting the added-variable plot for linear DAG}
\usage{
\method{lines}{coefx}(x, new = TRUE, xvertical = 1, col = "black", lwd = 2, ...)

lines_(x, new = FALSE, xvertical = 1, col = "black", lwd = 2, ...)
}
\arguments{
\item{x}{an object of class 'coefx' produced by
\code{\link{coefx}}.}

\item{new}{if FALSE add to previous plot. Default: TRUE.}

\item{xvertical}{position of vertical line to help visualize
standard error factor. Default: 1.}

\item{col}{color of ellipse and lines. Default: 'black'.}

\item{lwd}{line width.}

\item{\dots}{other arguments passed to \code{\link{plot}}.}
}
\description{
See \code{\link{coefx}} for an extended example.
}
