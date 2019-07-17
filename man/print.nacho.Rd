% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.nacho}
\alias{print.nacho}
\title{print.nacho}
\usage{
\method{print}{nacho}(x, colour = "CartridgeID", size = 0.5,
  show_legend = FALSE, ...)
}
\arguments{
\item{x}{[\link{list}] List obtained from \link{summarise} or \link{normalise}.}

\item{colour}{[\link{character}] Character string of the column in \code{ssheet_csv}
or more generally in \code{nacho_object$nacho} to be used as grouping colour.}

\item{size}{[\link{numeric}] A \code{numeric} controlling point size (\link{geom_point} or \link{geom_beeswarm})
or line size (\link{geom_line}).}

\item{show_legend}{[\link{logical}] Boolean to indicate whether the plot legends should
be plotted (\code{TRUE}) or not (\code{FALSE}). Default is \code{TRUE}.}

\item{...}{not used.}
}
\description{
This function allows to print text and figures from the results of a call to \code{summarise()}
or \code{normalise()}.
It is intended to be used in a \code{Rmarkdown} chunk.
}
\examples{

data(GSE74821)
print(GSE74821, colour = "CartridgeID", size = 0.5, show_legend = TRUE)

}