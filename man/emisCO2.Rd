% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emisCO2.R
\name{emisCO2}
\alias{emisCO2}
\title{emisCO2}
\usage{
emisCO2(
  gdx,
  file = NULL,
  level = "cell",
  unit = "gas",
  sum_cpool = TRUE,
  sum_land = TRUE,
  cumulative = FALSE,
  baseyear = 1995,
  lowpass = 3
)
}
\arguments{
\item{gdx}{GDX file}

\item{file}{a file name the output should be written to using write.magpie}

\item{level}{Level of regional aggregation;
"reg" (regional),
"glo" (global),
"regglo" (regional and global) or
any other aggregation level defined in superAggregate}

\item{unit}{"element" or "gas";
"element": co2_c in Mt C/yr
"gas": co2_c Mt CO2/yr}

\item{sum_cpool}{aggregate carbon pools (TRUE), below ground (soilc) and
above ground (vegc and litc) will be reported, if FALSE}

\item{sum_land}{TRUE (default) or FALSE. Sum over land types (TRUE)
or report land-type specific emissions (FALSE).}

\item{cumulative}{Logical; Determines if emissions are reported annually
(FALSE) or cumulative (TRUE). The starting point for
cumulative emissions is y1995.}

\item{baseyear}{Baseyear used for cumulative emissions (default = 1995)}

\item{lowpass}{number of lowpass filter iterations (default = 3)}
}
\value{
CO2 emissions as MAgPIE object (unit depends on \code{unit})
}
\description{
reads detailed CO2 emissions out of a MAgPIE gdx file
}
\examples{
\dontrun{
x <- emisCO2(gdx)
}
}
\author{
Florian Humpenoeder, Michael Crawford
}
