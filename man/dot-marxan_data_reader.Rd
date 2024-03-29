% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/marxan.R
\name{.marxan_data_reader}
\alias{.marxan_data_reader}
\title{Marxan data reader}
\usage{
.marxan_data_reader(input, datafile)
}
\arguments{
\item{input}{string - The address of a Marxan input.dat file.}

\item{datafile}{string - The file to read. either 'spec' or 'puvspr'.}
}
\value{
A data.frame
}
\description{
Reads a marxan spec.dat or puvsp.dat file.
}
\examples{
\dontrun{
.marxan_data_reader('/data/marxan/analysis01/input.dat', 'spec')
.marxan_data_reader('/data/marxan/analysis01/input.dat', 'puvspr')
.marxan_data_reader('/data/marxan/analysis01/input.dat', 'pu')
}
}
\author{
Daniele Baisero, \email{daniele.baisero@gmail.com}
}
\keyword{internal}
