% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDataDS.R
\name{getDataDS}
\alias{getDataDS}
\title{Retrieves the encrypted data from a server to the analysis computer}
\usage{
getDataDS(master_mode = TRUE)
}
\arguments{
\item{master_mode}{Boolean argument. It indicates the mode of a server is a \strong{master} or a \strong{receiver}. By default, set to TRUE.}
}
\value{
a list made of a header, a payload, and four properties[a-d]
}
\description{
This server function retrieves some encrypted data to be passed onto the analysis computer
}
\details{
Some encrypted data are transformed into a suitable format to be transferred from a DataSHIELD server to a DataSHIELD client. The property.a indicates
the size of the data at encoding time. Property b is the number of columns and property c a timestamp. property d a random number.
}
\seealso{
\link[dsServerParameter]{assignDataDS}
}
