\name{nesting.activity}
\alias{nesting.activity}
\title{
Calculate the Proportion of Active Nests for a Set of Dates
}
\description{
Calculate the proportion of active nests along the breeding season for a set of species, regions or years
}
\usage{
nesting.activity(dat, dates = F, groups = F, by = "sp", n = T, chronology = F, ...)
}
\arguments{
  \item{dat}{
A \code{\link{data.frame}} containing observations for one or several nests or the output of the \code{\link{active.nest}} function with
column names : nest_id, species_code, date, eggs, young and optionally status_code, longitude and latitude.
}
  \item{dates}{
Restricts the range of dates for which the nesting activity is calculated. A \code{vector} of length 2 with the dates given as
a character string of the in the mm-dd format (ex: \code{dates = c("04-01","09-01")}).
}
  \item{groups}{
Used to compute the nesting phenology for different group of species based on habitat, phylogeny, feeding guild, etc.
A named \code{list} of character vectors with the species code and the group ids as names (ex: \code{list(warblers = c("COYE", "YWAR"),
hawks = c("BAEA" , "GOEA"))}). Species that are not part of a group will be returned as usual. If a species is part of more
than one group, an error message will be returned.
}
  \item{by}{
Used to compute the nesting phenology based on species, years or regions. The three types of output are \code{"sp"}, \code{"year"} or \code{"region"}.
}
  \item{n}{
Gives the number of nests used to compute the proportion of active nests. If set to \code{TRUE}, the first line of the resulting
\code{data.frame} gives the number of nests used.
}
  \item{chronology}{
Does the nesting chronology needs to be determined first? When set to \code{TRUE}, the \code{dat} argument has to be a \code{data.frame} with
nests observations. When set to \code{FALSE}, the \code{dat} argument is assumed to be the output of the \code{\link{active.nest}} function.
}
  \item{\dots}{
Further arguments to be passed to the \code{\link{chronology}} or \code{\link{active.nest}} function.
}
}
\details{
To be described
}
\value{
A \code{\link{data.frame}} with the first column named date and the remaining columns named with either the species code, year or regions used
according to the \code{by} argument. The first line of the \code{data.frame} optionaly (see argument \code{n}) contains the number of nests
available for the calculation.
}
\references{
To be described
}
\author{
Francois Rousseu <francoisrousseu@hotmail.com>
}
\note{
To be described
}
\seealso{
\code{\link{active.nest}}, \code{\link{nesting.calendar}}
}

