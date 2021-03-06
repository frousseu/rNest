\name{chronology}
\alias{chronology}
\title{
Estimate Nesting Chronology from Nest Observations to a Single Nest}
\description{
Used to estimate the nesting chronology from observations to a single nest.}
\usage{
chronology(dat, details = FALSE, nparam = NULL, test = FALSE, minclutch = TRUE, rel.flex = c(0, 0), nact = TRUE, lsc = NULL)
}
\arguments{
  \item{dat}{
A \code{\link{data.frame}} containing observations for a single nest with column names : nest_id, species_code,
date, eggs, young and optionally status_code.
}
  \item{details}{
Modifies the output of the function to give either the range of dates for which the nest is active or the complete nesting sequence
with the nesting stages on different dates as names. When set to \code{TRUE}, gives the complete nesting sequence.
}
  \item{nparam}{
A \code{\link{data.frame}} containing nesting parameters for the species with column names : species_code, cmin,
cmax, clutch, laying, start, incub and rearing.
}
  \item{test}{
Modifies the output of the function to give a \code{data.frame} facilitating the comparison between the observations, the nesting parameters
and the estimation. When set to \code{TRUE}, the output is the test \code{data.frame}.
}
  \item{minclutch}{
If a single observation is made with a number of eggs inferior to the minimal clutch size, this argument determines if the nest will be
considered in the laying stage with the mean clutch size or if the nest will be considered in the incubation stage with the observed
number of eggs used as the clutch size. Whent set to \code{TRUE}, a single observation with a number of eggs below the minimum clutch size will be
considered in the laying stage.
}
  \item{rel.flex}{
Determines the amount of variation that will be attributed to the incubation period when the length of the observed incubation+rearing
period differ from the theoretical length. When set to \code{NULL}, the relative adjustement of the incubation and the rearing period will be
proportional to their theoretical lengths. Otherwise, a \code{vector} of two proportions determining the amount of variation that will be
attributed to the incubation period when the observed length of the incubation+rearing period is either longer or shorter than the
theoretical one, respectively. A value of \code{rel.flex = c(0, 0)} attributes all the variation to the rearing period and fixes the length
of incubation.
}
  \item{nact}{
Determines how observations of unknown number of eggs and young will be interpreted when estimating the nest chronology. When set to
\code{TRUE}, observations reporting values of (-1, -1) for the number of eggs and young will be considered to represent an active nest
containing either eggs, young or both and the complete chronology will be centered around these observations. When set
to \code{FALSE}, values of (-1,-1) will be interpreted as indicative of the presence of both eggs and young in the nest.
}
  \item{lsc}{
A named \code{list} with stages as names and corresponding status codes to be interpreted as such (ex: \code{list(laying=c("HR"), hatching=c("PI","CR"))}).
}
}
\details{
When no nesting parameters are available ( \code{nparam = NULL} ), nesting parameters from the database \code{\link{nestingparameters}} can be used.
In this case, the database has to be made available in the global environment by typing \code{data(nestingparameters)} in the console
(see function \code{\link{data}}). The \code{\link{rNest}} function will also make the nesting parameters available in the global environment.
}
\value{
Usually a \code{vector} (see argument \code{details})
}
\author{
Francois Rousseu <francoisrousseu@hotmail.com>
}
\note{
In general, the \code{\link{active.nest}} function should be used instead because it applies to several nests. The \code{\link{chronology}}
function is mostly there to simplify the code and is mostly intended to be used inside the \code{\link{active.nest}} function.
}
\seealso{
\code{\link{active.nest}}, \code{\link{rNest}}
}
\examples{
To be described}

