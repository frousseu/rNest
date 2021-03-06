\name{nesting.calendar}
\alias{nesting.calendar}
\title{
Building Custom Nesting Calendars}
\description{
This function is used to obtain nesting calendars for specific regions, years, species and/or group of species}
\usage{
nesting.calendar(d, sp = F, cons = 10, colcons = "grey75", dates = F, groups = F, type = "Species", color.scale = T,
prop = c(0, 0.01, 0.02, 0.1, 0.2, 0.5, 0.7, 1), color = c(rev(heat.colors(length(prop) - 3, alpha = 1)), "brown", "darkred"))
}
\arguments{
  \item{d}{
A \code{\link{data.frame}} of proportions of active nests on different dates or the output of the \code{\link{nesting.activity}} function.
The first column is named date with dates in the "mm-dd" format. Other columns are usually species, years or regions, but any name to describe
the set of nests used is accepted. The first line contains the number of nests used to compute the proportions. The first element of the column date
is the character expression \code{"nb_nest"}.
}
  \item{sp}{
Used to restrict the calendar to a set of species (or other groups). A character \code{vector} with column names found in the \code{data.frame}.
Ignored when set to \code{FALSE}.
}
  \item{cons}{
The number of nest under which only the presence/absence of nesting activity will be depicted without a more precise description of the nesting phenology.
}
  \item{colcons}{
The color to be used when the nesting activity is only presence/absence. A color specified in anyway accepted by the graphical parameter \code{"col"}
(see \code{\link{par}}). Defaults to \code{"grey"}.
}
  \item{dates}{
Used to restrict the calendar to a range of dates. A character vector of length 2 with the range of dates desired in
the "mm-dd" format (ex: \code{c("04-01","09-01")}).
}
  \item{groups}{
Used to label group of species in the calendar. A named \code{list} containing as element the groups of species and the group as names
(ex: \code{list(group1=c("sp1","sp2"), group2=c("sp3" , "sp4"))}).
}
  \item{type}{
Used to label the calendar. Any character string (ex. \code{"Years", "Regions"}, etc.).
}
  \item{color.scale}{
Displays the color scale at the bottom of the calendar. When set to \code{FALSE}, the color scale is not displayed.}
  \item{prop}{
Breaks used to display the different proportions. Any sequence of numbers in the [0,1] interval beginning with 0 and ending with 1.}
  \item{color}{
Colors used for the different proportions. A vector of colors specified in anyway accepted by the graphical parameter \code{"col"} (see \code{\link{par}}).
The number of colors specified must be equal to the number of blocks given to the \code{prop} argument.
}
}
\details{
To be described}
\value{
A \code{\link{plot}} on the graphic device.}
\author{
Francois Rousseu <francoisrousseu@hotmail.com>
}
\note{
To be described}
\seealso{
\code{\link{nesting.activity}}}
