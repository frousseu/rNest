\name{rNest}
\alias{rNid}
\title{
Calling rNest's User Interface}
\description{
These functions call the user tcltk interface as well as the nestwatch data, nesting parameters and bird grouping characteristics}
\usage{
rNest()
rNid()
}
\details{
The function \code{rNest} is used to call the english interface and \code{rNid} is used to call the french interface.

Information on how to use the interface is partly contained in the different menus and buttons.

When using the \code{rNest} function, the NestWatch data (\code{\link{nestwatch}}) is called first, then nesting parameters (\code{\link{nestingparameters}}) and
bird groupings (\code{\link{birdgroupings}}) and finally the user interface appears with its different menus. Both functions use the \code{rNest_tcltk} function internal to the package.
}
\value{
Opens the tcltk interface of rNest}
\author{
Francois Rousseu <francoisrousseu@hotmail.com>
}
\note{
Currently, a bug in tcltk causes R and the user interface to crash when regions are selected interactively on a plot
and the plot is prematurely closed before ending the selection. To prevent the crash, always end any selection by using the right
button of the mouse and choosing the stop option displayed. Similarly, interrupting a long calculation is currently causing the
same type of problem.}
\seealso{
To be described}

