% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wizz.R
\name{egor_vis_app}
\alias{egor_vis_app}
\title{\code{egor} Network Visualization App}
\usage{
egor_vis_app(object = NULL, shiny_opts = list(launch.browser = TRUE))
}
\arguments{
\item{object}{An \code{egor} object.}

\item{shiny_opts}{\code{List} of arguments to be passed to \code{shinyApp()}'s options argument.}
}
\description{
Launches an interactive Shiny Web App, that creates a list of
\code{igraph} objects from an 'egor' object and offers the user several graphical
means of interacting with the visualization parameters for all networks in
the \code{egor} object.
}
\examples{
if(interactive()){
  data("egor32")
  egor_vis_app(egor32)
}
}
\keyword{analysis}
\keyword{ego-centered}
\keyword{network}
