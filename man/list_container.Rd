% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docker_functions.R
\name{list_container}
\alias{list_container}
\title{list_container}
\usage{
list_container(image_src = NULL, not_running = T)
}
\arguments{
\item{image_src}{A String providing a specific image to filter}

\item{not_running}{a logical vector specifying whether not_running containers should be returned}
}
\value{
a tibble listing containers with their information
}
\description{
This function lists all running containers. If image_src is specified, it only return containers, of which the building image matches the given string.
}
