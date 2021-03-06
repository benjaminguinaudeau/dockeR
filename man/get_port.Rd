% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docker_functions.R
\name{get_port}
\alias{get_port}
\title{get_port}
\usage{
get_port(container_name, filter_port = NULL)
}
\arguments{
\item{container_name}{The name of the container}

\item{filter_port}{If one value, it returs the corresponding port on the main computer. If several values, the function returns a tibble matching ports of the main computer with ports from the container.}
}
\value{
A tibble or a single value depending on filter_port
}
\description{
This function return the port exposed on a specific container and their allocated port on the main computer.
}
