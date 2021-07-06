#' quiet
#' @export

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

#' system
#' @export
system <- function(..., cmd = F){
  if(cmd) return(cmd) else return(base::system(...))
}

#' silently
#' @export
silently <- function(x){
  suppressMessages(suppressWarnings(x))
}

#' clean_url
#' @export
clean_url <- function(x){stringr::str_replace_all(x, " ", "\\\\ ")}

#' check_container_name
#' @export

check_container_name <- function(container){

  if(class(container) == "R6"){
    return(chrome$container_name)
  } else if(class(container) == "remoteDriver"){
    name <- list_container() %>%
      dplyr::filter(stringr::str_detect(ports, as.character(container$port))) %>%
      dplyr::pull(names)
    return(name)
  } else {
    return(container)
  }

}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
