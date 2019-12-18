#' quiet
#' @export

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
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
  } else {
    return(container)
  }

}
