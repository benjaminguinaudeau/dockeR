#' click
#' @export

click <- function(x, y = "css selector"){
  remDr %>%
    findElement(y, x) %>%
    elementClick()

  Sys.sleep(sample(1:500, 1)/1000)
}

#' screenshot
#' @export

screenshot <- function(browser, display = T, useViewer = T, file = NULL){
  browser$screenshot(display, useViewer, file)
}

#' @export
go <- function(brow, ...){
  brow$navigate(...)
}

#' @export
screenshot <- function(browser, file = NULL, display = T, useViewer = T){
  browser$screenshot(file = file, display = display, useViewer = useViewer)
}

#' @export
element <- function(browser,  value, using = "css selector"){
  browser$findElement(using, value)
}

#' @export
send_keys <- function(browser, ...){
  browser$sendKeysToElement(...)
  #return(browser)
}







