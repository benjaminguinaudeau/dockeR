#' click
#' @export

click <- function(x, y = "css selector"){
  remDr %>%
    findElement(y, x) %>%
    elementClick()

  Sys.sleep(sample(1:500, 1)/1000)
}

#' screen
#' @export

screen <- function(x) takeScreenshot(x)
