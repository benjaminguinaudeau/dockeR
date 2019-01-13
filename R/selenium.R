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







selenium <- remoteDriver(remoteServerAddr = "selenium", port = 4444L, browserName = "chrome")
selenium$open()

selenium %>% go("https://www.google.ca")

selenium %>% screenshot()
selenium %>% screenshot("test.png")

selenium %>%
  element("q", "name") %>%
  send_keys(list("Paris"))


elem <- selenium %>%
  element("input:nth-child(1)")

elem$click()

selenium %>%
  click("btnK", "name")

click <- function(browser, value, using = "css selector"){
  elem <- browser$findElement(using, value)

  elem$clickElement

  Sys.sleep(sample(1:500, 1)/1000)
}


