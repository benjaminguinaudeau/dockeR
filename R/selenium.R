#' screenshot
#' @export

screenshot <- function(browser, display = T, useViewer = T, file = NULL){
  browser$screenshot(display, useViewer, file)
}

#' go
#' @export
go <- function(browser, ...){
  browser$navigate(...)
  return(browser)
}

#' screenshot
#' @export
screenshot <- function(browser, file = NULL, display = T, useViewer = T){
  browser$screenshot(file = file, display = display, useViewer = useViewer)
}

#' element
#' @export
element <- function(browser,  value, using = "css selector"){
  browser$findElement(using, value)
}

#' elements
#' @export
elements <- function(browser,  value, using = "css selector"){
  browser$findElements(using, value)
}

#' send_keys
#' @export
send_keys <- function(browser, ...){
  browser$sendKeysToElement(...)
  return(browser)
}

#' click
#' @export
click <- function(browser, value, using = "css selector", return = NULL){
  if("remoteDriver" %in% class(browser)){
    elem <- browser$findElement(using, value)
    elem$clickElement()
    if(return == "element") return(elem)
    if(return == "browser") return(browser)
  } else {
    browser$clickElement()
  }
  Sys.sleep(sample(1:500, 1)/1000)
}

#' new_window
#' @export
new_window <- function(port = 4444, prune = T, browser = "chrome"){
  tmp <- remoteDriver(remoteServerAddr = "selenium", port = as.integer(port), browserName = browser)
  if(prune) tmp$closeall()
  tmp$open()
  return(tmp)
}

#' get_real_source_code
#' @export
get_real_source_code <- function(browser, filepath = NULL){

  tmp <- browser$executeScript("return window.document.getElementsByTagName('html')[0].innerHTML")

  tmp[[1]] %>%
    xml2::read_html() %>%
    xml2::write_html(nnn, file = filepath)

  message(glue("Source code was saved under { filepath }"))
}




