
#' filter.webElement
#' @export

filter.webElement <- function(elements, attr, value){
  if(class(elements)[[1]] != "list"){elements <- list(elements)}
  values <- elements %>%
    get_attribute(attr)

  to_select <- which(values %in% value)

  if(length(to_select) != 0){
    out <- elements[to_select]
    if(length(out) == 1){out <- out[[1]]}
    return(out)
  } else {
    return(list())
  }

}

#' get_all_attribute
#' @export

get_all_attribute <- function(element){
  element %>%
    get_attribute("outerHTML") %>%
    stringr::str_extract("<.*?>") %>%
    stringr::str_extract_all('\\w+=\\".*?\\"') %>% .[[1]] %>%
    stringr::str_split("\\=", n = 2) %>%
    purrr::map_dfc(~{
      tibble::tibble(stringr::str_remove_all(.x[2], '"')) %>%
        purrr::set_names(.x[1])
    }) %>%
    dplyr::mutate(element = list(element))
}

#' clear
#' @export

clear <- function(element){
  element$clearElement()
}


#' highlight
#' @export
highlight <- function(element, wait = NULL){
  element$highlightElement(wait = wait)
}

#' set_attribute
#' @export

set_attribute <- function(elements, attr, value){
  if(class(elements)[[1]] != "list"){elements <- list(elements)}
  elements %>%
    purrr::map_chr(~{
      out <- .x$setElementAttribute(attributeName = attr, value = value)
      out <- ifelse(length(out) == 0, NA_character_, out[[1]])
      return(out)
    })
}

#' get_attribute
#' @export

get_attribute <- function(elements, attr){
  if(class(elements)[[1]] != "list"){elements <- list(elements)}
  elements %>%
    purrr::map_chr(~{
      out <- .x$getElementAttribute(attr)
      out <- ifelse(length(out) == 0, NA_character_, out[[1]])
      return(out)
    })
}

#' keys
#' @export

keys <- RSelenium::selKeys

#' chrome_init
#' @export

chrome_init <- function(view = T, name = "", ua = 1){

  name <- ifelse(name == "", "chrome", name)

  if(!name %in% dockeR::existing_containers()){
    dockeR::create_container("selenium/standalone-chrome-debug", name)
    bashR::wait(4, .5)
  }
  if(name %in% dockeR::stopped_containers()){
    dockeR::start_container(name)
    bashR::wait(4, .5)
  }
  if(name %in% dockeR::running_containers()){
    chrome <- dockeR::quiet(dockeR::get_driver(port = dockeR::get_port(name, 4444), ua = ua))
  }

  if(view == T){dockeR::view_container(name)}
  return(chrome)
}

#' get_driver
#' @export

get_driver <- function(port, ua = 1){
  eCaps <- list(
    chromeOptions =
      list(
        prefs = list(
          "profile.default_content_settings.popups" = 0L
          # "download.prompt_for_download" = F
          # #"download.default_directory" = "~/extract_temp"
        ),
        args = c('--disable-dev-shm-usage',
                 '--disable-gpu',
                 glue::glue('--user-agent="{user_agents[ua]}"'))# '--no-sandbox', '--headless') #  '--window-size=1200,1800' , ,
      )
  )

  driver <- RSelenium::remoteDriver(
    remoteServerAddr = "localhost",
    port = port,
    browserName = "chrome",
    extraCapabilities = eCaps
  )

  driver$open()

  return(driver)
}


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
click <- function(browser, value, using = "css selector", return = ""){
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
  tmp <- get_driver(remoteServerAddr = "selenium", port = as.integer(port), browserName = browser)
  if(prune) tmp$closeall()
  tmp$open()
  return(tmp)
}

#' get_real_source_code
#' @export
get_real_source_code <- function(browser, filepath = NULL){

  tmp <- browser$executeScript("return window.document.getElementsByTagName('html')[0].innerHTML")

  page <- tmp[[1]] %>%
    xml2::read_html(.)

  if(is.null(filepath)){
    return(page)
  } else {
    page %>% xml2::write_html(., file = filepath)
    message(glue::glue("Source code was saved under { filepath }"))
  }

}


#' wait_until
#' @export
wait_until <- function(chrome, n_wait = 4, value = "", using = "css selector", return = "", click = F){
  n <- n_wait*2
  while(n > 0){
    res <- silently(try(chrome %>% element(value = value, using = using), silent = T))
    n <- ifelse(class(res) == "try-error", n -.5, 0)
    Sys.sleep(.5)
  }
  if(click){chrome %>% click(value, using, return)}
}

#' wait_and_click
#' @export
wait_and_click <- function(chrome, n_wait = 4, value = "", using = "css selector", return = ""){
  wait_until(chrome, n_wait = n_wait, value = value, using = using, return = return, click = T)
}

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

#' get_class
#' @export
get_class <- function(elems){
  elems %>% purrr::map_chr(~.x$getElementAttribute("class")[[1]])
}

#' check_element
#' @export
check_element <- function(chrome, value, using = "css selector"){
  element <- silently(try(chrome$findElement(using, value), silent = T))
  if(class(element)[1] == "try-error"){
    return(F)
  } else {
    return(T)
  }
}

#' find_child
#' @export
find_child <- function(element, value = "", using = "css selector"){element$findChildElement(value = value, using = using)}
#' find_children
#' @export
find_children <- function(element, value = "", using = "css selector"){element$findChildElements(value = value, using = using)}
#' get_text
#' @export
get_text <- function(element){element$getElementText()[[1]]}
#' switch_to_frame
#' @export
switch_to_frame <- function(chrome, div_value = "", div_using = "css selector",
                            frame_value = "", frame_using = "name"){
  elem <- chrome %>%
    element(div_value, div_using) %>%
    find_child(frame_value, frame_using)

  chrome$switchToFrame(elem)
}

