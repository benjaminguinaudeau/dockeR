devtools::install_github("benjaminguinaudeau/dockeR")
pacman::p_load(tidyverse, RSelenium, dockeR)

selenium <- remoteDriver(remoteServerAddr = "selenium", port = 4444L, browserName = "chrome")
selenium$open()

selenium %>% go("https://www.google.ca")

selenium %>% screenshot()
selenium %>% screenshot("test.png")

selenium %>%
  element("q", "name") %>%
  send_keys(list("Paris"))
