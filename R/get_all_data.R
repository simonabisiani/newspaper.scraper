library(rvest)
library(purrr)
library(tidyverse)

# IPSO #########################################################################

suffices <- c("0,1,2,3,4,5,6,7,8,9", letters)

# generate urls
urls <-
  paste0("https://www.ipso.co.uk/complain/who-ipso-regulates/?letters=",
         suffices)

# define the scraping function
scraper <- function(urls) {
  read_html(urls) %>%
    html_elements(".pagination-listings") %>%
    html_elements("li") %>%
    html_text() %>%
    as.data.frame() %>%
    mutate(
      publication = gsub(" \\(.*", "", .),
      publisher = gsub(".*\\(", "", .),
      publisher = gsub("\\)", "", publisher)
    ) %>%
    select(-.)
}

# iterate over the urls
ipso_members <- map_dfr(urls, scraper)

write.csv(ipso_members,
          file = paste0("data_raw/ipso_members_",
                        make.names(Sys.date()), ".csv"))


# HFTP #########################################################################

root_url <- "https://www.holdthefrontpage.co.uk/directory/"
suffices <- c("newspaperwebsites/",
              "dailynewspapers/",
              "weeklynewspapers/",
              "mediacompanies/")

# generate urls
urls <- paste0(root_url,
               suffices)

pager <- function(page) {
  doc <- read_html(url(page))
  data.frame(
    Publication = doc %>% html_elements(".pf-content") %>% 
      html_elements("li") %>% 
      html_text(),
    Website = doc %>% html_elements(".pf-content") %>% 
      html_elements("li") %>% html_element("a") %>% 
      html_attr("href"),
    Type = doc %>% html_elements(".entry-title") %>% 
      html_text()
  )
}

htfp <- do.call(rbind, lapply(urls, pager))


# HYPERLOCALS

hyperlocals <-
  "https://www.holdthefrontpage.co.uk/directory/hyperlocal-publications/"

hl <-
  data.frame(
    Publication <-
      read_html(hyperlocals) %>% html_elements("td:nth-child(1)") %>% 
      html_text(),
    Website <-
      read_html(hyperlocals) %>% html_elements("td:nth-child(2)") %>% 
      html_element("a") %>% html_attr("href"),
    Type <- "Hyperlocal publications"
  )

names(hl) <- c("Publication", "Website", "Type")
hl <- hl[-c(1:2), ]

htfp_database <- rbind(htfp, hl)

write.csv(htfp_database,
          paste0("data_raw/htfp_directory_", make.names(Sys.Date()), ".csv"))


# IMPRESS #########################################################################

# generate urls
url <- "https://www.impress.press/regulated-publications/"

# define the scraping function
scraper_impress <- function(url) {
  read_html(url) %>%
    html_elements(".span-md-8") %>%
    html_elements("a") %>%
    html_text()
}


# iterate over the urls
impress_members <- as.data.frame(scraper_impress(url)) %>%
  rename("publisher" = "scraper_impress(url)")

write.csv(impress_members,
          paste0("data_raw/impress_members_", make.names(Sys.Date()), ".csv"))

