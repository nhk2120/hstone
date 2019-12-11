#' hstopdeckcodes
#'
#' This function shows the top tier decks in the current season as well as the deck codes for each decks.
#'
#' The top tier decks and their codes are retrived from (http://metastats.net/). When the new season starts, the data may not be available
#' from few hours to a day or two.\cr Please, run this function again after a while when there are no decks yet available.
#'
#' @return Data frame with top tier decks and their codes.
#' @examples
#' hstopdeckcodes()
#' @import dplyr httr rvest jsonlite stringr xml2
#' @export


hstopdeckcodes <- function() {
  url_all <- "http://metastats.net/decksbyrank/"
  all <- read_html(url_all)
  question_nodes <- html_nodes(all, ".col-lg-10")
  fin <- html_nodes(question_nodes, "button") %>% html_attr("data-clipboard-text")
  if (length(fin) == 0) {
    warning("The new season started and top tier decks are not yet available.")
  } else {
    dn <- str_match(fin, "###(.*?) #")
    dn <- as.data.frame(dn[ , 2])
    dc <- str_split(fin, "\r\n")
    dc <- as.data.frame(sapply(dc, "[", 2))
    deckcodes <- as.data.frame(cbind(dn, dc))
    deckcodes <- deckcodes %>% rename("deck_name" = 'dn[, 2]', "deck_code" = 'sapply(dc, "[", 2)')
    assign("TopDeckCodes", deckcodes, envir = globalenv())
  }
}
