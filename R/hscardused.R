#' hscardused
#'
#' This function returns how many times the cards are used in each decks that are retrived from the topdeckcodes function in hstone package. \cr
#' (You must first run hstopdeckcodes function in hstone package to use this function)
#'
#' @param id A client Id you obtained from Blizzard api.
#' @param secret A client Secret you obtained from Blizzard api.
#' @param used Put either "high", "medium", or "low" in order to retrieve cards with high usage (4 and more times used), medium usage (2 to 3), and low usage (1 or less). \cr
#'             Leave this blank to retrieve all the cards without such criteria.
#' @return The data frame with cards and how many times they are used in top tier decks.
#' @examples
#' hscardused(id = Sys.getenv("id"), secret = Sys.getenv("secret"), used = "high")
#' hscardused(id = Sys.getenv("id"), secret = Sys.getenv("secret"))
#' @import dplyr httr rvest jsonlite stringr
#' @export

hscardused <- function(id, secret, used = "") {
  if (exists("TopDeckCodes") == FALSE) {
    warning("You first have to run topdeckcodes function in hstone package.")
  } else {
    df <- list()
    for (i in TopDeckCodes$deck_code) {
      alpha <- hsdeck(id = id, secret = secret, deckcode = i)
      df[[i]] <- alpha
    }
    df2 <- do.call("rbind", df) %>% as.data.frame()
    df3 <- table(df2$name) %>% as.data.frame() %>% arrange(desc(Freq))
    assign("CardsUsed", df3, envir = globalenv())
    if (used == "high") {
      high <- filter(CardsUsed, Freq > 3)
      View(high)
      assign("High", high, envir = globalenv())
    } else {
      if (used == "medium") {
        medium <- filter(CardsUsed, Freq <= 3 & Freq >= 2)
        View(medium)
        assign("Medium", medium, envir = globalenv())
      } else {
        if (used == "low") {
          low <- filter(CardsUsed, Freq < 2)
          View(low)
          assign("Low", low, envir = globalenv())
        }
      }
    }
  }
}
