#' hsexpectdeck
#'
#' This function expects what concept the deck is. Decks are divided into four categories: Highlander, Winnie, Aggro, and Control. \cr
#' (You must first run hsdeck function in hstone package to use this function)
#'
#' @return Expectation of what the type of deck is.
#' @examples
#' hsexpectdeck()
#' @import dplyr httr rvest jsonlite stringr
#' @export

hsexpectdeck <- function() {
  if (exists("Deck") == FALSE) {
    warning("You first have to run hsdeck function in hstone package.")
  } else {
    if (anyDuplicated(Deck$name) < 1) {
      print("Highlander")
    } else {
      if (mean(Deck$manaCost) < 2.5) {
        print("Winnie")
      } else {
        if (((mean(Deck$manaCost)) >= 2.5) && ((mean(Deck$manaCost)) <= 5)) {
          print("Aggro")
        } else {
          if (mean(Deck$manaCost) > 5) {
            print("Control")
          }
        }
      }
    }
  }
}
