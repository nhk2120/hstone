#' hsavgmana
#'
#' This function shows the average mana for either cards you searched or the deck you searched. \cr
#' (You must first run either hscard or hsdeck function in hstone package to use this function)
#'
#' @param type Enter either "Deck" or "Card". When you enter "Deck", it will give the average mana cost for the deck you searched
#'             by using hsdeck function in hstone package.\cr When you enter "Card", it will give the average mana cost for the cards
#'             you searched by using hscard function in hstone package.
#' @return The average mana cost.
#' @examples
#' hsavgmana(type = "Deck")
#' hsavgmana(type = "Card")
#' @import dplyr httr rvest jsonlite stringr tidyverse
#' @export


hsavgmana <- function(type) {
  if (type == "Deck") {
    if (exists("Deck") == FALSE) {
      warning("You first have to run hsdeck function in hstone package.")
    } else {
      Deck <- Deck %>% drop_na(manaCost)
      print(mean(Deck$manaCost))
    }
  } else {
    if (type == "Card") {
      if (exists("Card") == FALSE) {
        warning("You first have to run hscard function in hstone package.")
      } else {
        Card <- Card %>% drop_na(manaCost)
        print(mean(Card$manaCost))
      }
    } else {
      x <- 'You have to put either "Deck" or "Card" in type argument.'
      warning(cat(x))
    }
  }
}
