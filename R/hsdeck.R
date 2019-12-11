#' hsdeck
#'
#' This function shows all the cards that are in the deck you search.
#'
#' @param id A client Id you obtained from Blizzard api.
#' @param secret A client Secret you obtained from Blizzard api.
#' @param deckcode Enter the Hearthstone deckcode.
#' @return All the cards that are in the deck.
#' @examples
#' hsdeck(id = Sys.getenv("id"), secret = Sys.getenv("secret"), deckcode = "AAECAaoIAqH4AualAw7TAeAGkwn6qgL28AKP+wKc/wKMhQP2igO0lwPGmQP0mQPUpQPYqQMA")
#' @import dplyr httr rvest jsonlite stringr
#' @export


hsdeck <- function(id, secret, deckcode) {

  client_id = id
  client_secret = secret

  token_resp <- POST(
    url = "https://us.battle.net/oauth/token",
    config = authenticate(client_id, client_secret),
    body = list(grant_type = "client_credentials")
  )

  token <- content(token_resp)$access_token

  ep_deck <- "https://us.api.blizzard.com/hearthstone/deck/"
  deckcode = deckcode
  ep_deckc <- str_c("https://us.api.blizzard.com/hearthstone/deck/", deckcode)

  bearer_token <- str_c("Bearer", token, sep = " ")

  query_params_deck <- list(region = "US", locale = "en_US")

  hsgdeck <- GET(url = ep_deckc,
                 config = add_headers(authorization = bearer_token),
                 query = query_params_deck)

  hsgcdeck <- fromJSON(content(hsgdeck, as = "text"))
  hsgddeck <- as.data.frame(hsgcdeck$cards)

  if ((http_status(hsgdeck))$category == "Success") {
    hsgddeck <- hsgddeck %>% select(id, classId, cardTypeId, rarityId, artistName, health, attack, manaCost, name, text, image, flavorText, minionTypeId)
    assign("Deck", hsgddeck, envir = globalenv())
    print(Deck)
  } else {
    warning("The request produced an error.")
  }
}
