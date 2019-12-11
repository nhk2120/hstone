#' hscard
#'
#' This function shows the data frame with all the cards with your choice of classifications. For various search terms for each classification, \cr
#' you can use hsmeta function in hstone package.
#'
#' @param id A client Id you obtained from Blizzard api.
#' @param secret A client Secret you obtained from Blizzard api.
#' @param class Class classification you want to obtain. You must enter the slug of the class that can be obtained using hsmeta function.
#' @param manaCost Mana Cost value classification you want to obtain.
#' @param attack Attack value classification you want to obtain.
#' @param health Health value classification you want to obtain.
#' @param rarity Rarity classification you want to obtain. You must enter the slug of the class that can be obtained using hsmeta function.
#' @param type Type classification you want to obtain. You must enter the slug of the class that can be obtained using hsmeta function.
#' @param minionType Minion type classification you want to obtain. You must enter the slug of the class that can be obtained using hsmeta function.
#' @param textFilter Text classfication you want to obtain.
#' @param orderBy Order by either health, attack, or mana cost of the cards you searched.
#' @return A data frame with cards based on your selected choice for each classifications.
#' @examples
#' hscard(id = Sys.getenv("id"), secret = Sys.getenv("secret"), class = "mage", manaCost = 5, attack = 5, orderBy = "health")
#' hscard(id = Sys.getenv("id"), secret = Sys.getenv("secret"), rarity = "legendary", type = "spell", orderBy = "manaCost")
#' hscard(id = Sys.getenv("id"), secret = Sys.getenv("secret"), minionType = "murloc", textFilter = "Battlecry")
#' @import dplyr httr rvest jsonlite stringr
#' @export

hscard <- function(id, secret, class = "", manaCost = "", attack = "",
                   health = "", rarity = "", type = "", minionType = "",
                   textFilter = "", orderBy = "") {

  client_id = id
  client_secret = secret

  token_resp <- POST(
    url = "https://us.battle.net/oauth/token",
    config = authenticate(client_id, client_secret),
    body = list(grant_type = "client_credentials")
  )

  token <- content(token_resp)$access_token

  ep_card <- "https://us.api.blizzard.com/hearthstone/cards"

  bearer_token <- str_c("Bearer", token, sep = " ")

  query_params <- list(region = "US", locale = "en_US", class = class, manaCost = manaCost, attack = attack, health = health,
                       rarity = rarity, type = type, minionType = minionType, textFilter = textFilter, pageSize = 1000)

  hsg <- GET(url = ep_card,
             config = add_headers(authorization = bearer_token),
             query = query_params)

  hsgc <- fromJSON(content(hsg, as = "text"))
  hsgd <- as.data.frame(hsgc$cards)

  if (orderBy == "manaCost") {
    hsgd <- hsgd %>% arrange(manaCost)
  }
  if (orderBy == "attack") {
    hsgd <- hsgd %>% arrange(attack)
  }
  if (orderBy == "health") {
    hsgd <- hsgd %>% arrange(health)
  }

  if ((http_status(hsg))$category == "Success") {
    hsgd <- within(hsgd, suppressWarnings(rm(id, collectible, slug, multiClassIds, cardSetId, cropImage, childIds, keywordIds, durability, imageGold)))
    assign("Card", hsgd, envir = globalenv())
  } else {
    warning("The request produced an error.")
  }
}
