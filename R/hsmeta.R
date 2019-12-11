#' hsmeta
#'
#' This function shows the data frame with codes for card's metadata.
#' For example, the codes specific for classifications such as classes, types, minions, and rarities.
#'
#' @param id A client Id you obtained from Blizzard api.
#' @param secret A client Secret you obtained from Blizzard api.
#' @param give Entering \code{"classes"}, \code{"types"}, \code{"minions"}, or \code{"rarities"} will give you the codes assigned for each classifications.
#' @return A data frame with codes for that specific \code{give} you entered.
#' @examples
#' hsmeta(id = Sys.getenv("id"), secret = Sys.getenv("secret"), give = "classes")
#' hsmeta(id = Sys.getenv("id"), secret = Sys.getenv("secret"), give = "minions")
#' @import dplyr httr rvest jsonlite stringr
#' @export


hsmeta <- function(id, secret, give) {
  client_id = id
  client_secret = secret

  token_resp <- POST(
    url = "https://us.battle.net/oauth/token",
    config = authenticate(client_id, client_secret),
    body = list(grant_type = "client_credentials")
  )

  token <- content(token_resp)$access_token

  ep_metadata <- "https://us.api.blizzard.com/hearthstone/metadata"

  bearer_token <- str_c("Bearer", token, sep = " ")

  query_params <- list(region = "US", locale = "en_US")

  hsg <- GET(url = ep_metadata,
             config = add_headers(authorization = bearer_token),
             query = query_params)

  hsmd <- fromJSON(content(hsg, as = "text"))
  cla <- as.data.frame(hsmd$classes)
  typ <- as.data.frame(hsmd$types)
  minio <- as.data.frame(hsmd$minionTypes)
  rar <- as.data.frame(hsmd$rarities)

  if (give == "classes") {
    assign("Classes", cla, envir = globalenv())
  } else {
    if (give == "types") {
      assign("Types", typ, envir = globalenv())
    } else {
      if (give == "minions") {
        assign("Minions", minio, envir = globalenv())
      } else {
        if (give == "rarities") {
          assign("Rarities", rar, envir = globalenv())
        }
      }
    }
  }
}
