library(testthat)
library(hstone)

test_check("hstone")

hscard(id = Sys.getenv("id"), secret = Sys.getenv("secret"), class = "mage", manaCost = 5, attack = 5, orderBy = "health")
hsdeck(id = Sys.getenv("id"), secret = Sys.getenv("secret"), deckcode = "AAECAaoIAqH4AualAw7TAeAGkwn6qgL28AKP+wKc/wKMhQP2igO0lwPGmQP0mQPUpQPYqQMA")
