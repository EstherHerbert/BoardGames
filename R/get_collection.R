get_collection <- function(username) {

  collection <- bggAnalytics::bggCollection$new(username = username)

  collection$expand(c("own", "name", "minplayers", "maxplayers", "minplaytime",
                      "maxplaytime"))

  games <- bggAnalytics::bggGames$new(ids = collection$ids)
  games$expand(c("minage"))

  collection <- bggAnalytics::bgg_merge(collection, games)

  collection <- as.data.frame(collection)

  return(collection)

}
