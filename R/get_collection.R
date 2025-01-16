get_collection <- function(username) {

  collection <- bggAnalytics::bggCollection$new(username = username)

  collection$expand(c("own", "name", "minplayers", "maxplayers", "minplaytime",
                      "maxplaytime", "image"))

  games <- bggAnalytics::bggGames$new(ids = collection$ids, chunk_size = 20)
  games$expand(c("minage", "type", "category", "description"))

  collection <- bggAnalytics::bgg_merge(collection, games)

  collection <- as.data.frame(collection)

  return(collection)

}
