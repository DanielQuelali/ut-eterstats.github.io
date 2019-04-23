get_df_players_from_db <- function(sqlite_db_filepath = "./data/data.sqlite") {
  conn_ut4 <- dbConnect(drv = RSQLite::SQLite(), dbname = sqlite_db_filepath)
  df_players <- dbGetQuery(conn_ut4, statement = "SELECT * FROM player")
  dbDisconnect(conn_ut4)
  
  df_players
}

get_raw_events <- function(events_log_filepath = "./data/devel.log") {
  stream_in(file(events_log_filepath))
}