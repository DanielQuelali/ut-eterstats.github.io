## TODO: This looks like it should belong to make_base.

get_m_killer_victim <- function(df_kill_events, df_match_players) {
  df_kill_events %>%
    
    get_df_killer_victim() %>%
    add_killer_victim_names(df_match_players) %>%
    
    select(
      killer_name,
      victim_name,
      kills
    ) %>%
    spread(victim_name, kills) %>%
    {
      my_row_names <- pull(., killer_name)
      m <- as.matrix(select(., -killer_name))

      rownames(m) <- my_row_names

      m[is.na(m)] <- 0

      m
    }
}