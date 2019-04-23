get_df_knife_kills <- function(df_kill_events, df_match_players) {
  df_kill_events %>%
    filter(
      death_cause %in% c(
        'UT_MOD_KNIFE',
        'UT_MOD_KNIFE_THROWN',
        'UT_MOD_KICKED'  
      )
    ) %>%
    add_killer_victim_names(df_match_players) %>%
    
    mutate(round_time = format_elapsed_time(round_time)) %>%
    
    select(
      round,
      round_time,
      killer = killer_name,
      victim = victim_name,
      death_cause
    )
}