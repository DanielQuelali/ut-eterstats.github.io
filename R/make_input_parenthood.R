get_df_parenthood <- function(df_kill_events, df_match_players) {
  df_killer_victim <- df_kill_events %>%
    filter(is_team_kill == FALSE) %>%
    get_df_killer_victim()
    
  df_killer_victim %>%
    full_join(
      df_killer_victim,
      by = c(
        'killer_guid' = 'victim_guid',
        'victim_guid' = 'killer_guid'
      )
    ) %>%
    mutate(
      father_killed_son = coalesce(kills.x, 0L),
      son_killed_father = coalesce(kills.y, 0L),
      balance = father_killed_son - son_killed_father
    ) %>%
    filter(balance >= 5) %>%
    arrange(-balance, -father_killed_son) %>%
    
    add_killer_victim_names(df_match_players) %>%
    
    select(
      father = killer_name,
      son = victim_name,
      father_killed_son,
      son_killed_father,
      balance
    )
}