get_player_spawn_id <- function(player_id, asctime, df_spawns) {
  map2_int(
    player_id,
    asctime,
    ~df_spawns$player_spawn_id[
      df_spawns$spawning_player_id == .x &
        df_spawns$player_spawn_start <= .y
      ] %>%
      #TODO: Revisar esto, antes era max y se rompio
      last()
  )
}

get_df_max_streak <- function(df_kill_events, df_spawns, df_match_players) {
  df_max_streak <- df_kill_events %>%
    mutate(
      killer_spawn_id = get_player_spawn_id(killer_guid, asctime, df_spawns)
    ) %>%
    count(killer_guid, killer_spawn_id) %>%
    group_by(killer_guid) %>%
    summarize(
      max_streak = max(n)
    ) %>%
    ungroup()
  
  df_match_players %>%
    left_join(
      df_max_streak,
      by = c('player_guid' = 'killer_guid')
    ) %>%
    select(-player_name) %>%
    arrange(-max_streak)
}

get_df_multikill <- function(df_kill_events, df_match_players) {
  df_multikill <- df_kill_events %>%
    filter(!is_team_kill) %>%
    group_by(killer_guid) %>%
    mutate(
      diff_time = (asctime - lag(asctime)) %>% as.numeric(unit = 'secs'),
      not_monster_kill = diff_time > 5,
      mk_id = not_monster_kill %>% coalesce(FALSE) %>% cumsum()
    ) %>%
    # ungroup() %>%
    count(mk_id) %>%
    count(n) %>%
    ungroup() %>%
    filter(n >= 3) %>%
    mutate(
      mk = if_else(n == 3, 'MultiKill', 'MonsterKill')
    ) %>%
    select(-n) %>%
    group_by(killer_guid, mk) %>%
    summarize_at("nn", sum) %>%
    ungroup()
  
  df_match_players %>%
    left_join(
      df_multikill,
      by = c('player_guid' = 'killer_guid')
    ) %>%
    select(-player_name) %>%
    complete(player_guid, mk = c('MultiKill', 'MonsterKill'), fill = list(nn = 0)) %>%
    spread(mk, nn)
}

get_df_weapons_used <- function(df_kill_events, df_match_players) {
  df_weapons_used <- df_kill_events %>%
    filter(!is_team_kill) %>%
    filter(!is.na(killer_guid)) %>%
    count(killer_guid, death_cause) %>%
    group_by(death_cause) %>%
    mutate(
      sum_cause = sum(n),
      killers_cause = n_distinct(killer_guid)
    ) %>%
    ungroup() %>%
    arrange(-sum_cause, -killers_cause, death_cause) %>%
    mutate(
      death_cause = death_cause %>% str_replace('UT_MOD_', '') %>% factor() %>% fct_inorder()
    ) %>%
    select(killer_guid, death_cause, n) %>%
    spread(death_cause, n, fill = 0)
  
  df_match_players %>%
    left_join(
      df_weapons_used,
      by = c('player_guid' = 'killer_guid')
    ) %>%
    select(-player_name)
}

get_df_kill_details <- function(df_kill_events, df_spawns, df_match_players) {
  df_match_players %>%
    left_join(
      get_df_max_streak(df_kill_events, df_spawns, df_match_players),
      by = 'player_guid'
    ) %>%
    left_join(
      get_df_multikill(df_kill_events, df_match_players),
      by = 'player_guid'
    ) %>%
    left_join(
      get_df_weapons_used(df_kill_events, df_match_players),
      by = 'player_guid'
    ) %>%
    select(-player_guid) %>%
    replace_na(
      replace = list(
        max_streak = 0
      )
    ) %>%
    arrange(-max_streak, -MonsterKill, -MultiKill)
}