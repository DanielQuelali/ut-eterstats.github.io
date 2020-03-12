# Internals ---------------------------------------------------------------

stats_mode <- function(x) {
  a <- unique(x)
  
  match(x, a) %>%
    tabulate %>%
    which.max %>%
    a[.]
}

format_elapsed_time <- function(time_in_secs) {
  paste0(
    as.numeric(time_in_secs) %/% 60, "m",
    trunc(as.numeric(time_in_secs)) %% 60, "s"
  )
}

get_players_dict <- function(df_players) {
  setNames(df_players$name, df_players$guid)
}

get_player_name <- function(player_guid, players_dict) {
  players_dict[player_guid] %>%
    as.vector()
}

add_killer_victim_names <- function(df, df_match_players) {
  df %>%
    inner_join(
      df_match_players %>%
        rename(killer_name = player_name),
      by = c('killer_guid' = 'player_guid')
    ) %>%
    inner_join(
      df_match_players %>%
        rename(victim_name = player_name),
      by = c('victim_guid' = 'player_guid')
    )
}

# Level 1 -----------------------------------------------------------------

## df_match_players currently fills two roles at the same time:
## 1- Get a list of all players that were active at this match
## 2- Get their player_name's
## TODO: Think about that :/
get_df_match_players <- function(df_match_events, players_dict) {
  df_match_events %>%
    filter(!is.na(player_guid)) %>%
    distinct(player_guid) %>%
    mutate(
      player_name = get_player_name(player_guid, players_dict)
    )
}

get_df_kill_events <- function(df_match_events) {
  df_match_events %>%
    filter(event_type == 'kill') %>%
    select(
      event_id,
      asctime,
      message,
      round,
      round_time,
      player_team,
      killer_guid = player_guid,
      victim_guid = event_properties.victim,
      death_cause = event_properties.death_cause,
      is_team_kill = event_properties.team_kill
      
      ## TODO:
      # flag_carrier_killed = TRUE/FALSE
    )
}

get_df_flag_events <- function(df_match_events) {
  ## TODO: Add Hot Potato
  ## Known bug: If the flag returns on its own, it won't get tracked :(
  
  df_match_events %>%
    filter(grepl('flag', event_type), event_type != 'flag_captured') %>%
    
    mutate(
      capture_time_secs = if(
        'event_properties.capture_time_ms' %in% names(.))
        as.numeric(event_properties.capture_time_ms) / 1000
        else 0
    ) %>%
    
    ## Flag team
    mutate(
      flag_team = case_when(
        event_type %in% c('flag_dropped', 'flag_capture_time') ~
          if_else(player_team == 'red', 'blue', 'red'),
        event_type == 'flag_returned' ~ player_team
      )
    ) %>%
    
    ## Flag ID
    group_by(round, flag_team) %>%
    mutate(
      flag_id = cumsum(event_type %in% c('flag_capture_time', 'flag_returned')) %>%
        lag() %>%
        coalesce(0L)
    ) %>%
    ungroup() %>%
    
    select(
      event_id,
      asctime,
      message,
      round,
      flag_team,
      flag_id,
      event_type,
      capture_time_secs,
      player_guid
    )
}

get_df_spawns <- function(df_match_events) {
  missing_time_value <- as.POSIXct(NA_real_, origin = '1970-01-01')
  
  df_match_events %>%
    # head(1000) %>%
    mutate(
      spawning_player_id = player_guid,
      player_spawn_start = if_else(
        event_type == 'player_spawn',
        asctime,
        missing_time_value
      )
    ) %>%
    mutate(
      spawning_player_id = if_else(
        !is.na(event_properties.death_cause),
        event_properties.victim,
        spawning_player_id
      ),
      player_spawn_end = if_else(
        grepl("Exit", message),
        asctime,
        missing_time_value
      )
    ) %>%
    fill(player_spawn_end, .direction = 'up') %>%
    # group_by(
    #   player_guid
    # ) %>%
    # ungroup() %>%
    mutate(
      player_spawn_end = if_else(
        !is.na(event_properties.death_cause) | grepl("Disconnect", message),
        asctime,
        player_spawn_end
      )
    ) %>%
    arrange(asctime, event_id) %>%
    group_by(spawning_player_id) %>%
    mutate(
      player_spawn_id = cumsum(coalesce(event_type, '') == 'player_spawn')
    ) %>%
    ungroup() %>%
    select(
      spawning_player_id,
      player_spawn_id,
      player_spawn_start,
      player_spawn_end,
      player_team
    ) %>%
    filter(
      player_team %in% c('red', 'blue')
    ) %>%
    # filter(!is.na(player_spawn_start), !is.na(player_spawn_end)) %>%
    # filter(spawning_player_id == '3B1D7F2EA942DAD5C2AC7F828A9B33B4') %>%
    # filter(spawning_player_id == 'C6BB082FCB688977AA66D20ABA6086B2') %>%
    group_by(
      spawning_player_id,
      player_spawn_id
    ) %>%
    summarize(
      # Puede devolver valores Inf.
      player_spawn_start = min(player_spawn_start, na.rm = TRUE),
      player_spawn_end = min(player_spawn_end, na.rm = TRUE),
      player_team = first(player_team)
    ) %>%
    ungroup() %>%
    arrange(spawning_player_id, player_spawn_id)
}

# Level 2 -----------------------------------------------------------------

get_df_killer_victim <- function(df_kill_events) {
  df_kill_events %>%
    
    ## Not counting "World" kills, nor suicides
    filter(killer_guid != 'NONE') %>%
    
    group_by(killer_guid, victim_guid) %>%
    summarize(
      kills = n()
    ) %>%
    ungroup()
}