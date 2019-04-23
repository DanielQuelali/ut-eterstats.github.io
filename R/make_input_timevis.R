# Internals ---------------------------------------------------------------

get_df_tv_players_order <- function(m_killer_victim){
  player_order <- seriation::seriate(m_killer_victim) %>% seriation::get_order()
  
  tibble(
    player = rownames(m_killer_victim)[player_order]
  ) %>%
    mutate(
      ## In alphanumeric ordering, 2 is greater than 10.
      ## This is an ugly hack to preserve the correct order,
      ## but supports at most 26 players
      order = letters[row_number()]
    )
}

# Intermediate results ----------------------------------------------------

get_df_tv_livetime <- function(df_spawns) {
  df_spawns %>%
    
    group_by(spawning_player_id) %>%
    filter(player_spawn_end <= lead(player_spawn_start)) %>%
    ungroup() %>%
    
    filter(complete.cases(.)) %>%
    
    mutate(
      type = 'background',
      content = '&nbsp',
      # style = paste0(
      #   "background-color: ",
      #   if_else(player_team == 'red', 'lightpink', NA_character_),
      #   ";"
      # )
      style = if_else(
        player_team == 'red',
        'background-color: lightpink;',
        ''
      )
    ) %>%
    
    select(
      group = spawning_player_id,
      start = player_spawn_start,
      end = player_spawn_end,
      type,
      content,
      style
    )
}

get_df_tv_flags <- function(df_flag_events) {
  df_flag_events %>%
    filter(event_type == 'flag_capture_time') %>%
    mutate(
      # id = paste0(group, 'flag_', id),
      content = '<i class="fa fa-flag"></i>',
      type = 'range',
      start = asctime - capture_time_secs
    ) %>%
    select(
      id = event_id,
      group = player_guid,
      type,
      content,
      title = message,
      start,
      end = asctime
    )
}

get_df_tv_kills <- function(df_kill_events) {
  df_kill_events %>%
    
    ## Do not include "World" kills
    filter(killer_guid != 'NONE') %>%
    
    select(
      id = event_id,
      group = killer_guid,
      title = message,
      start = asctime,
      death_cause
    ) %>%
    mutate(
      # id = paste0('event_', id),
      style = 'z-index: 1',
      content = if_else(
        grepl("KNIFE", death_cause),
        '<i class="fa fa-fighter-jet" aria-hidden="true"></i>', #emo::ji("drop")
        '&nbsp'
      ),
      type = 'point'
    )
}

get_df_tv_players <- function(m_killer_victim, df_match_players) {
  df_match_players %>%
    rename(
      id = player_guid,
      content = player_name
    ) %>%
    left_join(
      get_df_tv_players_order(m_killer_victim),
      by = c("content" = "player")
    )
}

## Deprecated!
get_df_tv_rounds <- function(df_raw_events) {
  df_raw_events %>%
    group_by(round) %>%
    summarize(
      start = min(asctime),
      end = max(asctime)
    ) %>%
    ungroup() %>%
    mutate(
      id = -round,
      content = paste0("Round ", round),
      type = 'background',
      style = 'background-color: lightpink;'
    ) %>%
    select(-round)
}

# Exported ----------------------------------------------------------------

get_df_tv_main <- function(df_spawns, df_flag_events, df_kill_events) {
  bind_rows(
    get_df_tv_livetime(df_spawns),
    get_df_tv_flags(df_flag_events),
    get_df_tv_kills(df_kill_events)
  )
}

get_df_tv_groups <- function(m_killer_victim, df_match_players) {
  bind_rows(
    tibble(id = "_", content = "Round", order = '0'),
    get_df_tv_players(m_killer_victim, df_match_players)
  )
}