# Internals ---------------------------------------------------------------

flatten_json_to_df <- function(df) {
  df %>%
    jsonlite::flatten() %>%
    as_tibble() %>%
    mutate_at("asctime", ~stringr::str_replace(string = .x, ",", ".")) %>%
    mutate_at("asctime", ~as.POSIXct(., format = "%Y-%m-%d %H:%M:%OS")) %>%
    mutate(
      event_id = row_number()
    )
}

add_rounds <- function(df) {
  df %>%
    mutate(
      round = cumsum(grepl("Init", message))
    ) %>%
    group_by(round) %>%
    mutate(
      round_time = asctime - first(asctime)
    ) %>%
    ungroup()
}

add_mapname <- function(df) {
  df %>%
    fill(event_properties.map_name, event_properties.g_gametype) %>%
    arrange(
      event_id
    )
}

add_player_data <- function(df, players_dict) {
  df %>%
    mutate(
      player_id = str_extract(message, "(?<=Player )\\d+"),
      player_guid = str_extract(message, "(?<=guid )\\w+")
    ) %>%
    group_by(
      player_id
    ) %>%
    fill(player_guid) %>%
    ungroup() %>%
    arrange(
      event_id
    ) %>%
    mutate(
      event_type = if_else(grepl("ClientBegin", message), "player_entered", event_type),
      event_properties.player = coalesce(event_properties.player, player_guid)
    ) %>%
    
    # DEBUG:
    # select(event_id, asctime, message, event_type, event_properties.player, round, round_time) %>%

    group_by(player_guid) %>%
    mutate(
      player_team = if_else(
        event_type %in% 'player_disconnected',
        'no_team',
        str_extract(message, "(?<=team )\\w+")
      )
      # ,
      # player_name = str_extract(message, "(?<=Player \\d\\d? )\\w+")
    ) %>%
    fill(player_team) %>%
    ungroup() %>%
    arrange(event_id) %>%
    group_by(player_guid) %>%
    mutate(
      player_session_id = cumsum(
        coalesce(player_team, 'no_team') !=
          coalesce(lag(player_team), 'no_team')
      )
    ) %>%
    ungroup()
  # %>%
  #   mutate(
  #     player_name = coalesce(map_chr(player_guid, ~players_dict[.x]), player_name)
  #   )
}

get_df_raw_data <- function(raw_data) {
  raw_data %>%
    flatten_json_to_df() %>%
    add_rounds() %>%
    add_mapname() %>%
    add_player_data()
}

# Export ------------------------------------------------------------------

get_df_match_events <- function(df_raw_data) {
  df_raw_data %>%
    
    # Traer sólo las rondas que:
    group_by(round) %>%
    filter(
      # - hayan durado más de 5 minutos.
      max(round_time) > 5 * 60,
      
      # - hayan arrancado antes de las 14hs
      lubridate::hour(min(asctime)) < 14,
      
      # - sean CTF
      any(grepl("flag", event_type))
    ) %>%
    ungroup() %>%
    
    # De esas rondas, traemos las dos últimas
    filter(round >= nth(unique(round), -2)) %>%
    
    # Renombramos las rondas
    mutate(
      round = dense_rank(round)
    )
}
