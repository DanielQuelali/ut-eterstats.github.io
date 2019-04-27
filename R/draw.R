# Internals ---------------------------------------------------------------

left_color_bar <- function (color = "lightgray", fun = "proportion", ...) 
{
  fun <- match.fun(fun)
  formatter(
    "span",
    style = function(x) style(
      display = "inline-block", 
      `border-radius` = "4px",
      `padding-left` = "4px", #`margin-right` = "20px", `padding-left` = "0px", #
      # padding = "0 0px",
      `background-color` = if_else(x < 0, csscolor("lightpink"), csscolor(color)) 
      ,
      width = percent(
        fun(as.numeric(x), ...)
      )
    )
  )
}

draw_datatable <- partial(
  DT::datatable,
  class = 'row-border hover',
  options = list(
    paging = FALSE,
    columnDefs = list(
      list(
        className = 'dt-left dt-head-left',
        targets = "_all"
      )
    )
  )
)

draw_formattable <- function(formated_df, ...) {
  formated_df %>%
    formattable:::render_html_matrix.formattable() %>%
    
    draw_datatable(
      # rownames = FALSE,
      escape = FALSE,
      ...
    )
}

# Export ------------------------------------------------------------------

draw_timevis <- function(df_tv_main, df_tv_groups) {
  timevis(
    df_tv_main,
    groups = df_tv_groups,
    options = list(stack = FALSE)
  )
}

draw_match_stats <- function(df_match_stats) {
  style_left_border <- "border-left: 1px solid #d7d7d7;"
  
  ## TODO: Make a Header builder
  complex_header <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, ' '),
        th(rowspan = 2, 'Player'),
        th(rowspan = 2, 'Playtime', style = style_left_border),
        th(colspan = 4, 'KDA', style = style_left_border),
        th(colspan = 3, 'Flags', style = style_left_border),
        th(colspan = 2, 'Team Flags', style = style_left_border),
        th(rowspan = 2, 'Match Result', style = style_left_border)
      ),
      tr(
        th('Kills', style = style_left_border),
        th('Deaths'),
        th('KD Ratio'),
        th('Kills per Minute'),
        th('Captured', style = style_left_border),
        th('Returned'),
        th('Dropped'),
        th('Won', style = style_left_border),
        th('Lost')
      )
    )
  ))
  
  df_match_stats %>%
    
    formattable(list(
      kills = left_color_bar("lightblue"),
      deaths = left_color_bar("lightblue"),
      kd_ratio = color_tile("transparent", "lightpink"),
      kills_per_minute = color_tile("transparent", "lightpink"),
      result = formatter(
        "span",
        style = x ~ style(
          color = case_when(
            x == 'WIN' ~ "DarkGreen",
            x == 'DRAW' ~ "DarkBlue",
            TRUE ~ "Crimson"
          )
        )
      )
    )) %>%
    
    draw_formattable(
      container = complex_header
    )
}

draw_parenthood_stats <- function(df_parenthood) {
  draw_datatable(df_parenthood)
}

draw_kills_heatmap <- function(m_killer_victim) {
  m_killer_victim %>%
    heatmaply(
      xlab = 'victims', ylab = 'killers',
      k_col = 2, k_row = 2,
      colors = viridis::inferno,
      seriate = "GW"
      # plot_method = "plotly",
      # row_dend_left = TRUE,
      # margins = c(120, 0, 0, NA)
    )
}

draw_knifeNkicks_stats <- function(df_knife_kills) {
  df_knife_kills %>%
    draw_datatable(
      rownames = FALSE
    )
}