
# import data  ----
data_raw <- read_xlsx(
  paste0(data_folder, "NM_data.xlsx"),
  sheet = "EoSM",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names()

## prepare data ----
data_prep_maturity <- data_raw %>% 
  filter(year_report == .env$year_report) %>% 
  select(entity_name, 
         year, 
         actual_policy_and_objectives,
         actual_risk_management,
         actual_assurance,
         actual_promotion,
         actual_culture) %>% 
  pivot_longer(-c(entity_name, year), names_to = "type", values_to = "score") %>% 
  mutate(type = str_replace_all(type, "actual_", ""),
         type = str_replace_all(type, "_", " "),
         type = str_to_sentence(type),
         score = case_when(
           year > year_report ~ NA,
           .default = score
         )
  ) %>% 
  mutate(
    score_text = case_when (
      score == 20 ~ 'A',
      score == 40 ~ 'B',
      score == 60 ~ 'C',
      score == 80 ~ 'D',
      .default = as.character(score)
    )
  )

main_safety_ansp <- country   

# plot chart ----
myc <-  function(mywidth, myheight, myfont, mymargin) {
    plot_ly(
      data = data_prep_maturity,
      width = mywidth,
      height = myheight,
      x = ~ year,
      y = ~ score,
      yaxis = "y1",
      cliponaxis = FALSE,
      type = "bar",
      color = ~ factor(type, levels = c("Policy and objectives",
                                        "Risk management",
                                        "Assurance",
                                        "Promotion",
                                        "Culture")
                       ),
      colors = c('#0070C0', '#44546A', '#DAE3F3', '#00B0F0', '#002060'),
      text = ~ paste0(type, ': ', score_text),
      textfont = list(color = 'transparent'),
      hovertemplate = paste0('%{text}<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>%
    add_trace(
      inherit = FALSE,
      data = data_prep_maturity,
      x = ~ year,
      y = 60,
      yaxis = "y1",
      type = 'scatter',
      mode = "line",
      name = "Target other MOs",
      line = list (color = '#FF0000', width = 3, dash = 'solid'),
      hoverinfo = 'none',
      showlegend = F
    ) %>%
    add_trace(
      inherit = FALSE,
      data = data_prep_maturity,
      x = ~ year,
      y = 80,
      yaxis = "y1",
      type = 'scatter',
      mode = "line",
      name = "Target risk mgt",
      line = list (color = '#FF0000', width = 3, dash = 'solid'),
      # hovertemplate = paste0('%{x}'),
      hoverinfo = 'none',
      showlegend = F
    ) %>%
    add_trace (
               inherit = FALSE,
               x = 2024.3,
               y = 60,
               yaxis = "y1",
               type = 'scatter',
               mode = "marker",
               name = "fake series",
               marker = list (color = 'transparent'),
               # hovertemplate = paste0('%{x}'),
               hoverinfo = 'none',
               showlegend = F
    ) %>%
    add_trace (
      inherit = FALSE,
      data = data_prep_maturity,
      x =  ~ year,
      y = 60,
      yaxis = "y1",
      type = 'scatter',
      mode = "marker",
      name = "",
      marker = list (color = 'transparent'),
      hovertemplate = paste0('-'),
      # hoverinfo = 'none',
      showlegend = F
    ) %>%
    add_annotations (text = c('Risk management target',
                              'Other MO targets'),
                     x = 0.97,
                     y = c(86, 66),      
                     showarrow = F,
                     xref = "paper",
                     yref = "y",
                     yanchor = "center",
                     xanchor = "right",
                     align = "right",
                     # textangle = -90,
                     font = list(color = '#FF0000', size = myfont)
    ) %>% 
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>% 
    layout(
      font = list(family = "Roboto"),
      title = list(text=paste0("EoSM - ", main_safety_ansp),
                   y = mytitle_y, 
                   x = mytitle_x, 
                   xanchor = mytitle_xanchor, 
                   yanchor = mytitle_yanchor,
                   font = list(size = mytitle_font_size)
      ),
      dragmode = FALSE,
      bargap = 0.25,
      hovermode = "x unified",
      hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
      xaxis = list(title = "",
                   gridcolor = 'rgb(255,255,255)',
                   showgrid = FALSE,
                   showline = FALSE,
                   showticklabels = TRUE,
                   dtick = 1,
                   # range = list(2020, 2024),
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   tickfont = list(size = myfont)
      ),
      yaxis = list(title = "Minimum maturity level",
                   # gridcolor = 'rgb(255,255,255)',
                   showgrid = TRUE,
                   showline = FALSE,
                   # dtick = 20,
                   # showticklabels = TRUE,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   tickvals = c(20, 40, 60, 80),
                   ticktext = c("A  ", "B  ", "C  ", "D  "),
                   zerolinecolor = 'rgb(240,240,240)',
                   titlefont = list(size = myfont), 
                   # showticklabels = FALSE
                   tickfont = list(size = myfont, color = 'black')
      ),
      # showlegend = FALSE
      legend = list(
        orientation = 'h', 
        xanchor = "left",
        x = -0.05, 
        y =-0.1,
        font = list(size = myfont*0.95)
      ),
      margin = list(t = mymargin/2, r = mymargin)
  )
  
}

myc(NA, 320, 14, 70)



