# 
# # parameters ----
# if (exists("data_folder") == FALSE) {
#   source("R/parameters.R")
# }

# import data  ----
data_raw_maturity  <-  read_xlsx(
  paste0(data_folder, "SAF EoSM.xlsx"),
  sheet = "A>P",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

data_raw_eosm  <-  read_xlsx(
  paste0(data_folder, "SAF EoSM.xlsx"),
  sheet = "EoSM",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----

data_prep_eosm <- data_raw_eosm %>% 
  filter(
    ms == country
  ) %>% 
  select(
    ms, entity_name, year, eo_sm_score
  ) 

data_prep_maturity <- data_raw_maturity %>% 
  filter(
    ms == country
    ) %>% 
  select(
    - c(entity, reference_period, ansp_meeting_targets_yearly)
  ) %>% 
  pivot_longer(-c(ms, entity_name, year), names_to = "type", values_to = "score") %>%  
  separate_wider_delim(type, delim = "_", names = c("status", "type"),
                       too_many = "merge") %>% 
  mutate(type = str_replace_all(type, "_", " "),
         type = str_to_sentence(type)
  ) %>% 
  filter(status == "actual") %>%
  mutate(
    score_text = case_when (
      score == 20 ~ 'A',
      score == 40 ~ 'B',
      score == 60 ~ 'C',
      score == 80 ~ 'D',
      .default = as.character(score)
     )
  )
  
  main_safety_ansp <- data_prep_maturity %>% select(entity_name) %>% unique() %>% pull()   

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
      data = data_prep_eosm,
      x = ~ year,
      y = ~ eo_sm_score,
      yaxis = "y2",
      type = 'scatter',
      mode = "markers",
      name = "EoSM score",
      marker = list (color = '#FFC000',
                     symbol = "diamond",
                     size = 11),
      hovertemplate = paste0('EoSM score %{y}<extra></extra>'),
      # hovertemplate = paste('%{text}<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>%
    add_trace(
      inherit = FALSE,
      data = data_prep_eosm,
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
      data = data_prep_eosm,
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
               # data = data_prep_eosm,
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
      data = data_prep_eosm,
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
                   y = 0.99, 
                   x = 0, 
                   xanchor = 'left', 
                   yanchor =  'top',
                   font = list(size = myfont * 20/15)
      ),
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
                   zerolinecolor = 'rgb(255,255,255)',
                   titlefont = list(size = myfont), 
                   # showticklabels = FALSE
                   tickfont = list(size = myfont, color = 'black')
      ),
      yaxis2 = list(title = "EoSM score",
                   overlaying = "y",
                   side = "right",
                   showgrid = FALSE,
                   showline = FALSE,
                   ticksuffix = "",
                   tickformat = ",.0f",
                   dtick = 25,
                   range = list(0,113),
                   zeroline = TRUE,
                   zerolinecolor = 'rgb(255,255,255)',
                   titlefont = list(size = myfont), tickfont = list(size = myfont)
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

# export to image ----
w = 1200
h = 600
export_fig(myc(w, h, 14 * w/900, 70 * w/1000),"saf_main.png", w, h)

