#libraries
library(tidyr)
library(dplyr)
library(openxlsx)
library(readxl)
library(plotly)
library(stringr)
library(janitor)
library(webshot)
library(data.table)
library(magick)
library(reactable)
library(gt)
library(htmltools)
library(here)
library(fs)


# parameters ----
data_folder <- 'G:/HQ/dgof-pru/Data/SES Monitoring Dashboard/PBI files/'
data_folder_a2 <- 'G:/HQ/dgof-pru/Data/SES Monitoring Dashboard/Annex 2/data/'
# country <- 'Bulgaria'
# year_report <- 2022

# functions ----
## right x characters function ----
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}  

## read range function ----
read_range <- function(file, sheet, range){
  read_excel(
    file,
    sheet = sheet,
    range = range) %>% 
    mutate_all(~ str_replace_all(., "\r\n\r\n", "\r\n")) %>% 
    mutate_all(~ str_replace_all(., "<br><br><br><br>", "<br><br>"))   
}

## read table range function ----
read_mytable <- function(file, sheet, table){
  wb <- loadWorkbook(paste0(data_folder, file))
  tables <- getTables(wb, sheet = sheet)
  # get the range
  table_range <- names(tables[tables == table])
  # read range
  read_range(paste0(data_folder, file), sheet, table_range) 
}

## export figure function ----
# the export function needs webshot and PhantomJS. Install PhantomJS with 'webshot::install_phantomjs()' and then cut the folder from wherever is installed and paste it in C:\Users\[username]\dev\r\win-library\4.2\webshot\PhantomJS

export_fig <- function (fig, fig_name, width, height) {
  fig_dir <- paste0('images/', year_report, '/', country,'/')
  invisible(export(fig, paste0(fig_dir, fig_name)))
  invisible(figure <- image_read(paste0(fig_dir,fig_name)))
  invisible(cropped <- image_crop(figure, paste0(width, "x", height)))
  invisible(image_write(cropped, paste0(fig_dir, fig_name)))
}

# get main state parameters  ----
params_table <- read_mytable("Lists.xlsx", "Lists", "Table_States") %>% clean_names()
state_parameters <- params_table %>% 
  filter(state == country) %>% clean_names()

aua_entities <- read_mytable("Lists.xlsx", "Lists", "Table_AUA") %>% 
  filter(State == country) %>% clean_names()

main_ansp <- state_parameters %>% select(main_ansp) %>% pull()
main_ansp_aua <- aua_entities %>%  filter(year == .env$year_report) %>% select(ansp_name) %>% pull()
nat_curr <- state_parameters %>% select(currency) %>% pull()
state_type <- state_parameters %>% select(dashboard_case) %>% pull()
pp_version <- state_parameters %>% select(pp_adoption_full) %>% pull()

# get state list
state_list <- params_table %>% select(state)

# get ACC list
acc_list <- read_mytable("Lists.xlsx", "Lists", "Table_ACCs") %>% 
  filter(State == country) %>% clean_names()

acc_no <- nrow(acc_list)
acc1 <- acc_list$acc_full_name[1]
acc2 <- if_else(acc_no <2, '', acc_list$acc_full_name[2])
acc3 <- if_else(acc_no <3, '', acc_list$acc_full_name[3])
acc4 <- if_else(acc_no <4, '', acc_list$acc_full_name[4])
acc5 <- if_else(acc_no <5, '', acc_list$acc_full_name[5])

# get ecz list and forecast
ecz_list <- read_mytable("Lists.xlsx", "Lists", "Table_ECZ") %>%
  filter(State == country) %>% 
  left_join(
    read_mytable("Lists.xlsx", "Lists", "Table_forecast"),
    by = "forecast_id"
  ) %>% clean_names()

# for spain we present only one  traffic zone
if (country == "Spain") {
  statfor_zone <- ecz_list %>% filter(statfor_ecz_name == "Spain") %>% select(statfor_ecz_name) %>% pull()
  ecz_list <- ecz_list %>% filter (ecz_name != "Spain all")
} else {
  statfor_zone <- ecz_list %>% filter(state  == country) %>% select(statfor_ecz_name) %>% pull()
}

forecast <- ecz_list %>% select(forecast) %>% pull() %>%  unique()
forecastid <- ecz_list %>% select(forecast_id) %>% pull() %>%  unique()

# get tcz list
tcz_list <- read_mytable("Lists.xlsx", "Lists", "Table_TCZ") %>%
  filter(State == country) %>% clean_names()

# get context data
context_data <- read_mytable("context_data.xlsx", "context", "Table_context") %>%  clean_names() %>% 
  filter(state == .env$country,
         year_report == .env$year_report  
  ) 

tsu_share <- paste0(format(round(as.numeric(context_data$tsu_share) *100,1), nsmall=1),'%')
ert_cost_share <- paste0(format(round(as.numeric(context_data$ert_cost_share) *100,1), nsmall=1),'%')
ert_trm_share <- context_data$ert_trm_share
no_apt_big <- context_data$no_apts_big
no_apt_small <- context_data$no_apts_small


other_orgs <- read_mytable("Lists.xlsx", "Lists", "Table_PP_2023_ANSPs") %>%  clean_names() %>% 
  filter(state == .env$country)
other_ansps <- other_orgs %>% filter(type == "Other ANSPs") %>% select(ansp) %>% filter(ansp != '-')
other_met <- other_orgs %>% filter(type == "MET Providers") %>% select(ansp) %>% filter(ansp != '-')

other_ansps_str <- '--'
if(nrow(other_ansps)>0) {
  for (i in 1:nrow(other_ansps)) {
    other_ansps_str <- paste0(if_else(i == 1, '', other_ansps_str),
                              '• ',
                              other_ansps[i,],
                              '<br/>')
  }
}

other_met_str <- '--' 
if(nrow(other_met)>0) {
  for (i in 1:nrow(other_met)) {
    other_met_str <- paste0(if_else(i == 1, '', other_met_str),
                            '• ',
                            other_met[i,], 
                            '<br/>')
  }
}


if (country != "Network Manager" & country != "SES RP3") {
  
  # get ceff file ----
  ceff_files <- list.files(paste0(data_folder_a2, 'ceff/'))
  
  for (i in 1:length(ceff_files)) {
    if (grepl(country, ceff_files[i], fixed = TRUE) == TRUE) {
      ceff_file <- ceff_files[i]
    }
  }
  
  file <-  paste0(data_folder_a2, "ceff/", ceff_file)
  ceff_file <-  paste0(data_folder_a2, "ceff/", ceff_file)
  
  # get er cap file ----
  cap_files <- list.files(paste0(data_folder_a2, 'cap/'))
  
  for (i in 1:length(cap_files)) {
    if (grepl('RP3_monitoring_CAPACITY', cap_files[i], fixed = TRUE) == TRUE) {
      cap_file <- cap_files[i]
    }
  }
  
  cap_file <-  paste0(data_folder_a2, "cap/", cap_file)
  
  # get trm cap file ----
  for (i in 1:length(cap_files)) {
    if (grepl('RP3_monitoring_CAP_ARP', cap_files[i], fixed = TRUE) == TRUE) {
      cap_trm_file <- cap_files[i]
    }
  }
  
  cap_trm_file <-  paste0(data_folder_a2, "cap/", cap_trm_file)
  
  # get env_kea file ----
  env_files <- list.files(paste0(data_folder_a2, 'env/'))
  
  for (i in 1:length(env_files)) {
    if (grepl('RP3_monitoring_KEA_VOL2', env_files[i], fixed = TRUE) == TRUE) {
      env_kea_file <- env_files[i]
    }
  }
  
  env_kea_file <-  paste0(data_folder_a2, "env/", env_kea_file)
  
  # get env_apt file ----
  for (i in 1:length(env_files)) {
    if (grepl('RP3_monitoring_ENV_ARP_VOL2', env_files[i], fixed = TRUE) == TRUE) {
      env_apt_file <- env_files[i]
    }
  }
  
  env_apt_file <-  paste0(data_folder_a2, "env/", env_apt_file)
  
  # get env_mil file
  for (i in 1:length(env_files)) {
    if (grepl('RP3_monitoring_ENV_MIL_VOL2', env_files[i], fixed = TRUE) == TRUE) {
      env_mil_file <- env_files[i]
    }
  }
  
  env_mil_file <-  paste0(data_folder_a2, "env/", env_mil_file)
  
  # get saf file ----
  saf_files <- list.files(paste0(data_folder_a2, 'saf/'))
  
  for (i in 1:length(env_files)) {
    if (grepl('_Safety', saf_files[i], fixed = TRUE) == TRUE) {
      saf_eosm_file <- saf_files[i]
    }
  }
  
  saf_eosm_file <-  paste0(data_folder_a2, "saf/", saf_eosm_file)
}

# plot bar chart with target  ----
mybarct <-  function(mywidth, myheight, myfont) {
  data_for_chart %>% 
    plot_ly(
      width = mywidth,
      height = myheight,
      x = ~ year,
      y = ~ target,
      yaxis = "y1",
      cliponaxis = FALSE,
      name = "",
      textfont = list(color = 'transparent'),
      type = 'scatter',  mode = 'lines',
      line = list(color = 'transparent', width = 0),
      hovertemplate = paste('%{x}:<extra></extra>'),
      # hoverinfo = "none",
      showlegend = F
    ) %>% 
    add_trace(
      inherit = FALSE,
      x = ~ year,
      y = ~ actual,
      yaxis = "y1",
      marker = list(color =('#FFC000')),
      text = ~ paste0(format(actual, nsmall = mytooltip_decimals),'%'),
      # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
      # textangle = -90,
      textposition = "inside", 
      cliponaxis = FALSE,
      insidetextanchor =  "middle",
      name = "Actual",
      textfont = list(color = 'black', size = myfont),
      type = "bar",
      hovertemplate = paste0(mymetric, ': %{y:.', mytooltip_decimals, 'f}%<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>%
    add_trace(
      inherit = FALSE,
      x = ~ year,
      y = ~ target,
      yaxis = "y1",
      type = 'scatter',  mode = 'lines+markers',
      line = list(color = '#FF0000', width = 3),
      marker = list(size = 9, color = '#FF0000'),
      name = "Target",
      opacity = 1,
      hovertemplate = paste0('Target: %{y:.', mytooltip_decimals ,'f}%<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>%
    add_trace(
      inherit = FALSE,
      x = ~ year,
      y = ~ target + 0.025 * max(data_for_chart$target),
      yaxis = "y1",
      mode = 'text',
      text = ~ paste0('<b>', format(target, nsmall = mytooltip_decimals) ,'%', '</b>'),
      textposition = "top", cliponaxis = FALSE,
      textfont = list(color = targetcolor, size = myfont),
      # hovertemplate = paste('<extra></extra>'),
      hoverinfo = "none",
      showlegend = F
    ) %>%
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>% 
    layout(
      font = list(family = "Roboto"),
      title = list(text = mychart_title,
                   y = 1, 
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
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   tickfont = list(size = myfont)
      ),
      yaxis = list(title = myaxis_title,
                   # gridcolor = 'rgb(255,255,255)',
                   showgrid = TRUE,
                   showline = FALSE,
                   tickprefix = " ",
                   ticksuffix = "% ",
                   tickformat = ".1f",
                   # showticklabels = TRUE,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   zerolinecolor = 'rgb(255,255,255)',
                   titlefont = list(size = myfont), tickfont = list(size = myfont)
      ),
      # showlegend = FALSE
      legend = list(
        orientation = 'h', 
        xanchor = "center",
        x = 0.5, 
        y =-0.1,
        font = list(size = myfont)
      ),
      margin = list (t = 40)
      
    )
}

# plot bar chart without target  ----
mybarc <-  function(mywidth, myheight, myfont) {
  data_for_chart %>% 
    plot_ly(
      width = mywidth,
      height = myheight,
      x = ~ year,
      y = 0, # to force all years in x axis
      yaxis = "y1",
      cliponaxis = FALSE,
      name = "",
      textfont = list(color = 'transparent'),
      type = "bar",
      marker = list(color =('transparent')),
      # hovertemplate = '',
      hoverinfo = "none",
      showlegend = F
    ) %>%
    add_trace(
      x = ~ year,
      y = ~ actual,
      yaxis = "y1",
      marker = list(color = mybarcolor),
      text = ~ paste0(format(actual, nsmall = mytooltip_decimals),'%'),
      # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
      # textangle = -90,
      textposition = "inside", 
      cliponaxis = FALSE,
      insidetextanchor =  "middle",
      name = mymetric,
      textfont = list(color = mytextcolor, size = myfont),
      type = "bar",
      hovertemplate = paste0(mymetric, ': %{y:.', mytooltip_decimals, 'f}%<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
      
    ) %>% 
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>% 
    layout(
      font = list(family = "Roboto"),
      title = list(text = mychart_title,
                   y = 1, 
                   x = 0, 
                   xanchor = 'left', 
                   yanchor =  'top',
                   font = list(size = myfont * 20/15)
      ),
      bargap = 0.25,
      barmode = 'stack',
      hovermode = "x unified",
      hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
      xaxis = list(title = "",
                   gridcolor = 'rgb(255,255,255)',
                   showgrid = FALSE,
                   showline = FALSE,
                   showticklabels = TRUE,
                   dtick = 1,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   tickfont = list(size = myfont)
      ),
      yaxis = list(title = myaxis_title,
                   # gridcolor = 'rgb(255,255,255)',
                   showgrid = TRUE,
                   showline = FALSE,
                   tickprefix = " ",
                   ticksuffix = "% ",
                   tickformat = ".1f",
                   # showticklabels = TRUE,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   zerolinecolor = 'rgb(255,255,255)',
                   titlefont = list(size = myfont), tickfont = list(size = myfont)
      ),
      # showlegend = FALSE
      legend = list(
        orientation = 'h', 
        xanchor = "center",
        x = 0.5, 
        y =-0.1,
        font = list(size = myfont)
      ),
      margin = list (t = 40)
      
    )
}
