# import data  ----
if (country == "SES RP3"){
  ## SES case ----
  data_raw_kep  <-  read_xlsx(
    paste0(data_folder, "SES file.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_KEP",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() |> 
    #so it has the same structure as the state case
    mutate(entity_name = "SES RP3") 

  data_raw_scr  <-  read_xlsx(
    paste0(data_folder, "SES file.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_SCR",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names()  |> 
    #so it has the same structure as the state case
    mutate(entity_name = "SES RP3") 
    
} else  {
  ## State case ----
  data_raw_kep  <-  read_xlsx(
    paste0(data_folder, "ENV dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_KEP",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_scr  <-  read_xlsx(
    paste0(data_folder, "ENV dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_SCR",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
 
}

# prepare data ----
## create sequence of years to ensure full series
rp3_years <- 2020:2024
rp3_years <- data.frame(rp3_years) %>% rename(xlabel = rp3_years)

data_prep_kep <- data_raw_kep %>% 
  mutate(type = indicator_type,
         mymetric = round(kep_value_percent,2),
         xlabel = year) %>% 
  filter(
    entity_name == country,
    year <= year_report
  ) %>% 
  select(xlabel, type, mymetric)

data_prep_kep_full <- data_prep_kep %>% 
  right_join(rp3_years, by = 'xlabel') 

data_prep_scr <- data_raw_scr %>% 
  mutate(type = indicator_type,
         mymetric = round(scr_value*100,2),
         xlabel = year) %>% 
  filter(
    entity_name == country,
    year <= year_report
  ) %>% 
  select(xlabel, type, mymetric)

data_prep_scr_full <- data_prep_scr %>% 
  right_join(rp3_years, by = 'xlabel') 


data_prep <- data_prep_kep_full %>% 
  rbind(data_prep_scr_full)


# chart parameters ----
mysuffix <- "%"
mydecimals <- 2

### trace parameters
mycolors = c('#FFC000', '#5B9BD5' )
###set up order of traces
myfactor <- c("KEP", "SCR")

myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- 'middle'
mytextfont_color <- 'black'

### layout 
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <-  paste0("KEP & SCR")
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "KEP & SCR (%)"
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".2f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin <- mymargin

# plot chart ----
mybarchart(data_prep, mywidth, myheight+30, myfont, mylocalmargin, mydecimals) %>% 
  add_empty_trace(., data_prep) 
