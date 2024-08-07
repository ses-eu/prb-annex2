
# fix ez if script not executed from qmd file ----
if (exists("cz") == FALSE) {cz = c("1", "terminal")}
# ez=1

# define cz ----
ez <- as.numeric(cz[[1]])
cztype <- cz[[2]]
# cztype <- "terminal"
mycz <- if_else(cztype == "terminal",
                tcz_list$tcz_id[ez],
                ecz_list$ecz_id[ez])
mycz_name <- if_else(cztype == "terminal",
                     tcz_list$tcz_name[ez],
                     ecz_list$ecz_name[ez])

# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "CEFF dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = if_else(cztype == "terminal", "Terminal_T2", "Enroute_T2"),
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 
  
# prepare data ----
data_prep_t2 <- data_raw %>% 
  filter(
    entity_code == mycz,
  ) %>% 
  ## to be used for the table, just kept until the table script is created
  # select(
  #   year,
  #   x3_1_investment,
  #   x3_3_cost_authority_qes,
  #   x3_4_ectl_cost_eur,
  #   x3_5_pension_cost,
  #   x3_6_interest_loan,
  #   x3_7_change_in_law
  # ) %>% 
  # pivot_longer(cols = -c(year),
  #              names_to = 'type', 
  #              values_to = 'mymetric') %>% 
  # mutate(xlabel = rep(c("New and existing investments", 
  #                       "Competent authorities\nand qualified entities costs",
  #                       "Eurocontrol costs",
  #                       "Pension costs",
  #                       "Interest on loans",
  #                       "Changes in law"), 4),
  #        year_text = as.character(year),
  #        year_text = str_replace(year_text, "20202021", "2020-2021"),
  # )
  select(year, x3_8_diff_det_cost_actual_cost) %>% 
  mutate(
    actual = case_when(
      year > year_report & year != 20202021 ~ NA,
      .default = round(x3_8_diff_det_cost_actual_cost/1000,2)
      ),
    year = as.character(year),
    year = str_replace(year, "20202021", "2020-2021")
  )
  
# t exchange rates
yearly_xrates <- get_xrates(cztype, mycz)

data_prep_xrates <- yearly_xrates %>% 
  filter(
    entity_code == mycz
  ) %>% 
  select(-entity_code) %>% 
  filter(year > 2020) %>% 
  mutate(year = as.character(year),
         year = if_else(year == '2021', '2020-2021', year)
        ) 

data_prep <- data_prep_t2 %>% 
  left_join(data_prep_xrates, by = 'year') %>% 
  mutate(mymetric = actual/pp_exchangerate,
         xlabel = year,
         type = 'Cost exempt') %>% 
  arrange(xlabel)
    
  
# chart parameters ----
mysuffix <- ""
mydecimals <- 1

### trace parameters
mycolors = c( '#8497B0')
###set up order of traces
myfactor <- data_prep %>% select(type) %>% unique() 
invisible(as.list(myfactor$type))
myfactor <- sort(myfactor$type, decreasing = TRUE)
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "auto"
myinsidetextanchor <- NA
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0("Cost exempt from cost sharing (€'000)")
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "Cost exempt from cost sharing\n(€'000)"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ",.0f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin = mymargin

# plot chart  ----
mybarchart(data_prep, mywidth, myheight+20, myfont, mylocalmargin, mydecimals)  %>% 
  add_empty_trace(., data_prep) 
