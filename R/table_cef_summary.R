# import data  ----
## dimension table ----
ecz_table <- read_mytable("Lists.xlsx", "Lists", "Table_ECZ") %>% clean_names() |> 
  select(State = state, ecz_id)

state_table <- state_list |> as_tibble() |> 
  filter(value != "MUAC", 
         value != "Luxembourg", 
         value != "SES RP3", 
         value != "Network Manager") |> 
  select(State = value) |> 
  left_join(ecz_table, by = "State")

## ses ----
data_raw_ses  <-  read_xlsx(
  paste0(data_folder, "SES CEFF.xlsx"),
  sheet = "SES_ERT_all",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

## ECZ  ----
data_raw_ecz  <-  read_xlsx(
  paste0(data_folder, "CEFF dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet =  "Enroute_T1",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names()


# prepare data ----
## SES ----
data_prep_ses_split <- data_raw_ses %>% 
  mutate(entity_name = "Union-wide") |> 
  select(entity_name, 
         year, 
         status, 
         su = su_cz,
         costs = costs_eur2017nominal_cz,
         costs_eur2017 = costs_eur2017_cz,
         duc = duc_2017eur_combined)

data_prep_ses_20202021 <- data_prep_ses_split |> 
  filter(year < 2022) |> 
  group_by(entity_name, status) |> 
  summarise(su = sum(su, na.rm = TRUE),
            costs = sum(costs, na.rm = TRUE),
            costs_eur2017 = sum(costs_eur2017, na.rm = TRUE),
            duc = sum(duc, na.rm = TRUE)
            ) |> 
  mutate(year = 20202021) |> 
  select(entity_name, year, status, su, costs, costs_eur2017, duc)

data_prep_ses <- data_prep_ses_split |> 
  filter(year > 2021) |> 
  rbind(data_prep_ses_20202021)

## state ----
data_prep_ecz <- data_raw_ecz |> 
  filter(year > 2021,
         entity_subtype == "ECZ") |> 
  select(
    entity_name, 
    year,
    status,
    su = x5_4_total_su,
    costs = x4_2_cost_excl_vfr,
    costs_eur2017 = x5_3_cost_nc2017,
    duc = x5_5_unit_cost_nc2017
  )

data_prep <- data_prep_ecz |> 
  rbind(data_prep_ses) |> 
  filter(year == if_else(year_report == 2020 | year_report == 2021, 20202021, year_report)) |> 
  select(-year)

data_prep_su <- data_prep |> 
  pivot_wider(id_cols = -c(costs, costs_eur2017, duc), values_from = "su", names_from = "status") |> 
  mutate(
    mymetric = (A/D-1) * 100,
    entity_name = factor(entity_name, levels = entity_name[order(mymetric)]),
    mycolor = if_else(entity_name == "Union-wide", "#FFC000", "#0070C0")) |> 
  arrange(entity_name)

data_prep_costs <- data_prep |> 
  pivot_wider(id_cols = -c(su, costs_eur2017, duc), values_from = "costs", names_from = "status") |> 
  mutate(mymetric = (A/D-1) * 100,
         mycolor = if_else(entity_name == "Union-wide", "#FFC000", "#0070C0")) 

data_prep_costs2017 <- data_prep |> 
  pivot_wider(id_cols = -c(su, costs, duc), values_from = "costs_eur2017", names_from = "status") |> 
  mutate(mymetric = (A/D-1) * 100,
         mycolor = if_else(entity_name == "Union-wide", "#FFC000", "#0070C0")) 

data_prep_duc <- data_prep |> 
  pivot_wider(id_cols = -c(su, costs, costs_eur2017), values_from = "duc", names_from = "status") |> 
  mutate(mymetric = (A/D-1) * 100,
         mycolor = if_else(entity_name == "Union-wide", "#FFC000", "#0070C0")) 

# plot function  ----
cef_plot <- function(df, xtitle) {
  plot_ly(
  df,
  x = ~mymetric, 
  y = ~entity_name,
  text = ~paste0(round(mymetric,0), "%"),
  textposition = "outside",
  textfont = list(size = myminsize),
  type = 'bar',
  cliponaxis = FALSE,
  hoverinfo = "none",
  orientation = 'h', 
  marker = list(color = ~mycolor)
  ) %>%
  layout(
    showlegend = FALSE,
    uniformtext=list(minsize = myminsize, mode='show'),
    font = list(family = myfont_family),
    xaxis = list(
      title = xtitle,
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(floor((min(df$mymetric)-5)/10)*10-5, ceiling((max(df$mymetric)+5)/10)*10+5)
      ), 
    yaxis = list(
      title = '', 
      tickfont = list(size=myfont*0.93),
      showgrid = FALSE
      )
    ) |> 
  config( responsive = TRUE,
          displaylogo = FALSE,
          displayModeBar = F
          # modeBarButtons = list(list("toImage")),
  )
}

p1 <- cef_plot(data_prep_su, '% difference service units') 
p2 <- cef_plot(data_prep_costs, '% difference total costs nominal €')
p3 <- cef_plot(data_prep_costs2017, '% difference total costs €<sub>2017</sub>')
p4 <- cef_plot(data_prep_duc, '% difference AUC/DUC')


subplot(p1, p2, p3, p4, nrows = 1, shareY = TRUE, titleX = TRUE) |> 
  layout(
    yaxis = list(tickmode = "linear", dtick = 1)
    )
  
