
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
  sheet = if_else(cztype == "terminal", "Terminal_T1", "Enroute_T1"),
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 


# prepare data ----
data_prep <- data_raw %>% 
  filter(
    charging_zone_code == mycz,
    entity_type_id == "ANSP1",
    year == .env$year_report
  ) %>% 
  mutate(
    x1_1_staff = x1_1_staff / (x5_2_inflation_index_nc2017/100) /xrate2017,
    x1_2_other_operating_cost = x1_2_other_operating_cost / (x5_2_inflation_index_nc2017/100) /xrate2017, 
    x1_3_depreciation = x1_3_depreciation / xrate2017,
    x1_4_cost_of_capital = x1_4_cost_of_capital / xrate2017,
    x1_5_exceptional_items = x1_5_exceptional_items / (x5_2_inflation_index_nc2017/100) /xrate2017,
    x4_1_cost_for_vfr_exempted =x4_1_cost_for_vfr_exempted / (x5_2_inflation_index_nc2017/100) /xrate2017
  ) |> 
  select(
    year,
    entity_name,
    status,
    x1_1_staff,
    x1_2_other_operating_cost,
    x1_3_depreciation,
    x1_4_cost_of_capital,
    x1_5_exceptional_items,
    x4_1_cost_for_vfr_exempted
  ) %>% 
  pivot_longer(cols = -c(year, entity_name, status),
    names_to = 'type', 
    values_to = 'value') %>% 
  pivot_wider(names_from = 'status', values_from = 'value') %>% 
  arrange(desc(type)) %>% 
  mutate(mymetric = (A-D)/10^6,
         mylabel = if_else(D == 0, '-', 
                           paste0(
                             if_else(mymetric > 0, '+', ''),
                             round((A/D-1) *100, 1), 
                             '%')),
         ylabel = as.factor(c("VFR exempted", 
                        "Exceptional items",
                        "Cost of capital",
                        "Depreciation costs",
                        "Other operating costs",
                        "Staff costs"))) 

# check if all values are negative or very small to fix formatting issue
all_negative_or_zero <- all(data_prep$mymetric <= 0.05)

# set x axis range to avoid labels being clipped
myroundup <- max(floor((log10(abs(max(data_prep$mymetric, na.rm = TRUE))))), floor((log10(abs(min(data_prep$mymetric, na.rm = TRUE))))))
range_min <- floor(min(data_prep$mymetric, na.rm = TRUE)/10^myroundup) * 10^myroundup - 10^myroundup/2
range_min <- if_else(range_min >0, 0, range_min)
range_max <- ceiling(max(data_prep$mymetric, na.rm = TRUE)/10^myroundup) * 10^myroundup + 10^myroundup/2

# chart parameters ----
mychart_title <- paste0("Costs by nature for main ANSP ", main_ansp," (M€<sub>2017</sub>) - ", year_report)
myaxis_title <- "Costs (M€<sub>2017</sub>)"
mybarcolor_pos <- '#A5A5A5'
mybarcolor_neg <- '#A5A5A5'
mytextcolor <- 'black'
myhovertemplate <- paste0('%{y} (A-D): %{x:,.1f}<extra></extra>')
# myxaxis_tickformat <- "0,.1f"
myxaxis_tickformat <- if_else(all_negative_or_zero, "0,.1f", "+0,")
mydecimals <- 3

###set up order of traces
myfactor <- c("VFR exempted", 
              "Exceptional items",
              "Cost of capital",
              "Depreciation costs",
              "Other operating costs",
              "Staff costs")

# plot chart  ----
myhbarc(mywidth, myheight, myfont, mymargin) %>% 
  layout(xaxis = list(range = c(range_min, range_max)))
