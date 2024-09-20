# import data  ----
if (country == "SES RP3"){
  ## SES case ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SES file.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "RI - occurrences",
    range = cell_limits(c(1, 1), c(NA, 5))) %>%
    as_tibble() %>% 
    clean_names() |> 
    mutate(state = "SES RP3")
    
} else  {
  ## State case ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SAF EoSM.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "RI - occurrences",
    range = cell_limits(c(1, 1), c(NA, 6))) %>%
    as_tibble() %>% 
    clean_names() 
}

# prepare data ----

data_prep <- data_raw %>%
  filter(
    state == country,
    year <= year_report) |> 
  select(-state, -type, -reference_period) |> 
  pivot_longer(-c(year), names_to = "type", values_to = "mymetric") |> 
  mutate(xlabel = year,
         type = if_else(type == "rate_per_100_000",
                        "RI",
                        "EU Wide Average"),
         mytextposition = "top center",
         linedash = if_else(type == "RI",
                            "solid",
                            if_else(country == "SES RP3",
                                    "solid",
                                    "dot")
                            )
         ) |> 
  select(xlabel, type, mymetric, mytextposition, linedash) |> 
  #otherwise the lindash column does not work
  arrange(desc(linedash))


# chart parameters ----
mysuffix <- ""
mydecimals <- 2

### trace parameters
mycolors = c('#00B0F0', '#00B0F0' )
###set up order of traces
myfactor <- c("RI", "EU Wide Average")

myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextfont_color <- 'black'

#### title
mytitle_text <-  paste0("RIs per 100,000 movements")
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "RIs per 100,000 movements"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".1f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin <- mymargin

# plot chart ----
mylinechart(data_prep, mywidth, myheight, myfont, mylocalmargin, mydecimals) |> 
  layout(yaxis = list(rangemode = "tozero"),
         xaxis = list(range = c(2019.5, 2024.5)))

