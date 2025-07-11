# import data  ----
if (country == "SES RP3"){
  ## SES case ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SES file.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "SMI - occurrences",
    range = cell_limits(c(1, 1), c(NA, 5))) %>%
    as_tibble() %>% 
    clean_names() |> 
    mutate(state = "SES RP3")
    
} else  {
  ## State case ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SAF EoSM.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "SMI - occurrences",
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
                        "Rate of SMI",
                        "EU Wide Average"),
         mytextposition = "top center",
         linedash = if_else(type == "Rate of SMI",
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
mycolors = c('#FFC000', '#FFC000' )
###set up order of traces
myfactor <- c("Rate of SMI", "EU Wide Average")

myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextfont_color <- 'black'
mytextfont_size <- myfont

#### title
mytitle_text <-  paste0("SMIs per 100,000 flight hours")
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "SMIs per 100,000 flight hours"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".1f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin <- mymargin

# plot chart ----
##I had to do it this way because when there are NA values the dash doesn't work
data_prep_s1 <- data_prep |> filter(type == "EU Wide Average")
data_prep_s2 <- data_prep |> filter(type == "Rate of SMI") |> 
  mutate(myothermetric = round(mymetric,2))

p1 <- mylinechart(data_prep_s1, mywidth, myheight, myfont, mylocalmargin, mydecimals) |> 
  layout(yaxis = list(rangemode = "tozero"),
         xaxis = list(range = c(2019.5, 2024.5)))

## additional target trace
myat_name <- "Rate of SMI"
myat_mode <- "line+markers"
myat_yaxis <- "y1"
myat_symbol <- NA
myat_marker_color <- '#FFC000'
myat_line_color <- '#FFC000'
myat_line_width <- mylinewidth
myat_showlegend <- T

myat_textbold <- FALSE
myat_textangle <- 0
myat_textposition <- 'top'
myat_textfont_color <- 'black'
myat_textfont_size <- myfont

p1 %>%  add_line_trace(., data_prep_s2) |> 
  layout(legend=list(
    traceorder= "reversed"))



