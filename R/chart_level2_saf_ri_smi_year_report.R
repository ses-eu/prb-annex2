if (!exists("country") | is.na(country)) {country <- "Poland"
source("R/parameters.R")
}

if (!exists("safindicator")) {safindicator <- "smi"}


# import data  ----
if (safindicator == "ri") {
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SAF EoSM.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "RI - occurrences",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() %>% 
    mutate(
      main_indicator = number_of_ri,
      main_indicator_ans = number_of_ri_with_ans_contribution
    )
  
  } else if (safindicator == "smi") {
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SAF EoSM.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "SMI - occurrences",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() %>% 
    mutate(
      main_indicator = number_of_smi,
      main_indicator_ans = number_of_smi_with_ans_contribution
    )
}

# process data  ----
data_calc <- data_raw %>% 
  filter(state == .env$country) %>% 
  arrange(year) %>% 
  mutate(
    variation_perc = if_else(lag(main_indicator, 1) == 0, NA, main_indicator / lag(main_indicator, 1) -1) * 100,
    rate_variation_perc = if_else(lag(rate_per_100_000, 1) == 0, NA, rate_per_100_000 / lag(rate_per_100_000, 1) -1)* 100,
    ans_variation_perc = if_else(lag(main_indicator_ans, 1) == 0, NA, main_indicator_ans / lag(main_indicator_ans, 1) -1)* 100,
    rate_ans_variation_perc = if_else(lag(rate_per_100_000_with_ans_contribution, 1) == 0, NA, rate_per_100_000_with_ans_contribution / lag(rate_per_100_000_with_ans_contribution, 1) -1)* 100,
  ) %>% 
  filter(year == year_report)

data_prep1 <- data_calc %>%
  select(
    main_indicator,
    rate_per_100_000, 
    main_indicator_ans, 
    rate_per_100_000_with_ans_contribution
  )  %>% 
  pivot_longer(cols = everything(), names_to = "ylabel", values_to = "mymetric") %>% 
  mutate(
    ylabel = case_when(
      ylabel == "main_indicator" ~ paste0("Number of ", toupper(safindicator)," in the MS&nbsp;&nbsp;\n(airports included in performance plans)&nbsp;&nbsp;"),
      ylabel == "rate_per_100_000" ~ paste0("Rate of ", toupper(safindicator),"&nbsp;&nbsp;\n(per 100,000 mvts)&nbsp;&nbsp;"),
      ylabel == "main_indicator_ans" ~ paste0("Number of ", toupper(safindicator)," with ANS contribution&nbsp;&nbsp;\n(airports included in performance plans)&nbsp;&nbsp;"),
      ylabel == "rate_per_100_000_with_ans_contribution" ~ paste0("Rate of ", toupper(safindicator)," with ANS contribution&nbsp;&nbsp;\n(per 100,000 mvts)&nbsp;&nbsp;")
    ),
    mylabel = as.character(round(mymetric,2 ))
  )

data_prep2 <- data_calc %>%
  select(
    variation_perc,
    rate_variation_perc, 
    ans_variation_perc, 
    rate_ans_variation_perc
  ) %>% 
  pivot_longer(cols = everything(), names_to = "ylabel", values_to = "mymetric") %>% 
  mutate(
    mylabel = paste0(if_else(mymetric>=0, "+", ""),as.character(round(mymetric,1 )), "%")
  )


# plot charts  ----

p1 <- myhbarc2(df = data_prep1,
         suffix = "",
         local_factor = c(
           paste0("Rate of ", toupper(safindicator)," with ANS contribution&nbsp;&nbsp;\n(per 100,000 mvts)&nbsp;&nbsp;"),
           paste0("Number of ", toupper(safindicator)," with ANS contribution&nbsp;&nbsp;\n(airports included in performance plans)&nbsp;&nbsp;"),
           paste0("Rate of ", toupper(safindicator),"&nbsp;&nbsp;\n(per 100,000 mvts)&nbsp;&nbsp;"),
           paste0("Number of ", toupper(safindicator)," in the MS&nbsp;&nbsp;\n(airports included in performance plans)&nbsp;&nbsp;"),
           NULL
                          ),
         hovertemplate = paste0('%{x:,.1f}<extra></extra>'),         
         mybarcolor_pos = '#196AB5',
         mybarcolor_neg = 'transparent',
         
         textposition = "outside",
         
         xaxis_tickfont_size =  myfont-1,
         yaxis_tickfont_size = myfont -1,
         
         title_text = "",
         hovermode = "closest",
         margin = list(t= 40)
         )

p2 <- myhbarc2(df = data_prep2,
               suffix = "%",
               local_factor = c(
                 "rate_ans_variation_perc",
                 "ans_variation_perc",
                 "rate_variation_perc",
                 "variation_perc",
                 NULL
               ),
               hovertemplate = paste0('%{x:,.1f}%<extra></extra>'),         
               hovermode = "closest",
               mybarcolor_pos = '#FFC000',
               mybarcolor_neg = '#92D050',
               
               textposition = "outside",
               
               xaxis_ticksuffix = "%",
               xaxis_tickfont_size =  myfont-1,
               yaxis_showticklabels = FALSE,
               
               title_text = paste0(year_report, " vs ",year_report-1) ,
               title_x = 0.98,
               title_y = 0.83,
               title_xanchor = "center",
               title_yanchor =  "center",

               margin = list(t= 40, r = 50)
)

subplot(p1, p2, nrows = 1, shareY = FALSE, titleX = TRUE, titleY = TRUE, widths = c(0.55, 0.45), margin = 0.10) %>% 
  layout(showlegend = FALSE)

