
# fix ez if script not executed from qmd file ----
if (exists("ez") == FALSE) {ez = 1}
# ez=1

# initialise list to store plots ----
myplot = list()

# loop through czs ----
for (ez in 1:no_ecz) {
  ## import data  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Enroute_T1",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  data_prep <- data_raw %>% 
    filter(
      charging_zone_code == ecz_list$ecz_id[ez],
      entity_type != "ECZ",
      year == .env$year_report
    ) %>% 
    mutate(
      entity_group = case_when(
        entity_type_id == "ANSP1" ~ "Main ATSP",
        entity_type_id %like% "ANSP" & entity_type_id != "ANSP1" ~ "Other ATSP",
        entity_type == "MUAC" ~ "Other ATSP",
        entity_type == "MET" ~ "METSP",
        .default = "NSA (including EUROCONTROL)"
      ),
      mymetric = round(x5_3_cost_nc2017/xrate2017/10^6,2),
      status = str_replace(status, "A", "Actual unit cost"),
      status = str_replace(status, "D", "Determined unit cost")
    ) %>% 
    select(
      year,
      status,
      entity_group,
      mymetric
    ) %>% 
    group_by(entity_group, status) %>% 
    reframe(year, status, entity_group, mymetric = sum(mymetric)) %>% 
    rename(year_text = entity_group)
  
  ### replace 0 by NAs so they are not plotted
  data_prep[data_prep == 0] <- NA
  
  ## chart parameters ----
  mychart_title <- paste0("Total en route costs per entity group - ", year_report)
  myaxis_title <- "En route costs (€2017'000)"
  ###set up order of traces
  myfactor <- data_prep %>% select(status) %>% unique() 
  as.list(myfactor$status)
  myfactor <- sort(myfactor$status, decreasing = TRUE)
  
  ## define chart function ----
  # function moved to utils
  
  ## plot chart  ----
  myplot[[ez]] <- mybarc_nonst(mywidth, myheight-20, myfont)
  
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document
