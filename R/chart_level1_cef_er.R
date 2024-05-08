
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
    entity_code == ecz_list$ecz_id[ez]) %>% 
  mutate(
    mymetric = round(x5_5_unit_cost_nc2017/xrate2017, 2)
  ) %>%  
  select(
    year,
    status,
    mymetric
  ) %>%  
  filter(year > 2021) %>% 
  mutate(year_text = as.character(year),
         year_text = str_replace(year_text, "20202021", "2020-2021"),
         status = str_replace(status, "A", "Actual unit cost"),
         status = str_replace(status, "D", "Determined unit cost")
  ) %>% 
  arrange(year_text)

### replace 0 by NAs so they are not plotted
data_prep[data_prep == 0] <- NA

## chart parameters ----
mychart_title <- paste0("En route unit costs - ", ecz_list$ecz_name[ez])
myaxis_title <- "En route unit costs (€2017)"

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
