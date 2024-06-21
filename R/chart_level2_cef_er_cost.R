
# fix ez if script not executed from qmd file ----
if (exists("ez") == FALSE) {ez = 1}
# ez=1

# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "CEFF dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Enroute_T1",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
data_prep <- data_raw %>% 
  filter(
    entity_code == ecz_list$ecz_id[ez]) %>% 
  mutate(
    mymetric = round(x5_3_cost_nc2017/xrate2017/10^6,2)
  ) %>%  
  select(
    year,
    status,
    mymetric
  ) %>%  
  filter(year > 2021) %>% 
  mutate(xlabel = as.character(year),
         xlabel = str_replace(xlabel, "20202021", "2020-2021"),
         status = str_replace(status, "A", "Actual costs"),
         status = str_replace(status, "D", "Determined costs")
  ) %>% 
  arrange(xlabel) %>% 
  rename(type = status)

### replace 0 by NAs so they are not plotted
data_prep[data_prep == 0] <- NA

# chart parameters ----
mysuffix <- ""
mydecimals <- 2

### trace parameters
mycolors = c('#5B9BD5', '#FFC000')
###set up order of traces
myfactor <- data_prep %>% select(type) %>% unique() 
invisible(as.list(myfactor$type))
myfactor <- sort(myfactor$type, decreasing = TRUE)
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "outside"
myinsidetextanchor <- NA
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0("Total en route costs - ", ecz_list$ecz_name[ez])

#### xaxis

#### yaxis
myyaxis_title <- "En route costs (€2017'000)"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".0f"

#### legend

#### margin

# plot chart  ----
mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin)
