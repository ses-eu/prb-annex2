if (exists("country") == FALSE) {country <- "Belgium"}

source("R/parameters.R")

# import data  ----
if (country != 'SES RP3') {
  ## State case ----
  data_raw <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "CAPEX per Main ANSP",
    range = cell_limits(c(2, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
}  

# process data  ----
data_prep <- data_raw %>% 
  filter(member_state == .env$country) %>% 
  mutate(
    pp_new_major_share = new_major_investments_as_per_pp / total * 100,
    pp_other_major_share = other_new_investments_as_per_pp / total * 100,
    add_new_major_share = additional_new_major_investments / total * 100,
    add_other_major_share = 0
  ) %>% 
  select(
    pp_new_major_share,
    pp_other_major_share,
    add_new_major_share,
    add_other_major_share
  ) %>% 
  gather() %>% 
  mutate(category = ifelse(grepl("^pp", key), "pp", "add"),  # Extract category
         share_type = sub("^(pp_|add_)", "", key)) %>%  # Remove category prefix
  mutate(
    type = case_when(
      share_type == "new_major_share" ~ "New major investments",
      share_type == "other_major_share" ~ "Other major investments"
    ),
    xlabel = case_when(
      category == "pp" ~ "Included in the performance plan",
      category == "add" ~ "Additional"
    ),
    xlabel = factor(xlabel, levels = c("Included in the performance plan", "Additional")),
    mymetric = value
  ) %>% 
  arrange(desc(xlabel)) %>% 
  # filter(share_type == "new_major_share") %>% 
  select(xlabel, type, mymetric) 

# chart ----
## chart parameters ----
mysuffix <- "%"
mydecimals <- 0

### trace parameters
mycolors = c('#044598', '#22A0DD')
###set up order of traces
myfactor <- c("Other major investments", "New major investments")
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "inside"
myinsidetextanchor <- 'middle'
mytextfont_color <- 'white'
mytextfont_size <- myfont

### layout parameters
mybargap <- 0.25
mybarmode <- 'stack'

#### title
mytitle_text <- paste0("Asset value by investment category")
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "Total new asset value (%)"
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".0f"

#### legend
if (knitr::is_latex_output()) {
  mylocallegend_y <- mylegend_y
  mylegend_x <- -0.18
  mylegend_xanchor <- 'left'
  mylocallegend_fontsize <- myfont-1
  
} else {
  mylocallegend_y <- mylegend_y
  mylegend_x <- 0.5
  mylegend_xanchor <- 'center'
  mylocallegend_fontsize <- myfont
  
}

#### margin
mylocalmargin <- mymargin

# plot chart ----
myplot <- mybarchart(data_prep, mywidth, myheight + 20, myfont, mylocalmargin,
                     mydecimals, mylocallegend_y, mylocallegend_fontsize)

myplot