if (exists("country") == FALSE) {country <- "France"}

source("R/parameters.R")

# import data  ----
if (country != 'SES RP3') {
  ## State case ----
  data_raw_ansp <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "CAPEX per Main ANSP",
    range = cell_limits(c(2, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 

  data_raw_uw <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Union-wide median",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  }  

# process data  ----
data_prep_uw <- data_raw_uw %>% 
  filter(variable == "New major investments" | variable == "Other new investments") %>% 
  mutate(mymetric = round(percent*100, 0)) %>% 
  select(
    xlabel = union_wide_median,
    type = variable,
    mymetric
  )


data_prep_ansp <- data_raw_ansp %>% 
  filter(member_state == .env$country) %>% 
  mutate(
    share_new_major_inv = total_new_major_investments / total,
    share_other_new_inv = other_new_investments_as_per_pp / total
  ) %>% 
  select(share_new_major_inv, share_other_new_inv) %>% 
  gather() %>% 
  mutate(xlabel = "ANSP",
         type = case_when(
           key == "share_new_major_inv" ~ "New major investments",
           key == "share_other_new_inv" ~ "Other new investments"),
         mymetric = round(value*100,0)
         ) %>% 
  select(xlabel, type, mymetric)

data_prep <- rbind(data_prep_uw, data_prep_ansp) %>% 
  mutate(xlabel = factor(xlabel, levels = c("Union-wide median", "ANSP")))


# chart ----
## chart parameters ----
local_suffix <- "%"
local_decimals <- 0

###set up order of traces
local_hovertemplate <- paste0('%{y:,.', local_mydecimals, 'f}', local_suffix)

#### legend
if (knitr::is_latex_output()) {
  local_legend_y <- mylegend_y
  local_legend_x <- -0.18
  local_legend_xanchor <- 'left'
  local_legend_fontsize <- myfont-1
  
} else {
  local_legend_y <- -0.12
  local_legend_x <- 0.5
  local_legend_xanchor <- 'center'
  local_legend_fontsize <- myfont
  
}

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight + 20,
                      colors = c('#FFF000', '#22A0E7' ),
                      local_factor = c("Other new investments", "New major investments"),
                      shape = c("/", "", "/", ""),
                      
                      suffix = local_suffix,
                      decimals = local_decimals,
                      
                      hovertemplate = local_hovertemplate,
                      hovermode = "x unified",
                      
                      textangle = 0,
                      textposition = "inside",
                      textfont_color = 'black',
                      insidetextanchor = 'middle',
 
                      bargap = 0.25,
                      barmode = 'stack',
                      
                      title_text = "",
                      title_y = 0.99,
                      
                      yaxis_title = "Asset value for new investment for RP3",
                      yaxis_ticksuffix = "%",
                      yaxis_tickformat = ".0f",
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize)

myplot
