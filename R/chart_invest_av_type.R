if (exists("country") == FALSE) {country <- "Bulgaria"}

source("R/parameters.R")

# import data  ----
if (country != 'SES RP3') {
  ## State case ----
  data_raw <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Benefits | Investment category",
    range = cell_limits(c(2, NA), c(180, NA))) %>%
    as_tibble() %>% 
    clean_names() 
}  

# process data  ----
data_prep <- data_raw %>% 
  filter(member_state_1 == .env$country) %>% 
  select(atm, cns, infra, ancillary, unknown, other) %>% 
  summarise (atm = sum(atm, na.rm=TRUE),
             cns = sum(cns, na.rm=TRUE), 
             infra = sum(infra, na.rm=TRUE), 
             ancillary = sum(ancillary, na.rm=TRUE), 
             unknown = sum(unknown, na.rm=TRUE),
             other = sum(other, na.rm=TRUE)) %>% 
  mutate(total = rowSums(across(everything())),
         atm_share = atm/total * 100,
         cns_share = cns/total * 100,
         infra_share = infra/total * 100,
         ancillary_share = ancillary/total *100,
         unknown_share = unknown/total *100,
         other_share = other/total*100)%>% 
  select(atm_share,
         cns_share,
         infra_share,
         ancillary_share,
         unknown_share,
         other_share) %>% 
  gather() %>% 
  mutate(
    type = case_when(
      key == "atm_share" ~ "ATM systems",
      key == "cns_share" ~ "CNS systems",
      key == "infra_share" ~ "Infrastructure",
      key == "ancillary_share" ~ "Ancillary",
      key == "unknown_share" ~ "Unknown",
      key == "other_share" ~ "Other"
    ),
    type = factor(type, levels = c("ATM systems",
                                   "CNS systems",
                                   "Infrastructure",
                                   "Ancillary",
                                   "Other",
                                   "Unknown")),
    xlabel = "Asset value",
    mymetric = round(value,0),
    textlabel = if_else(mymetric == 0, "", paste0(format(mymetric, nsmall = 0), "%")),
    textposition = if_else(mymetric == 0 | mymetric > 2, "inside", "outside"),
    # mymetric = if_else(mymetric == 0, NA, mymetric),
    NULL
    
  )  %>% 
  select(xlabel, type, mymetric, textlabel, textposition)   
  




# chart ----
## legend
if (knitr::is_latex_output()) {
  local_legend_x <- 1
  local_legend_y <- 0.5  
} else {
  local_legend_x <- 0.82
  local_legend_y <- 0.5
  local_legend_xanchor <- 'left'
}


# plot chart ----
# myplot <- mybarchart(data_prep, mywidth, myheight + 20, myfont, mylocalmargin,
#                      mydecimals, mylocallegend_y, mylocallegend_fontsize)

myplot <- mydonutchart(data_prep, 
             colors = c( '#044598', '#22A0DD', '#58595B', 'black', '#FFF000', '#7030A0'),
             hovertemplate = "%{label}: %{value}%",
             title_text = "Asset value by type of investment",
             legend_x = local_legend_x,
             legend_y = local_legend_y,
             legend_xanchor = local_legend_xanchor,
             legend_orientation = "v")


myplot
