if (exists("country") == FALSE) {country <- "Bulgaria"}

source("R/parameters.R")

# import data  ----
if (!exists("data_category")) {
  source("R/get_investment_data.R")
}


# process data  ----
data_prep <- data_category %>% 
  filter(member_state_1 == .env$country) %>% 
  select(atm, cns, infra, ancillary, unknown, other) %>% 
  summarise (atm = sum(atm, na.rm=TRUE),
             cns = sum(cns, na.rm=TRUE), 
             infra = sum(infra, na.rm=TRUE), 
             unknown = sum(unknown, na.rm=TRUE),
             other = sum(other, na.rm=TRUE) + sum(ancillary, na.rm=TRUE)) %>% 
  mutate(total = rowSums(across(everything())),
         atm_share = atm/total * 100,
         cns_share = cns/total * 100,
         infra_share = infra/total * 100,
         unknown_share = unknown/total *100,
         other_share = other/total*100)%>% 
  select(atm_share,
         cns_share,
         infra_share,
         unknown_share,
         other_share) %>% 
  gather() %>% 
  mutate(
    type = case_when(
      key == "atm_share" ~ "ATM systems",
      key == "cns_share" ~ "CNS systems",
      key == "infra_share" ~ "Infrastructure",
      key == "unknown_share" ~ "Unknown",
      key == "other_share" ~ "Other"
    ),
    xlabel = "ANSP",
    mymetric = round(value,0),
    textposition = if_else(mymetric == 0 | mymetric > 2, "inside", "outside"),
    textlabel = if_else(mymetric == 0, " ", paste0(format(round(mymetric,0), nsmall = 0), "%"))
  )  %>% 
  select(type, mymetric, textlabel, textposition) 


# chart ----
## legend
if (knitr::is_latex_output()) {
  local_legend_x <- 1
  local_legend_y <- 0.5  
} else {
  local_legend_x <- 0.5
  local_legend_y <- -0.05
  local_legend_xanchor <- 'center'
}



# plot chart ----
mydonutchart(data_prep, 
             colors = c( '#044598', '#22A0DD', '#58595B', '#FFF000', '#7030A0'),
             hovertemplate = "%{label}: %{value}%<extra></extra>",
             title_text = "Asset value by type of investment",
             minsize = 14,
             legend_x = local_legend_x,
             legend_y = local_legend_y,
             legend_xanchor = local_legend_xanchor,
             legend_orientation = "h")

