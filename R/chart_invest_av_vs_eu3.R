if (exists("country") == FALSE) {country <- "Bulgaria"}

# source("R/parameters.R")

# import data  ----
if (!exists("data_union_wide") | !exists("data_category")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_prep_uw <- data_union_wide %>% 
  filter(variable == "ATM systems" | 
           variable == "CNS systems" |
           variable == "Infrastructure" |
           variable == "Other" |
           variable == "Unknown"
  ) %>% 
  mutate(mymetric = round(percent*100, 0)) %>% 
  select(
    xlabel = union_wide_median,
    type = variable,
    mymetric
  )

data_prep_ansp <- data_category %>% 
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
    NULL
    
  )  %>% 
  select(xlabel, type, mymetric)   

data_prep <- rbind(data_prep_ansp, data_prep_uw) %>% 
  mutate(xlabel = factor(xlabel, levels = c("ANSP", "Union-wide median")))


# chart ----
## chart parameters ----
local_suffix <- "%"
local_decimals <- 0

###set up order of traces
local_hovertemplate <- paste0('%{y:,.', local_decimals, 'f}', local_suffix)

#### legend
if (knitr::is_latex_output()) {
  local_legend_y <- mylegend_y
  local_legend_x <- -0.18
  local_legend_xanchor <- 'left'
  local_legend_fontsize <- myfont-1
  
} else {
  local_legend_y <- -0.12
  local_legend_x <- -0.1
  local_legend_xanchor <- 'left'
  local_legend_fontsize <- myfont-1
  
}

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight + 20,
                      colors = c( '#044598', '#22A0DD', '#58595B', '#FFF000', '#7030A0'),
                      local_factor = c("ATM systems",
                                       "CNS systems",
                                       "Infrastructure",
                                       "Other",
                                       "Unknown"),
                      shape = c("", "/", "", "/", "", "/", "", "/","", "/"),
                      
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
                      
                      yaxis_title = "Asset value for new investments for RP3 (%)",
                      yaxis_ticksuffix = "%",
                      yaxis_tickformat = ".0f",
                      yaxis_titlefont_size = myfont-1,
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize)

myplot




