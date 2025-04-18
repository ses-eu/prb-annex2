if (exists("country") == FALSE) {country <- "SES RP3"}

# source("R/parameters.R")

# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "SES file.xlsx"),
  sheet = "EoSM interdipendency #ANSPs",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# process data  ----
data_prep <- data_raw %>% 
  filter(year == year_report) %>% 
  mutate(
    xlabel = "ANSPs maturity",
    mymetric = if_else(is.na(number_of_ans_ps), 0, number_of_ans_ps),
    type = factor(level, levels = c("A", "B", "C", "D")),
    textposition = "auto",
    textlabel = paste0(level, ": ", mymetric)
    ) %>% 
  arrange(desc(type)) %>% 
  select(
    xlabel,
    type,
    mymetric,
    textposition,
    textlabel
  )


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


# find starting angle
# Get total of the metric
total <- sum(data_prep$mymetric)
valueofd <- data_prep %>% filter(type == "D") %>% select(mymetric) %>% pull()

# Now convert to angle
rotation_angle <- valueofd/total * 360


# plot chart ----
mydonutchart(data_prep, 
             height = myheight,
             colors = c('#196AB4', '#00B0F0', '#FFC000', '#585858' ),
             hovertemplate = "%{label}: %{value}",
             title_text = "Number of ANSPs per maturity level",
             minsize = 14,
             sort = FALSE,
             rotation = -rotation_angle,
             legend_traceorder = "reversed",
             legend_x = local_legend_x,
             legend_y = local_legend_y,
             legend_xanchor = local_legend_xanchor,
             legend_orientation = "h",
             margin = list(t = 60))


