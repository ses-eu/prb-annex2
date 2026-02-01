if (!exists("country") | is.na(country)) {country <- "Poland"
source("R/parameters.R")
}


# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "SAF EoSM.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Automated tools",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
data_prep_pdf <- data_raw %>% 
  filter(state == country & year == year_report) %>% 
  select(ri, smi)

data_prep <- data_prep_pdf %>% 
  mutate(
    smi = if_else(smi == 1,
                  "<span style='color:green; font-weight:bold; font-size:0.8rem;'>&nbsp;&nbsp;&#10003;</span>",
                  "<span style='color:red; font-size:0.8rem;'>&nbsp;&nbsp;&#10008;</span>"),
  
    ri = if_else(ri == 1,
                  "<span style='color:green; font-weight:bold; font-size:0.8rem;'>&nbsp;&nbsp;&#10003;</span>",
                  "<span style='color:red; font-size:0.8rem;'>&nbsp;&nbsp;&#10008;</span>")
  )



# plot table ----

table1 <- mygtable(data_prep, myfont)|> 
  tab_header(
    title = md(paste0("**Use of automated safety data recording system - ", md(year_report), "**"))
  ) %>% 
  cols_label(
    ri = "For RIs",
    smi = "For SMIs"
  ) %>% 
  fmt_markdown(columns = c(ri, smi)) |>
  cols_align(
    align = "center",
    columns = c(ri, smi)
  )


prbcolor <- '#0050A0' 
prbshading <- '#DDE6F7'

b <- cell_borders(
  sides  = "all",
  color  = prbcolor,
  weight = '1px',
  style  = "solid"
)



# table1_pdf <- mygtable(data_prep, myfont)|> 
#   tab_header(
#     title = md(paste0("**Use of automated safety data recording system - ", md(year_report), "**"))
#   ) %>% 
#   cols_label(
#     ri = "For RIs",
#     smi = "For SMIs"
#   ) %>% 
#   fmt_markdown(columns = c(ri, smi)) |>
#   cols_align(
#     align = "center",
#     columns = c(ri, smi)
#   ) %>% 
#   tab_options(
#     table.border.top.color = prbcolor,
#     table.border.top.width = px(1),
#     
#     table.border.left.color = prbcolor,
#     table.border.left.style = "solid",
#     table.border.left.width = px(1),
#     
#     table.border.right.color = prbcolor,
#     table.border.right.style = "solid",
#     table.border.right.width = px(1),
#     
#     heading.border.bottom.color = prbcolor,
#     heading.border.bottom.style = "solid",
#     heading.border.bottom.width = px(1),
#     
#     column_labels.border.top.color = prbcolor,
#     column_labels.border.top.style = "solid",
#     column_labels.border.bottom.color = prbcolor,
#     column_labels.border.bottom.style = "solid",
#     column_labels.border.bottom.width = px(1),
#     
#     table_body.hlines.color = prbcolor,
#     table_body.hlines.width = px(1),
#     
#     table_body.border.top.color = prbcolor,
#     table_body.border.top.width = px(1),
#     table_body.border.bottom.color = prbcolor,
#     table_body.border.bottom.width = px(1)
#   ) %>% 
# 
#   tab_style(
#     style = b,
#     locations = cells_body(columns = everything(), rows = everything())
#   ) %>%
#   tab_style(
#     style = b,
#     locations = cells_column_labels(columns = everything())
#   ) %>%
#   tab_style(
#     style = list(
#       cell_fill(color = prbcolor),
#       cell_text(color = "white")
#     ),
#     locations = cells_title(groups = "title")
#   ) %>%
#   tab_style(
#     style = list(
#       cell_fill(color = "#DDE6F7"),
#       cell_text(weight = "bold")
#     ),
#     locations = cells_column_labels(columns = everything())
#   )


if (!knitr::is_latex_output()) {
  table1
}


