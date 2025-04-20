if (!exists("country") | is.na(country)) {country <- "SES RP3"
source("R/parameters.R")
}


# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "SAF EoSM.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "SPI1c-RI_Airport",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  select(-rate) %>% 
  rename(
    name = name_apt,
    mvt = ifr_movements,
    rate = rate_per_100_000
  )

# process data ----
data_prep_mvt <- data_raw %>% 
  filter(year == year_report) %>% 
  arrange(desc(mvt)) %>% 
  mutate(rank = row_number()) %>% 
  select(
    rank,
    name,
    mvt,
    ri,
    rate
  ) %>% 
  slice(1:10)

mytype <- "TOP 10 APTs in terms of movements_"
mycolnames <- paste0(mytype, colnames(data_prep_mvt))

colnames(data_prep_mvt) <- mycolnames

data_prep_ri <- data_raw %>% 
  filter(year == year_report) %>% 
  arrange(desc(ri)) %>% 
  mutate(rank = row_number()) %>% 
  select(
    rank,
    name,
    mvt,
    ri,
    rate
  ) %>% 
  slice(1:10) 

mytype <- "TOP 10 APTs in terms of number of RIs_"
mycolnames <- paste0(mytype, colnames(data_prep_ri))

colnames(data_prep_ri) <- mycolnames

data_prep_rate <- data_raw %>% 
  filter(year == year_report) %>% 
  arrange(desc(rate)) %>% 
  mutate(rank = row_number()) %>% 
  select(
    rank,
    name,
    mvt,
    ri,
    rate
  ) %>% 
  slice(1:10) 

mytype <- "TOP 10 APTs in terms of rate of RIs_"
mycolnames <- paste0(mytype, colnames(data_prep_rate))

colnames(data_prep_rate) <- mycolnames



data_prep <- data_prep_mvt %>% 
  cbind(data_prep_ri) %>% 
  cbind(data_prep_rate) 


# Get column names of the final table
colnames_final <- colnames(data_prep)

# Define dynamic labels based on suffix
label_map <- c(
  rank = "#",
  name = "Airport name",
  mvt  = "APT mvts.",
  ri   = "Number of RI",
  rate = "Rate RI per 100,000 mvts."
)

# Extract the suffix from each column name
suffixes <- str_match(colnames_final, ".*_(rank|name|mvt|ri|rate)$")[,2]

# Map each suffix to its label
labels <- label_map[suffixes]

# Create the named vector for cols_label()
label_list <- set_names(labels, colnames_final)

nowrap_cols <- grep("_mvt$|_rank$", colnames(data_prep), value = TRUE)

rank_cols <- grep("_rank$", colnames(data_prep), value = TRUE)
mvt_cols <- grep("_mvts$", colnames(data_prep), value = TRUE)
name_cols <- grep("_name$", colnames(data_prep), value = TRUE)
ri_cols <- grep("ri$", colnames(data_prep), value = TRUE)
rate_cols <- grep("rate$", colnames(data_prep), value = TRUE)

# Add nowrap spans
data_prep_web <- data_prep %>%
  mutate(across(all_of(nowrap_cols), ~ paste0(
    "<span style='white-space:nowrap;'>", 
    format(as.numeric(.), big.mark = ",", scientific = FALSE),
    "</span>"
  )))

if (knitr::is_latex_output()) {
  data_for_table <- data_prep} else {
    data_for_table <- data_prep_web}

# web table ----
table1 <-mygtable(data_for_table,
                  myfont*0.9) %>% 
  tab_spanner_delim(
    delim = "_"
  ) %>% 
  tab_header(
    title = md("**Rate of RI per 100,000 airport movements**")
  ) %>% 
  cols_align(
    align = "left",
    columns = c(2,7,12)
  ) %>% 
  fmt_number(
    columns = c(3:4, 8:9, 13:14),  # Specify the columns to format
    decimals = 0,  # Number of decimal places
    use_seps = TRUE  # Use thousands separator
  ) %>% 
  fmt_number(
    columns = c(5, 10, 15),  # Specify the columns to format
    decimals = 2
  )  %>% 
  cols_label(.list = label_list)  


# create latex table
if (knitr::is_latex_output()) {
  table_level2_saf_ri_apt_ses <- table1 %>% 
    cols_width(
      contains("_rank") ~ pct(0.4),
      contains("_name") ~ pct(10),
      contains("_mvt") ~ pct(7),
      contains("_ri") ~ pct(6),
      contains("_rate") ~ pct(29.8/3)
    ) %>% 
    mylatex(NA) 
  
} else {
  table1 %>%
    fmt_markdown(columns = all_of(nowrap_cols)) 
  
}

