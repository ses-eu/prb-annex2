
# fix ez if script not executed from qmd file ----
if (exists("tz") == FALSE) {tz = 1}
# tz=1


# wrap script in function so it can be called from qmd ----
myfig <- function(){

  ## initialise list to store plots ----
  myplot = list()
  
  ## loop through czs ----
  for (tz in 1:no_tcz) {
  
  ### import data  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Terminal_T1",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ### prepare data ----
  data_prep <- data_raw %>% 
    filter(
      entity_code == tcz_list$tcz_id[tz]) %>% 
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
  
  ### chart parameters ----
  mychart_title <- paste0("Terminal unit costs - ", tcz_list$tcz_name[tz])
  myaxis_title <- "Terminal unit costs (€2017)"
  mylegend_y_position <- -0.1
  mycolors = c('#5B9BD5', '#FFC000')

  mytextangle <- -90
  mytextposition <- "inside"
  
  mydtick <- '1'
  mytickformat_x <- "0"
  
  myticksuffix <- ""
  mytickformat_y <- ",.0f"
  
  ###set up order of traces
  myfactor <- data_prep %>% select(status) %>% unique() 
  as.list(myfactor$status)
  myfactor <- sort(myfactor$status, decreasing = TRUE)
  
  
  ### plot chart ----

  myplot[[tz]] <-   mybarc_nonst(mywidth, myheight, myfont, mymargin)


  }
  
  ## return plot list ----
  return(htmltools::tagList(myplot))
  # https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document
}