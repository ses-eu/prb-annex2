
# fix ez if script not executed from qmd file ----
if (exists("cz") == FALSE) {cz = c("1", "enroute")}
# ez=1

# define cz ----
ez <- as.numeric(cz[[1]])
cztype <- cz[[2]]
# cztype <- "terminal"
mycz <- if_else(cztype == "terminal",
                tcz_list$tcz_id[ez],
                ecz_list$ecz_id[ez])
mycz_name <- if_else(cztype == "terminal",
                     tcz_list$tcz_name[ez],
                     ecz_list$ecz_name[ez])


# import data ----
data_reg_res <- regulatory_result(cztype, mycz)
  
# prep data ----
data_for_chart_wide <- data_reg_res %>% 
  filter(type == 'Main ANSP') %>% 
  mutate(
    share_rr_act_rev_expost = regulatory_result/actual_revenues * 100,
    share_rr_act_rev_exante = ex_ante_roe/actual_revenues * 100
    ) %>% 
  select(year_text,regulatory_result, ex_ante_roe, share_rr_act_rev_expost, share_rr_act_rev_exante)

## separate in two tables for pivoting 
data_for_chart_value <- data_for_chart_wide %>% 
  select(-c(share_rr_act_rev_expost, share_rr_act_rev_exante)) %>% 
  pivot_longer(-year_text, names_to = "xlabel", values_to = 'mymetric') %>% 
  mutate(xlabel = case_when(
    xlabel == 'regulatory_result'  ~ 'Ex-post',
    xlabel == 'ex_ante_roe'  ~ 'Ex-ante'),
    mymetric = mymetric/1000
  )

data_for_chart_share <- data_for_chart_wide %>% 
  select(-c(regulatory_result, ex_ante_roe)) %>% 
  pivot_longer(-year_text, names_to = "xlabel", values_to = 'share') %>% 
  mutate(xlabel = case_when(
    xlabel == 'share_rr_act_rev_expost'  ~ 'Ex-post',
    xlabel == 'share_rr_act_rev_exante'  ~ 'Ex-ante')
  )

data_prep <- data_for_chart_value %>% 
  left_join(data_for_chart_share, by = c('year_text', 'xlabel')) %>% 
  mutate(type = xlabel,
    xlabel = year_text,
    # year = as.numeric(str_replace_all(xlabel, "2020-2021", '2021')),
    # xlabel = year
    )

#

# chart parameters ----
mysuffix <- ""
mydecimals <- 1

### trace parameters
mycolors = c( '#CEE0EA', '#4B8DB1')
###set up order of traces
myfactor <- c("Ex-ante",
              "Ex-post")
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- "middle"
mytextfont_color <- 'transparent'

mytrace_showlegend <- F

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0("Regulatory Result for main ANSP ", main_ansp, ' (M€)')
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "Regulatory result (M€)"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- if_else(max(data_prep$mymetric, na.rm = TRUE) <10, 
                              ".1f",
                              ".0f")

#### legend
mylegend_y <- -0.24

#### margin
mylocalmargin = list(t = 60, b = 80, l = 40, r = 70)


# plot chart  ----
myplot<- mybarchart(data_prep, mywidth, myheight + 30, myfont, mylocalmargin, mydecimals) %>%  
add_trace(
  inherit = FALSE,
  data = data_prep,
  x = ~xlabel ,
  y = ~share,
  name = "RR in percent of en-route revenues",
  yaxis = "y2",
  # mode = "markers",
  type = 'box',
  color = ~ factor(type, levels = myfactor),
  line = list(color = '#E46C0A', width = mylinewidth), fillcolor = '#E46C0A',
  # marker = list(size = mylinewidth * 3, color = '#E46C0A'),
  hoverinfo = 'none',
  showlegend = F
) %>% 
  add_trace(
    data = filter(data_prep, type == "Ex-ante"),
    x = ~ xlabel,
    y = ~ paste0(share, '%'),
    yaxis = "y2",
    mode = "markers", 
    type = 'scatter',
    name = "RR (ex-ante) in % of revenues",
    text = "" ,
    marker = list(size = 1, 
                  color = "transparent"),
    hovertemplate <- paste0('%{y:,.2f}%'),
    showlegend = F
  )  %>% 
  add_trace(
    data = filter(data_prep, type == "Ex-post"),
    x = ~ xlabel,
    y = ~ share,
    yaxis = "y2",
    mode = "markers", 
    type = 'scatter',
    name = "RR (ex-post) in % of revenues",
    text = "" ,
    marker = list(size = 1, 
                  color = "transparent"),
    hovertemplate <- paste0('%{x:,.2f}%'),
    showlegend = F
  )  %>%     
  layout(
    yaxis = list(rangemode = "nonnegative"),
    yaxis2 = list(
    title = 'Regulatory result\nas a % of revenues',
    overlaying = "y",
    zerolinecolor = '#E8E8E8',
    rangemode = "tozero",
    ticksuffix = '%',
    tickformat = if_else(max(data_prep$share, na.rm = TRUE) >0.1, 
                          ".1f",
                          ".0f"),
    side = 'right',
    showgrid = FALSE
    ),
  boxmode = "group", bargroupgap = 0.1, boxgroupgap = 0.4, 
  boxgap = mybargap,
  # fake legend
  annotations = list(
      list (
      xanchor = "left",
      x = 0.0,
      y = mylegend_y,
      text = '■',
      font = list(size = myfont * 1.2, color = mycolors[[1]]),
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      ax = 0,
      ay = 0
      ),
      list (
        xanchor = "left",
        x = 0.07,
        y = mylegend_y,
        text = 'Ex-ante RR (in value)',
        font = list(size = myfont*0.9, color = "black"),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        ax = 0,
        ay = 0
      ),
      list (
        xanchor = "left",
        x = 0.6,
        y = mylegend_y,
        text = '■',
        font = list(size = myfont * 1.2, color = mycolors[[2]]),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        ax = 0,
        ay = 0
      ),
      list (
        xanchor = "left",
        x = 0.67,
        y = mylegend_y,
        text = 'Ex-post RR (in value)',
        font = list(size = myfont*0.9, color = "black"),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        ax = 0,
        ay = 0
      ),
      list (
        xanchor = "left",
        x = 0.0,
        y = mylegend_y-0.1,
        text = '<b>―</b>',
        font = list(size = myfont * 1.2, color = '#E46C0A'),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        ax = 0,
        ay = 0
      ),
      list (
        xanchor = "left",
        x = 0.07,
        y = mylegend_y-0.1,
        text = 'RR in percent of en route revenues',
        font = list(size = myfont*0.9, color = "black"),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        ax = 0,
        ay = 0
      ) 
      )
  ) %>% 
  add_empty_trace(data_prep)
  

myplot

