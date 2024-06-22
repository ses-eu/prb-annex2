
# fix ez if script not executed from qmd file ----
if (exists("cz") == FALSE) {cz = c("1", "terminal")}
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

# get data ----
data_prep <- regulatory_result(cztype, mycz)
data_prep <- data_prep %>% 
  mutate(mymetric = regulatory_result / 1000) %>% 
  rename(xlabel = year_text)
  
# chart parameters ----
mysuffix <- ""
mydecimals <- 2

### trace parameters
mycolors = c( '#5B9BD5', '#FFC000', '#BFBFBF')
###set up order of traces
myfactor <- c("Main ANSP",
              "Other ANSP",
              "MET")
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- "middle"
mytextfont_color <- 'transparent'

### layout parameters
mybargap <- 0.25
mybarmode <- 'stack'

#### title
mytitle_text <- paste0("Regulatory result at CZ level")

#### xaxis

#### yaxis
myyaxis_title <- "Regulatory result (€M)"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ",.0f"

#### legend

#### margin

# plot chart  ----
mybarchart(data_prep, mywidth, myheight+10, myfont, mylocalmargin, mydecimals) %>% 
  add_empty_trace(., data_prep)  
