

#Section 1

#Housekeeping------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, psych, ltm, misty, lavaan, skimr, magrittr,
               gtsummary, epiDisplay, extrafont, flextable)

#The data----------------------------------------------------------------------
df <- readxl::read_xlsx("df.xlsx")
head(df)
names(df)

#Participant sociodemographic characteristics---------------------------------
##Age
summary(df$cage)

df <- df %>% 
  mutate(
    cage_cat = 
      ifelse(cage < 15, "12-14", "15-17") %>% 
      as.factor())

table(df$cage_cat)

tab1(df$cage_cat, graph = F)

 ##Gender
tab1(df$csex, graph = F)

table(df$csex)
str(df$csex)

df <- df %>% 
  mutate(
    csex = as.factor(csex))

tab1(df$csex, graph = F)

#The Revised Child Anxiety and Depression Scale (RCADS-25)--------------------
df %>% 
  dplyr::select(dplyr::starts_with("rcads"))

apply(df %>% 
        dplyr::select(dplyr::starts_with("rcads")), 
      2, tab1, graph = F) #need to re-code the questions to start from 0-3

for (i in c("rcads_1", "rcads_2", "rcads_3", "rcads_4", "rcads_5",
            "rcads_6", "rcads_7", "rcads_8", "rcads_9", "rcads_10",
            "rcads_11", "rcads_12", "rcads_13", "rcads_14", 
            "rcads_15", "rcads_16", "rcads_17", "rcads_18",
            "rcads_19", "rcads_20", "rcads_21", "rcads_22",
            "rcads_23", "rcads_24", "rcads_25")) {
  
  var_new <- paste(i, "n", sep = "")
  df[[var_new]] <- 
    ifelse(df[[i]] == 1, 0,
           ifelse(df[[i]] == 2, 1,
                  ifelse(df[[i]] == 3, 2,
                         ifelse(df[[i]] == 4, 3, df[[i]]))))
}


apply(df[, c("rcads_1", "rcads_1n", "rcads_2", "rcads_2n")],
      2, tab1, graph = F)

#Total scores for total, depression, and anxiety scales
all_items <- c("rcads_1n", "rcads_2n", "rcads_3n", "rcads_4n", 
               "rcads_5n", "rcads_6n", "rcads_7n", "rcads_8n", 
               "rcads_9n", "rcads_10n", "rcads_11n", "rcads_12n",
               "rcads_13n", "rcads_14n", "rcads_15n", "rcads_16n",
               "rcads_17n", "rcads_18n", "rcads_19n", "rcads_20n", 
               "rcads_21n", "rcads_22n", "rcads_23n", "rcads_24n",
               "rcads_25n")

df$rcads_score <- 
  rowSums(
    df[,  all_items], na.rm = T)

skim(df$rcads_score)


dep_items <- c("rcads_1n", "rcads_4n", "rcads_8n", "rcads_10n",
               "rcads_13n", "rcads_15n", "rcads_16n", "rcads_19n",
               "rcads_21n", "rcads_24n")

df$rcads_dep_score <-  
  rowSums(
    df[, dep_items], na.rm = T)
skim(df$rcads_dep_score)


anx_items <- c("rcads_2n", "rcads_3n", "rcads_5n", "rcads_6n",
               "rcads_7n", "rcads_9n", "rcads_11n", "rcads_12n",
               "rcads_14n", "rcads_17n", "rcads_18n", "rcads_20n",
               "rcads_22n", "rcads_23n", "rcads_25n")

df$rcads_anx_score <- 
  rowSums(
    df[, anx_items], na.rm = T)

skim(df$rcads_anx_score)

#Sampling adequacy and Bartlett's test
KMO(df[, all_items]) #0.92
cortest.bartlett(df[, all_items]) #P<0.001
ltm::descript(df[, all_items])


#Internal consistency for total, depression, and anxiety scales
psych::alpha(
  df[,  all_items]) #total items - 0.86

misty::item.omega(
  df[,  all_items], std = T) #total items - 0.88

psych::alpha( df[, dep_items]) #depression items - 0.82
misty::item.omega( df[, dep_items], std = T) #depression items - 0.83

psych::alpha(df[, anx_items]) #anxiety items - 0.77
misty::item.omega(df[, anx_items], std = T) #anxiety items - 0.79


#Confirmatory factor analysis-------------------------------------------------------
##Aim is to build 2 models; #Model1 - all items load to a single factor
                            #Model2 - 2 factor model for the sub scales

#One factor model
model1 <-  '
      factor =~ rcads_1n + rcads_2n + rcads_3n + rcads_4n + 
                rcads_5n + rcads_6n + rcads_7n + rcads_8n + 
                rcads_9n + rcads_10n + rcads_11n + rcads_12n +
                rcads_13n + rcads_14n + rcads_15n + rcads_16n +
                rcads_17n + rcads_18n + rcads_19n + rcads_20n + 
                rcads_21n + rcads_22n + rcads_23n + rcads_24n +
                rcads_25n'

cfa_model1 <- lavaan::cfa(model = model1,
                  data = df,
                  estimator = "WLSMV",
                  std.lv = T)


summary(cfa_model1, standardized = T, fit = T)
fitmeasures(cfa_model1, fit.measures = c("rmsea", "srmr", "cfi", "tli"))

inspect(cfa_model1, what = "std")$lambda #standardised factor loadings



labels1 <- list(rcads_1n = "Item 1", rcads_2n = "Item 2", rcads_3n = "Item 3",
               rcads_4n = "Item 4", rcads_5n = "Item 5", rcads_6n = "Item 6",
               rcads_7n = "Item 7", rcads_8n = "Item 8", rcads_9n = "Item 9",
               rcads_10n = "Item 10", rcads_11n = "Item 11", rcads_12n = "Item 12",
               rcads_13n = "Item 13", rcads_14n = "Item 14", rcads_15n = "Item 15",
               rcads_16n = "Item 16", rcads_17n = "Item 17", rcads_18n = "Item 18",
               rcads_19n = "Item 19", rcads_20n = "Item 20", rcads_21n = "Item 21",
               rcads_22n = "Item 22", rcads_23n = "Item 23", rcads_24n = "Item 24",
               rcads_25n = "Item 25")



lavaanPlot::lavaanPlot(
  model = cfa_model1,
  labels = labels1,
  coefs = TRUE,
  stand = TRUE,
  covs = TRUE,
  node_options = list(
    shape = "box",
    fontname = "Aptos",
    fontsize = 30),
  edge_options = list(
    color = "black",
    fontname = "Aptos",
    fontsize = 25  ),
  graph_options = list(
    rankdir = "LR",
    ratio = "fill"
  )
)

pacman::p_load(webshot)
webshot::install_phantomjs() # 

# Create the plot
p <- lavaanPlot::lavaanPlot(
  model = cfa_model1,
  labels = labels1,
  coefs = TRUE,
  stand = TRUE,
  covs = TRUE,
  node_options = list(
    shape = "box",
    fontname = "Aptos",
    fontsize = 30
  ),
  edge_options = list(
    color = "black",
    fontname = "Aptos",
    fontsize = 25
  ),
  graph_options = list(
    rankdir = "LR")
)

# Save as HTML and convert to image
htmlwidgets::saveWidget(p, "temp_plot.html")
webshot("temp_plot.html", 
        "C:/Users/documents/plot.png",
        vwidth = 200, 
        vheight = 500,
        zoom = 7)
file.remove("temp_plot.html")


##Two factor model
model2 <-  '
      Depression =~ rcads_1n + rcads_4n + rcads_8n + rcads_10n +
                    rcads_13n + rcads_15n + rcads_16n + rcads_19n +
                    rcads_21n + rcads_24n

      Anxiety =~ rcads_2n + rcads_3n + rcads_5n + rcads_6n +
                 rcads_7n + rcads_9n + rcads_11n + rcads_12n +
                 rcads_14n + rcads_17n + rcads_18n + rcads_20n +
                 rcads_22n + rcads_23n + rcads_25n'


cfa_model2 <- cfa(model = model2,
                  data = df,
                  estimator = "WLSMV",
                  std.lv = T,
                  orthogonal = F 
                  )

summary(cfa_model2, standardized = T, fit = T)
fitmeasures(cfa_model2, fit.measures = c("rmsea", "srmr", "cfi", "tli"))

inspect(cfa_model2, what = "std")$lambda


# Create the plot
p2 <- lavaanPlot::lavaanPlot(
  model = cfa_model2,
  labels = labels1,
  coefs = TRUE,
  stand = TRUE,
  covs = TRUE,
  node_options = list(
    shape = "box",
    fontname = "Aptos",
    fontsize = 30
  ),
  edge_options = list(
    color = "black",
    fontname = "Aptos",
    fontsize = 25
  ),
  graph_options = list(
    rankdir = "LR")
)

# Save as HTML and convert to image
htmlwidgets::saveWidget(p2, "temp_plot2.html")
webshot("temp_plot2.html", 
        "C:/Users/documents/cfa_plot2.png",
        vwidth = 200, 
        vheight = 500,
        zoom = 7)
file.remove("temp_plot2.html")
 #2-factor model fits slightly better than 1-factor model and is supported by literature


##Invariance analysis------------------------------------------------------
###One factor model
####Configural invariance - Gender
cfa_fit_conf_gn_1 <- cfa(model = model1, 
                    data = df,
                    estimator = "WLSMV",
                    std.lv = T,
                    orthogonal = F,
                    parameterization = "theta",
                    group = "csex")  
fitmeasures(cfa_fit_conf_gn_1, fit.measures = c("rmsea", "srmr", "cfi", "tli"))

#####Metric invariance - Gender
cfa_fit_mtr_gn_1 <- cfa(model = model1, 
                   data = df,
                   estimator = "WLSMV",
                   std.lv = T,
                   orthogonal = F,
                   parameterization = "theta",
                   group = "csex",
                   group.equal = "loadings")  
fitmeasures(cfa_fit_mtr_gn_1, fit.measures = c("rmsea", "srmr", "cfi", "tli"))  

####Scalar- Gender
cfa_fit_scl_gn_1 <- cfa(model = model1, 
                   data = df,
                   estimator = "WLSMV",
                   std.lv = T,
                   orthogonal = F,
                   parameterization = "theta",
                   group = "csex",
                   group.equal = c("loadings", "thresholds")) 
fitmeasures(cfa_fit_scl_gn_1, fit.measures = c("rmsea", "srmr", "cfi", "tli"))


####Configural invariance - Age
cfa_fit_conf_age_1 <- cfa(model = model1, 
                        data = df,
                        estimator = "WLSMV",
                        std.lv = T,
                        orthogonal = F,
                        parameterization = "theta",
                        group = "cage_cat")  
fitmeasures(cfa_fit_conf_age_1, fit.measures = c("rmsea", "srmr", "cfi", "tli"))

####Metric invariance - Age
cfa_fit_mtr_age_1 <- cfa(model = model1, 
                       data = df,
                       estimator = "WLSMV",
                       std.lv = T,
                       orthogonal = F,
                       parameterization = "theta",
                       group = "cage_cat",
                       group.equal = "loadings")  
fitmeasures(cfa_fit_mtr_age_1, fit.measures = c("rmsea", "srmr", "cfi", "tli"))  

####Scalar - Age
cfa_fit_scl_age_1 <- cfa(model = model1, 
                       data = df,
                       estimator = "WLSMV",
                       std.lv = T,
                       orthogonal = F,
                       parameterization = "theta",
                       group = "cage_cat",
                       group.equal = c("loadings", "thresholds")) 
fitmeasures(cfa_fit_scl_age_1, fit.measures = c("rmsea", "srmr", "cfi", "tli"))





###Two factor model
####Configural invariance - Gender
cfa_fit_conf <- cfa(model = model2, 
                    data = df,
                    estimator = "WLSMV",
                    std.lv = T,
                    orthogonal = F,
                    parameterization = "theta",
                    group = "csex")  
fitmeasures(cfa_fit_conf, fit.measures = c("rmsea", "srmr", "cfi", "tli"))

####Metric invariance - Gender
cfa_fit_mtr <- cfa(model = model2, 
                   data = df,
                   estimator = "WLSMV",
                   std.lv = T,
                   orthogonal = F,
                   parameterization = "theta",
                   group = "csex",
                   group.equal = "loadings")  
fitmeasures(cfa_fit_mtr, fit.measures = c("rmsea", "srmr", "cfi", "tli"))  

####Scalar- Gender
cfa_fit_scl <- cfa(model = model2, 
                   data = df,
                   estimator = "WLSMV",
                   std.lv = T,
                   orthogonal = F,
                   parameterization = "theta",
                   group = "csex",
                   group.equal = c("loadings", "thresholds")) 
fitmeasures(cfa_fit_scl, fit.measures = c("rmsea", "srmr", "cfi", "tli"))


####Configural invariance - Age
cfa_fit_conf_age <- cfa(model = model2, 
                    data = df,
                    estimator = "WLSMV",
                    std.lv = T,
                    orthogonal = F,
                    parameterization = "theta",
                    group = "cage_cat")  
fitmeasures(cfa_fit_conf_age, fit.measures = c("rmsea", "srmr", "cfi", "tli"))

####Metric invariance - Age
cfa_fit_mtr_age <- cfa(model = model2, 
                   data = df,
                   estimator = "WLSMV",
                   std.lv = T,
                   orthogonal = F,
                   parameterization = "theta",
                   group = "cage_cat",
                   group.equal = "loadings")  
fitmeasures(cfa_fit_mtr_age, fit.measures = c("rmsea", "srmr", "cfi", "tli"))  

####Scalar - Age
cfa_fit_scl_age <- cfa(model = model2, 
                   data = df,
                   estimator = "WLSMV",
                   std.lv = T,
                   orthogonal = F,
                   parameterization = "theta",
                   group = "cage_cat",
                   group.equal = c("loadings", "thresholds")) 
fitmeasures(cfa_fit_scl_age, fit.measures = c("rmsea", "srmr", "cfi", "tli"))

# --------------------------------------------------------------
df$phq_score <- rowSums(df[, c("phq1", "phq2", "phq3", "phq4", "phq5",
                               "phq6", "phq7", "phq8", "phq9")])

psych::alpha(df[, c("phq1", "phq2", "phq3", "phq4", "phq5",
                    "phq6", "phq7", "phq8", "phq9")])

skim(df$phq_score)


df <- df %>% 
  mutate(
    dep = 
      case_when(
        phq_score < 10 ~ "No",
        phq_score >= 10 ~ "Yes",
        TRUE ~ NA))


tab1(df$dep, graph = F)
binom.test(96, 1163)


df$gad_score <- rowSums(df[, c("gad1", "gad2", "gad3", "gad4", "gad5",
                               "gad6", "gad7")])

psych::alpha(df[, c("gad1", "gad2", "gad3", "gad4", "gad5",
       "gad6", "gad7")])

skim(df$gad_score)


df <- df %>% 
  mutate(
    anx = 
      case_when(
        gad_score < 10 ~ "No",
        gad_score >= 10 ~ "Yes",
        TRUE ~ NA))

tab1(df$anx, graph = F)
binom.test(13, (1163-7))












