##########################
##  Final Term Project  ##
##   DA 2 & Coding 1    ##
##       Used Bikes     ##
##          CEU         ##
##########################

library(tidyverse)
library(haven)
library(data.table)
library(rms)
library(lspline)
library(huxtable)
library(modelsummary)
library(pscl)
library(mfx)
library(kableExtra)
library(lspline)
library(reshape2)
library(dplR)
library(fixest)
library(dplyr)




# Main Question?
## To understand the relationship between a post, that is s photo, 
## resulting with more interactions as a result of
## Total Reach for that post. With confounding variables 
## (1) if the post was paid (2) Time of the day it was posted at
## (3) Type of Post


### Importing data from github repo:

raw_data <- read_csv(url("https://raw.githubusercontent.com/alisial94/DA2-Coding1-Final-Term-Project/main/Data/dataset_Facebook.csv")) 

head( raw_data )
view(raw_data)

### Transforming the into columns since it was downloaded as CSV (20 columns were made in total).
data <- separate(data = raw_data, col = "Page total likes;Type;Category;Post Month;Post Weekday;Post Hour;Paid;Lifetime Post Total Reach;Lifetime Post Total Impressions;Lifetime Engaged Users;Lifetime Post Consumers;Lifetime Post Consumptions;Lifetime Post Impressions by people who have liked your Page;Lifetime Post reach by people who like your Page;Lifetime People who have liked your Page and engaged with your post;comment;like;share;Total Interactions", 
         into = c("Page_total_likes", "Type", "Category", "Post_Month", "Post_Weekday" ,
                  "Post_Hour" , "Paid" , "Total_Reach" , "Lifetime_Post_Total_Impressions" ,
                  "Lifetime_Engaged_Users", "Lifetime_Post_Consumer" ,
                  "Lifetime_Post_Consumptions" , "Lifetime_Post_Impressions_by_people_who_have_liked_your_Page" ,
                  "Lifetime_Post_reach_by_people_who_like_your_Page" , "Lifetime_People_who_have_liked_your_Page_and_engaged_with_your_post" ,
                  "comment" , "like" , "share" , "Total_Interactions"), sep = ";")
view(data)


### Removing columns that are not needed for the analysis
data_clean<- data[, -c(1,3,4,5,9,10,11,12,13,14,15,16,17,18)]

##data_clean <- select( data, -Page_total_likes , -Post_Month , -Post_Weekday , -Lifetime_Post_Total_Impressions ,  -Lifetime_Engaged_Users , -Lifetime_Post_Consumer,
     ##                -Lifetime_Post_Consumptions, -Lifetime_Post_Impressions_by_people_who_have_liked_your_Page,
       ##              -Lifetime_Post_reach_by_people_who_like_your_Page, -Lifetime_People_who_have_liked_your_Page_and_engaged_with_your_post ,
         ##            -comment , -like, -share )

view(data_clean)


### Fixing the column type
data_clean$Post_Hour <- as.numeric(data_clean$Post_Hour)
data_clean$Paid <-as.numeric(data_clean$Paid)
data_clean$Total_Reach <- as.numeric(data_clean$Total_Reach)
data_clean$Total_Interactions <- as.numeric(data_clean$Total_Interactions)


# Summary statistics on price
P95 <- function(x){ quantile(x,.95,na.rm=T)}
P05 <- function(x){quantile(x,0.05,na.rm=T)}
datasummary( Total_Interactions + Total_Reach + Post_Hour ~ mean + SD + Min + Max + Median + P95 + P05 + N , data = data_clean )
datasummary_skim(data_clean)

### Filtering the data for Total Interactions less than 2000 and total reach less than 80,000 (removed 19 observations)

data_clean <- data_clean %>% filter(Total_Interactions > 0 & Total_Interactions <= 2000) %>% 
  filter(Total_Reach <= 80000) 

data_clean <- data_clean %>% filter(!is.na(data_clean$Paid))


## Taking LOG for Total Interactions and Total Reach

### Checking for Total Interactions
ggplot(data=data_clean) +
  geom_histogram(aes(x=Total_Interactions))
#### right skewed thus log
data_clean$ln_Total_Interactions <- log(data_clean$Total_Interactions)

### Checking for Total Reach
ggplot(data=data_clean) +
  geom_histogram(aes(x=Total_Reach))
#### right skewed thus log
data_clean$ln_Total_Reach <- log(data_clean$Total_Reach)

### checking the data for normal distribution after logs
datasummary_skim(data_clean)


### Creating binary variables for confounders
data_clean$Photo <- ifelse(data_clean$Type == "Photo",1,0)
data_clean$Status <- ifelse(data_clean$Type == "Status",1,0)
data_clean$Link <- ifelse(data_clean$Type == "Link",1,0)
data_clean$Video <- ifelse(data_clean$Type =="Video",1,0)



### Correlation Matrix

numeric_df <- keep( data_clean , is.numeric )

cT <- round( cor( numeric_df , use = "complete.obs") , 2 )
# create a lower triangular matrix
cT[ upper.tri( cT ) ] <- NA
# Put it into a tibble format
melted_cormat <- melt( cT , na.rm = TRUE)
# Now we can create a heat-map
cor_matrix <- ggplot( data = melted_cormat, aes( Var2 , Var1 , fill = value ) )+
  geom_tile( color = "white" ) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_bw()+
  theme( axis.text.x = element_text(angle = 45, vjust = 1,
                                    size = 10, hjust = 1))+
  labs(y="",x="")+
  coord_fixed()
cor_matrix



# LOWESS NONPARAMETRIC REGRESSION


### checking for if any of the variable that's numeric that require spline and results
### that we don't need spline since there wasn't significant change in the curve of best fit 

ggplot(data = data_clean)  +
  geom_point( aes( x = ln_Total_Reach, y = ln_Total_Interactions )) +
geom_smooth( aes( x = ln_Total_Reach , y = ln_Total_Interactions ) , 
             method = 'loess' , formula = 'y ~ x' )

ggplot(data = data_clean)  +
  geom_point( aes( x = Post_Hour, y = ln_Total_Interactions )) +
  geom_smooth( aes( x = Post_Hour , y = ln_Total_Interactions ) , 
               method = 'loess' , formula = 'y ~ x' )

### Correlation Matrix

numeric_df <- keep( data_clean , is.numeric )

cT <- round( cor( numeric_df , use = "complete.obs") , 2 )
# create a lower triangular matrix
cT[ upper.tri( cT ) ] <- NA
# Put it into a tibble format
melted_cormat <- melt( cT , na.rm = TRUE)
# Now we can create a heat-map
cor_matrix <- ggplot( data = melted_cormat, aes( Var2 , Var1 , fill = value ) )+
  geom_tile( color = "white" ) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_bw()+
  theme( axis.text.x = element_text(angle = 45, vjust = 1,
                                    size = 10, hjust = 1))+
  labs(y="",x="")+
  coord_fixed()
cor_matrix


### checking for leaner relationship between y and x variable and since we have one we would use
### feols for regressions
ggplot(data = data_clean)  +
  geom_point( aes( x = ln_Total_Reach, y = ln_Total_Interactions )) +
  geom_smooth( aes( x = ln_Total_Reach , y = ln_Total_Interactions ) , 
               method = 'lm' , formula = 'y ~ x' )


# Regressions (feols with robust SE)

reg1 <- feols( ln_Total_Interactions ~ ln_Total_Reach, data=data_clean, vcov = 'hetero' )
summary( reg1 )

reg2 <- feols( ln_Total_Interactions ~ ln_Total_Reach + Paid + ln_Total_Reach*Paid, data=data_clean, vcov = 'hetero' )
summary( reg2 )

reg3 <- feols( ln_Total_Interactions ~ ln_Total_Reach + Paid + ln_Total_Reach*Paid +
               Post_Hour + ln_Total_Reach*Post_Hour, data=data_clean, vcov = 'hetero' )
summary( reg3 )

reg4 <- feols(ln_Total_Interactions ~ ln_Total_Reach + Paid + ln_Total_Reach*Paid +
                Post_Hour + ln_Total_Reach*Post_Hour + Video + Photo + Status
              , data=data_clean, vcov = 'hetero' )
summary( reg4 )


## Summarize our findings:
varname_report <- c("(Intercept)" = "Intercept",
                    "stratio" = "student/teacher",
                    "lspline(stratio,18)1" = "student/teacher (<18)",
                    "lspline(stratio,18)2" = "student/teacher (>=18)",
                    "english_d" = "english_dummy")
groupConf <- list("English" = c("english"),
                  "Lunch" = c("lunch"),
                  "Other Special" = c("special"),
                  "Wealth Measures" = c("exptot","income","scratio"))
vars_omit <- c("english|lunch|special|salary|exptot|income|scratio")
# Note: coefstat = 'confint' is just an example, usually you need to report se.
style_noHeaders = style.tex(var.title = "", fixef.title = "", stats.title = " ")
kable( etable( reg1 , reg2 , reg3 , reg4 , reg5 ,
               title = 'Average test scores for 4th graders',
               dict = varname_report,
               drop = vars_omit ,
               group = groupConf ,
               se.below = T,
               coefstat = 'se',
               fitstat = c('n','r2'),
               se.row = F,
               depvar = F ) , 
       col.names = c('(1)','(2)','(3)','(4)','(5)'),
       "latex", booktabs = TRUE,  position = "H",
       caption = 'Models to uncover relation between test score and student to teacher ratio') %>% kable_styling(latex_options = c("hold_position","scale_down"))



# Summarize our findings:
varname_report <- c("(Intercept)" = "Intercept",
                    "ln_Total_Reach" = "Total Reach",
                    "Paid Post" = "Paid",
                    "ln_Total_Reach*Paid" = "Total Reach x Paid Post",
                    "Post_Hour" = "Time of the Day",
                    "ln_Total_Reach*Post_Hour" = "Total Reach x Time of the Day",
                    "Video" = "Type: Video",
                    "Photo" = "Type: Photo",
                    "Status" = "Type: Status")
groupConf <- list("English" = c("english"),
                  "Lunch" = c("lunch"),
                  "Other Special" = c("special"),
                  "Wealth Measures" = c("exptot","income","scratio"))
vars_omit <- c("english|lunch|special|salary|exptot|income|scratio")
# Note: coefstat = 'confint' is just an example, usually you need to report se.
style_noHeaders = style.tex(var.title = "", fixef.title = "", stats.title = " ")
kable( etable( reg1 , reg2 , reg3 , reg4 , reg5 ,
               title = 'Average test scores for 4th graders',
               dict = varname_report,
               drop = vars_omit ,
               group = groupConf ,
               se.below = T,
               coefstat = 'se',
               fitstat = c('n','r2'),
               se.row = F,
               depvar = F ) , 
       col.names = c('(1)','(2)','(3)','(4)','(5)'),
       "latex", booktabs = TRUE,  position = "H",
       caption = 'Models to uncover relation between test score and student to teacher ratio')%>% 
  kable_styling(latex_options = c("hold_position","scale_down"))

a<- etable(reg1, reg2, reg3, reg4)


summarry_reg <- msummary(list(reg1 , reg2 , reg3 , reg4),
                         fmt="%.0f",
                         gof_omit = 'DF|Deviance|Log.Lik.|F|AIC|BIC|R2|PseudoR2|R2 Adj.|Std.Errors',
                         stars=c('*' = .05, '**' = .01),
                         coef_rename = c("(Intercept)" = "Intercept",
                                         "ln_Total_Reach" = "Total Reach",
                                         "Paid Post" = "Paid",
                                         "ln_Total_Reach*Paid" = "Total Reach x Paid Post",
                                         "Post_Hour" = "Time of the Day",
                                         "ln_Total_Reach*Post_Hour" = "Total Reach x Time of the Day",
                                         "Video" = "Type: Video",
                                         "Photo" = "Type: Photo",
                                         "Status" = "Type: Status",
                                         "Num.Obs."="Observation"),
                         title = "Regression Model Summary") %>% 
  kableExtra::kable_styling(latex_options = "hold_position")

summary_reg
