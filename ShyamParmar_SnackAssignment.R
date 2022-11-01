###############################
#                             #
#            Snack            #
#                             #
###############################

###############################
# Load libraries and import dataset

rm(list=ls())

# install.packages("pacman")
pacman::p_load(dplyr, tidyr, caret, ggplot2, caTools, MLmetrics, mlbench, mlTools, corrplot, 
               expss, PerformanceAnalytics, AER, MASS, stargazer, pscl, jtools, Hmisc, ggcorrplot, 
               rpart, rpart.plot, readxl, ROCR, lme4)

###############################
# Load the data sets

# Stores data
df_stores <- read_excel("C:/Users/Scott/Downloads/SnackChain.xlsx", sheet = 'stores')

# products data
df_products <- read_excel("C:/Users/Scott/Downloads/SnackChain.xlsx", sheet = 'products')

# transaction data
df_transactions <- read_excel("C:/Users/Scott/Downloads/SnackChain.xlsx", sheet = 'transactions')

##############################
# Merge and data cleaning

# drop oral hygiene products from the dataframe
df_products <- subset(df_products, CATEGORY != 'ORAL HYGIENE PRODUCTS')

# rename the store column to make it consistent across all tables
names(df_transactions)[names(df_transactions) == 'STORE_NUM'] <- 'STORE_ID'

# merge the product and transaction tables 
df <- merge(df_products, df_transactions, by="UPC")

# merge the merged and store tables
df <- merge(df, df_stores, by="STORE_ID")

# Drop na values
df <- df[complete.cases(df$PRICE), ] # Drop incomplete rows

str(df)

df$year <- format(df$WEEK_END_DATE, format="%Y")                                   # Get the Year
df$month <- format(df$WEEK_END_DATE, format="%B")                                  # Get the months
df$week <- format(df$WEEK_END_DATE, format="%W")                                   # Get the week

# Drop date column
df <- subset(df, select = -WEEK_END_DATE)

# Convert all non valued values to NA
df[is.na(df) | df=="Inf" | df == "-Inf"] = NA

# Create a log of spend column
df$LOGSPEND <- log(df$SPEND)

# Create a price elastic column
df$PRICE_ELASTIC <- df$UNITS/df$PRICE

# Drop the -inf value from the logspend column
df <- subset(df, LOGSPEND != "-Inf")

# Convert columns to factors
cols_fac <- c("STORE_ID", "UPC", "DESCRIPTION", "MANUFACTURER", "CATEGORY", "SUB_CATEGORY", "PRODUCT_SIZE", 
              "STORE_NAME", "FEATURE", "DISPLAY", "TPR_ONLY", "CITY", "STATE", "SEGMENT", "week")
df[cols_fac] <- (lapply(df[cols_fac], factor))

# Convert year and month to factors with controlled levels 
df$year <- factor(df$year, levels = c("2009", "2010", "2011"))

df$month <- factor(df$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

###############################
# Visuals
chart.Correlation(df[as.numeric(which(sapply(df,class)=="numeric"))]) # Plot for numeric variables

hist(df$SPEND)
hist(log(df$SPEND))

hist(df$UNITS)
hist(log(df$UNITS))

hist(df$HHS)
hist(log(df$HHS))

hist(df$PRICE_ELASTIC)

table(df$DISPLAY)

table(df$FEATURE)

table(df$TPR_ONLY)

bwplot(SPEND ~ DISPLAY, data=df)

##############################
# Models

# Q1
### SPEND
spend_m <- lm(LOGSPEND ~ DISPLAY + FEATURE + TPR_ONLY + PRODUCT_SIZE + UNITS + VISITS + STORE_NAME, 
              data = df) 
summary(spend_m)

spend_m2 <- lmer(LOGSPEND ~ DISPLAY + FEATURE + TPR_ONLY + HHS + PRODUCT_SIZE + UNITS + VISITS + (1 | STORE_NAME) + (1 | CITY), 
           data=df, REML=FALSE)
summary(spend_m2)
fixef(spend_m2)                                       # Magnitude of fixed effects
ranef(spend_m2)                                       # Magnitude of random effects
coef(spend_m2)                                        # Magnitude of total effects
vif(spend_m2)                                         # VIF test

### UNITS
units_m <- glm(UNITS ~ DISPLAY + FEATURE + TPR_ONLY + PRODUCT_SIZE + SPEND + VISITS, 
               data = df, family=poisson (link=log))
summary(units_m)

units_m1 <- lmer(UNITS ~ DISPLAY + FEATURE + TPR_ONLY + HHS + PRODUCT_SIZE + SPEND + VISITS + (1 | STORE_NAME) + (1 | CITY), 
               data = df, REML = FALSE)
summary(units_m1)
fixef(units_m1)                                       # Magnitude of fixed effects
ranef(units_m1)                                       # Magnitude of random effects
coef(units_m1)                                        # Magnitude of total effects
vif(units_m1)                                         # VIF test

### HHS
hhs_m <- glm(HHS ~ DISPLAY + FEATURE + TPR_ONLY + PRODUCT_SIZE + SPEND + VISITS, 
             data = df, family=poisson (link=log))
summary(hhs_m)

hhs_m1 <- lmer(HHS ~ DISPLAY + FEATURE + TPR_ONLY + UNITS + PRODUCT_SIZE + SPEND + VISITS + (1 | STORE_NAME) + (1 | CITY),  
             data = df,  REML = FALSE)
summary(hhs_m1)
fixef(hhs_m1)                                       # Magnitude of fixed effects
ranef(hhs_m1)                                       # Magnitude of random effects
coef(hhs_m1)                                        # Magnitude of total effects
vif(hhs_m1)                                         # VIF test

# Stargazer for Q1
stargazer(spend_m2, units_m1, hhs_m1, type="text", single.row=TRUE, digits = 3, out = "table1.html")
AIC(spend_m2, units_m1, hhs_m1)

#### 
# Q2
question2_m <- lmer(LOGSPEND ~ DISPLAY + FEATURE + TPR_ONLY +  PRODUCT_SIZE + UNITS + VISITS + (1 | CATEGORY) + (1 | SEGMENT), 
              data = df, REML = FALSE) 
summary(question2_m)
fixef(question2_m)                                       # Magnitude of fixed effects
ranef(question2_m)                                       # Magnitude of random effects
coef(question2_m)                                        # Magnitude of total effects
vif(question2_m)                                         # VIF test

####
# Q3
price_el_m <- lm(log(PRICE_ELASTIC) ~ DESCRIPTION, data = df )
summary(price_el_m)

question3_m <- lmer(log(PRICE_ELASTIC) ~ DESCRIPTION + DISPLAY + FEATURE + TPR_ONLY +  PRODUCT_SIZE + UNITS + VISITS + (1 | CATEGORY) + (1 | SEGMENT),
                    data = df, REML = FALSE)
summary(question3_m)
fixef(question3_m)                                       # Magnitude of fixed effects
ranef(question3_m)                                       # Magnitude of random effects
coef(question3_m)                                        # Magnitude of total effects
vif(question3_m)                                         # VIF test

####
# Q4
price_m <- lm(log(PRICE) ~ UNITS + DESCRIPTION, data = df)
summary(price_m)

question4_m <- lmer(log(PRICE) ~ SPEND + UNITS + DESCRIPTION + DISPLAY + FEATURE + PRODUCT_SIZE + VISITS + (1 | STORE_NAME) + (1 | CITY), 
                 data = df, REML = FALSE)
summary(question4_m)
fixef(question4_m)                                       # Magnitude of fixed effects
ranef(question4_m)                                       # Magnitude of random effects
coef(question4_m)                                        # Magnitude of total effects
vif(question4_m)                                         # VIF test

################################
# Stargazer
stargazer(question2_m, question3_m, question4_m, type="text", single.row=TRUE, digits = 3, out = "table2.html")
AIC(question2_m, question3_m, question4_m)


