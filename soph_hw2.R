# =======================LIBRARIES===============================
library(lmtest)
library(sandwich)
library(car)
# =======================IMPORT AND CLEAN DATA===============================
# Specify header argument to False to read in file with no errors
df <- read.csv('SCHealth1000_hw.csv', header=FALSE)

# Set third row as column names
names(df) <- df[2,]   # Make second row column names
df <- df[-1:-2,]   # Remove first two rows
# Look at variable names
names(df)
options(scipen = 999)
# Look at datatype each column 
sapply(df, class)
# Function to turn col into num 
to_num <- function(col) {
  col <- as.numeric(col)
  return(col)
}
# Change datatypes of necessary variables 
df$age <- to_num(df$age)
df$sex <- to_num(df$sex)
df$lowincome <- to_num(df$lowincome)
df$middleincome <- to_num(df$middleincome)
df$highincome <- to_num(df$highincome)
df$married <- to_num(df$married)
df$height <- to_num(df$height)
df$race <- to_num(df$race)
df$educ <- to_num(df$educ)
# Create new variables 
df$agesq <- df$age * df$age
df$sex_dum <- ifelse(df$sex == 2, 1, 0) # If sex = 2 (female), make it =1, else = 0 (male)

# =======================FIRST REGRESSION===============================
# We can only use 2/3 income levels to avoid dummy variable trap (lowincome is reference)
# Use sex_dum instead of sex
reg <- lm(df$weight ~ df$age + df$agesq + df$sex_dum + df$middleincome + df$highincome + df$married + df$height, data=df)
summary(reg)

# heteroskedastic robust standard errors 
coeftest(reg, vcov = vcovHC(reg, type = "HC1"))
# =======================SECOND REGRESSION===============================
# Add race and education level 
# Race is 1-5. We use k-1 dummy variables (so 4 dummy variables). race=1 (white) is reference
df$race_white <- ifelse(df$race == 1, 1, 0) # REFERENCE 
df$race_black <- ifelse(df$race == 2, 1, 0)
df$other_race <- ifelse(df$race == 3, 1, 0)
df$multi_race <- ifelse(df$race == 4, 1, 0)
df$race_hispanic <- ifelse(df$race == 5, 1, 0)
# Educ is 1-6, so we use 5 dummy variables. educ=1 (never attend school) is reference
check_educ <- table(df$educ)
check_educ     # No education (educ =1) has 0 observations, do not include it in analysis
df$no_educ <- ifelse(df$educ == 1, 1, 0)
df$educ_one_to_eight <- ifelse(df$educ == 2, 1, 0)   # REFERENCE
df$educ_nine_to_eleven <- ifelse(df$educ == 3, 1, 0)
df$educ_grad_highschool <- ifelse(df$educ == 4, 1, 0)
df$educ_some_college <- ifelse(df$educ == 5, 1, 0)
df$educ_grad_college_plus <- ifelse(df$educ == 6, 1, 0)
# Second regression 
reg2 <- lm(df$weight ~ df$age + df$agesq + df$sex_dum + df$middleincome + df$highincome + df$married + df$height + 
             df$race_black + df$other_race + df$multi_race + df$race_hispanic +
             df$educ_nine_to_eleven + df$educ_grad_highschool + df$educ_some_college +
             df$educ_grad_college_plus, data = df) 
summary(reg2)

# heteroskedastic robust standard errors 
coeftest(reg2, vcov = vcovHC(reg2, type = "HC1"))

# =======================THIRD REGRESSION===============================
# do not include height
reg3 <- lm(df$weight ~ df$age + df$agesq + df$sex_dum + df$middleincome + df$highincome + df$married +
             df$race_black + df$other_race + df$multi_race + df$race_hispanic + 
             df$educ_nine_to_eleven + df$educ_grad_highschool + df$educ_some_college + 
             df$educ_grad_college_plus, data = df)
summary(reg3)

# heteroskedastic robust standard errors 
coeftest(reg3, vcov = vcovHC(reg3, type = "HC1"))

# =======================PART 2===============================

# drinks per day ~ veteran

# Does being a veteran impact average drinks per day? An inquiry to see how 
# possible PTSD can affect alcohol consumption
df$avgDrinksPerDay <- to_num(df$avgDrinksPerDay)
df$veteran <- to_num(df$veteran)
reg4 <- lm(df$avgDrinksPerDay ~ df$veteran, data = df)
summary(reg4)

# heteroskedastic robust standard errors 
coeftest(reg4, vcov = vcovHC(reg4, type = "HC1"))

# Variable that could cause OBV: insured 
# Reasoning: being insured and drinking is an example of moral hazard, a possible correlation
# with Y. Veterans received massive health benefits/insurance, so it is plausible that
# veterans and insured are correlated

df$insured <- to_num(df$insured)

cor(df$insured, df$veteran)   # Check for multicollinearity

reg5 <- lm(df$avgDrinksPerDay ~ df$veteran + df$insured, data = df)
summary(reg5)

# heteroskedastic robust standard errors 
coeftest(reg5, vcov = vcovHC(reg5, type = "HC1"))

t.test(df$middleincome, df$highincome)

