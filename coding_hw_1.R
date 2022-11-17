birthweight <- read.csv('/Users/gavmross/Downloads/birthweight.csv')
population <- read.csv('/Users/gavmross/Downloads/cps2012v2.csv')

#Remove birthweights that do not have known mother smoking status
bwt_clean = birthweight[birthweight$smoke != 9,  ]
#Investigate relationship of birthweight and smoking status
cor.test(bwt_clean$bwt, bwt_clean$smoke)

#Dataframe of non smokers
n_smoke = bwt_clean[bwt_clean$smoke == 0,]
#Dataframe of smokers
y_smoke = bwt_clean[bwt_clean$smoke == 1,]

#Number of non smokers 
n_no_smoke = nrow(n_smoke)
#Number of smokers 
n_yes_smoke = nrow(y_smoke)

#Avg weight of non smoker
mean_no_smoke_wt = mean(n_smoke$bwt)
#Avg weight of smoker 
mean_yes_smoke_wt = mean(y_smoke$bwt)

#STANDARD ERROR OF MEANS
#create function to find se
stderror <- function(x) sd(x)/sqrt(length(x))
sd_no = stderror(n_smoke$bwt)
sd_yes = stderror(y_smoke$bwt)

#95% CI FOR MEANS
#1 sample t test for non smokers bwt
t.test(n_smoke$bwt)
#1 sample t test for smokers bwt
t.test(y_smoke$bwt)
#DIFFERENCE OF MEANS
mean_diff = mean_no_smoke_wt - mean_yes_smoke_wt
#Two sample t test to find CI for difference in means
t.test(n_smoke$bwt, y_smoke$bwt)
