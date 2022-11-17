birthweight <- read.csv('/Users/gavmross/Downloads/birthweight.csv')
population <- read.csv('/Users/gavmross/Downloads/cps2012v2.csv')

#select only rows that do not have 9 (unkown) in the smoke column
bwt_clean = birthweight[birthweight$smoke != 9,  ]
#find correlation and p-value 
cor.test(bwt_clean$bwt, bwt_clean$smoke)
#mothers who do not smoke 
n_smoke = bwt_clean[bwt_clean$smoke == 0,]
#mothers who did smoke 
y_smoke = bwt_clean[bwt_clean$smoke == 1,]
#number of mothers who did not smoke
n_smoke_count = nrow(n_smoke)
#number of mothers who did smoke
y_smoke_count = nrow(y_smoke)
#average weight of non smokers
avg_n_smoke = mean(n_smoke$bwt)
#average weight of smokers
avg_y_smoke = mean(y_smoke$bwt)
#find stander error of the means. Write function that does so
stderror <- function(x) sd(x)/sqrt(length(x))
sd_n = stderror(n_smoke$bwt)
sd_y = stderror(y_smoke$bwt)

#95% CI OF MEANS
moe_n = qt(0.975, df=n_smoke_count-1)*sd_n/sqrt(n_smoke_count)
n_lower = avg_n_smoke - moe_n
n_upper = avg_n_smoke + moe_n

moe_y = qt(0.975, df=y_smoke_count-1)*sd_y/sqrt(y_smoke_count)
y_upper = avg_y_smoke + moe_y
y_lower = avg_y_smoke - moe_y
#DIFFERENCE IN MEANS 
mean_diff = avg_n_smoke - avg_y_smoke
#95% CI FOR DIFFERENCE IN MEANS
#find pooled variance of data sets
pv = ((n_smoke_count-1)*sd_n^2+(y_smoke_count-1)*sd_y^2)/(n_smoke_count+y_smoke_count-2)
#find moe for differnece in means CI
moe_diff = qt(0.975,df=n_smoke_count+y_smoke_count-1)*sqrt(pv/n_smoke_count + pv/y_smoke_count)
#find upper and lower bounds 
diff_lower = (avg_n_smoke - avg_y_smoke) - moe_diff
diff_upper = (avg_n_smoke - avg_y_smoke) + moe_diff


qt(p=.05, df=n_smoke_count-1, lower.tail=FALSE)
qt(p=.05/2, df=22, lower.tail=FALSE)
var.test(bwt_clean$bwt ~ bwt_clean$smoke)
t.test(n_smoke$bwt)
t.test(n_smoke$bwt, y_smoke$bwt)


