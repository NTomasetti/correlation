#Residuals#

extreme <- subset(glmm.resid, residual <= -10 | residual >= 10)
extreme <- merge(extreme, full.sub[c(9,11)], by=0)

##sources of extreme residuals:
#multiple selections - one was correct, others are predicted to be correct but aren't
#only person to get an easy one wrong
#only person to get a hard one right
#investigate pic_id = 61 name = s459.... => true plot in 18 looks alright, but most people picked plot 17 with cor = 0.24. 17 looks very good
#if you ignore the outliers. Two chose reason 4, one chose reason 1. Last person selected 7 & 20, neither plot is especially interesting.





