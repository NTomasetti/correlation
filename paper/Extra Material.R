#Residuals#

extreme <- subset(glmm.resid, residual <= -10 | residual >= 10)
extreme <- merge(extreme, full.sub[c(9,11)], by=0)

##sources of extreme residuals:
#multiple selections - one was correct, others are predicted to be correct but aren't
#only person to get an easy one wrong
#only person to get a hard one right
#investigate pic_id = 61 name = s459.... => true plot in 18 looks alright, but most people picked plot 17 with cor = 0.24. 17 looks very good
#if you ignore the outliers. Two chose reason 4, one chose reason 1. Last person selected 7 & 20, neither plot is especially interesting.

#Subject evaluations#
subjplot <- summarise(group_by(full.sub, subj_id), eval=sum(weights*attempts), attempt = sum(attempts), detect = sum(correct))
subjplot <- mutate(subjplot, byeval = detect/eval, byattempt = detect/attempt)
#one subject has no successful evaluations: Saw ten lineups and made ten selections. Harder lineups: low correl scatter, negative lines.
#did see 0.7 scatter and two 0.9 lines which should be reasonable chance to get correct (pic_id = 181, 140, 44)
#was only person to get 0.7 scatter wrong (sd4V..., true = 12, chose 1, reason 1), but 0.7 is not high enough to get really large residuals.
#11/13 detected line 140 (levq...., true = 4, chose 17, reason 2, this one is really easy) and 4/7 deteced in line 44. (lYbEZ, picked 19, true = 3, reason=2)
#subjects with one successful detection tended to have negative lines and low correlation scatters
ggplot(data=subjplot, aes(x=byattempt)) + geom_histogram(binwidth=0.05) + ylim(0,30) #proportion: detection to attempts
#mean 0.37
ggplot(data=subjplot, aes(x=byeval)) + geom_histogram(binwidth=0.05) + ylim(0,30) # proportion: detection to lineup evals.
#mean 0.40