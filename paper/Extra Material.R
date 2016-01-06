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

#Choice plots#
full.subs$choice_reason <-sapply(strsplit(as.character(unlist(full.subs$choice_reason)), "[^0-9]+"), "[[", 1)
ggplot(data=full.subs) + geom_bar(aes(x=choice_reason))
choice <- full.subs %>% transform(choice_reason= strsplit(as.character(choice_reason), "")) %>% unnest(choice_reason)
choice$rsgn <- factor(choice$rsgn, labels=c("r -","r +"))
#tm <- summarise(group_by(time, correct, plot, rsgn), m=mean(time))
c1 <- ggplot(data=subset(choice, correct==0), aes(x=choice_reason)) + geom_bar() + 
  facet_grid(plot~rsgn) + labs(title="Did not detect", x=element_blank(), y=element_blank())  
c2 <- ggplot(data=subset(choice, correct==1), aes(x=choice_reason)) + geom_bar() + 
  facet_grid(plot~rsgn) + labs(title="Detected", x=element_blank, y=element_blank())  
grid.arrange(c1, c2, ncol=2, top = "Choice by plot design, true plot correlation and detection")

#Multiple selections#
m.select <- summarise(group_by(subset(full.sub, attempts>1), subj_id, pic_id), attempts=max(attempts))
##112 subject/lineup combinations
##53 different subjects chose multiple plots
##84 different lineups had multiple selections, 61 with 1, 19 with 2, 3 with 2, 1 with  
##55 lines, 29 scatter (1 person selecting multiple - 40 lines, 21 scatter
#                       2 people selecting multiple - 11 lines, 8 scatter
#                       3 people selecting multiple - 3 lines
#                       4 people selecting multiple - 1 line
)

m.freq <- data.frame(table(m.select$pic_id))
m.lineup <- merge(m.freq, Turk, by.x="Var1", by.y="pic_id")
ggplot(data=subset(m.lineup, Freq==2)) + geom_bar(aes(x=r, fill=plot)) + facet_wrap(~n)
#high sample size increases chance of multiple selection, even spread across correlations except -0.5/-0.3 lines t=96

#Subject ability#
turk.correct <- summarise(group_by(full.sub, pic_id), correct=sum(correct), attempts=sum(weights*attempts))
turk.correct$percent <- turk.correct$correct/turk.correct$attempts
ability <- merge(full.sub[,c("subj_id", "pic_id", "correct")], turk.correct[c("pic_id", "percent")], by="pic_id")
ability <- summarise(group_by(ability, subj_id), actual=sum(correct), estimated=sum(percent))
ability$diff <- ability$actual - ability$estimated
ability.re <- merge(ability, randomeffects, by="subj_id")
ggplot(data=ability.re, aes(x=diff, y=re)) + geom_point()


