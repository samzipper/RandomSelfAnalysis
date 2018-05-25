## CitationGenderAnalysis.R

# where to save the results
setwd("C:/Users/Sam/Dropbox/Work/Outreach & Press/Twitter/")

require(ggplot2)
require(reshape2)
require(magrittr)

# data
df <- data.frame(names=c("HRMET", "YieldGWsoil", "MAGI", "UHI-ET", "UHI-GSL", "AgroStream", "USDroughtYield", "Platte"),
                 n = c(1, 2, 6, 5, 3, 8, 4, 7),
                 year = c(2014, 2015, 2017, 2017, 2016, 2018, 2016, 2017),
                 refs_M = c(62, 101, 118, 44, 65, 50, 67, 53),
                 refs_F = c(10, 18, 31, 15, 31, 10, 13, 34),
                 refs_X = c(7, 18, 18, 5, 5, 4, 8, 6),
                 refs_me = c(0, 1, 5, 2, 1, 4, 2, 4))

df.time <- data.frame(interval = factor(c("<2008", "2008-2012", "2013-2017")),
                      refs_M = c(78, 66, 106),
                      refs_F = c(18, 24, 48))
df.time$refs_M_prc = df.time$refs_M/(df.time$refs_M+df.time$refs_F)
df.time$refs_F_prc = df.time$refs_F/(df.time$refs_M+df.time$refs_F)

# add some new columns
df$refs_n = df$refs_M + df$refs_F + df$refs_X
df$refs_M_prc = (df$refs_M-df$refs_me)/(df$refs_n - df$refs_X - df$refs_me)
df$refs_F_prc = df$refs_F/(df$refs_n - df$refs_X - df$refs_me)

## stats
mean(df$refs_M_prc)
sum(df$refs_n)
sum(df$refs_n) - sum(df$refs_X)

# colors
col.F <- "#86468B"
col.M <- "#94A537"

# make plots: all refs, bar-plot
df.plot <- 
  df %>% 
  subset(select=c("n", "refs_M", "refs_F", "refs_X")) %>% 
  melt(id="n")
df.plot$variable <- factor(df.plot$variable, levels=c("refs_X", "refs_F", "refs_M"), labels=c("?", "F", "M"))

ggplot(df.plot, aes(x=factor(n), y=value, fill=variable)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0, color="gray65") +
  scale_x_discrete(name="Paper #") +
  scale_y_continuous(name="Number of References") +
  scale_fill_manual(name="Gender", values=c("grey55", col.F, col.M)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  ggsave("CitationGenderAnalysis_nRefs.png", width=6, height=3, units="in")

# plot: proportion of references
df.prc <-
  df %>% 
  subset(select=c("n", "year", "names", "refs_M_prc", "refs_F_prc")) %>% 
  melt(id=c("n", "year", "names"))
df.prc$variable <- factor(df.prc$variable, levels=c("refs_F_prc", "refs_M_prc"), labels=c("F", "M"))

ggplot(df.prc, aes(x=factor(n), y=value, fill=variable)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0, color="gray65") +
  scale_x_discrete(name="Paper #") +
  scale_y_continuous(name="Percent of References", labels=scales::percent, expand=c(0,0)) +
  scale_fill_manual(name="Gender", values=c(col.F, col.M)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  ggsave("CitationGenderAnalysis_prcRefs.png", width=6, height=3, units="in")


ggplot(df.prc, aes(x=year, y=value, color=variable)) +
  geom_point() +
  stat_smooth(method="lm", se=F) +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Percent of References", labels=scales::percent,
                     limits=c(0,1), expand=c(0,0)) +
  scale_color_manual(name="Gender", values=c(col.F, col.M)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  ggsave("CitationGenderAnalysis_prcRefsYear.png", width=6, height=3, units="in")

summary(lm(value ~ year, subset(df.prc, variable=="M")))
summary(lm(value ~ year, subset(df.prc, variable=="F")))

## plot: different time windows
df.time.prc <- 
  df.time %>% 
  subset(select=c("interval", "refs_M_prc", "refs_F_prc")) %>% 
  melt(id=c("interval"))
df.time.prc$variable <- factor(df.time.prc$variable, levels=c("refs_F_prc", "refs_M_prc"), labels=c("F", "M"))

ggplot(df.time.prc, aes(x=interval, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0, color="gray65") +
  scale_x_discrete(name="Reference Publication Year") +
  scale_y_continuous(name="Percent by Gender", labels=scales::percent, expand=c(0,0)) +
  scale_fill_manual(name="Gender", values=c(col.F, col.M)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  ggsave("CitationGenderAnalysis_prcRefsPubYear.png", width=6, height=3, units="in")
