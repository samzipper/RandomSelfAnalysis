## PlotPubsAndCitations.R
#' This script is intended to extract publications and citations from Google Scholar
#' and make a good-looking plot you can put on your CV.

# load packages
require(scholar)  # interface with google scholar
require(ggplot2)  # for plotting
require(dplyr)    # for data tidying
require(reshape2) # for data tidying
require(stringr)  # for working with string data

# set user id (get this from your Google Scholar URL)
me <- "XXIpO1YAAAAJ"

# get publications
pubs <- 
  me %>% 
  get_publications() %>% 
  # get rid of non-journal articles (e.g. theses) - in my profiles, these don't have a year
  subset(is.finite(year))

# make new column with first author from each publication
pubs$first_author <- 
  # the 'author' column is a factor by default, so first convert to character
  pubs %>% 
  .$author %>% 
  as.character() %>% 
  # the authors are a comma-separated string, so we need to split based on commas and grab the first author
  strsplit(split="[,]") %>% 
  sapply(function(x) x[1])

pubs$first_author

# uh-oh! I'm referred to as both 'S Zipper' and 'SC Zipper'.
# fortunately, I don't have any co-authors named Zipper, so I can 
# just search for first authors containing my last name.
pubs$first_author_me <-
  pubs %>% 
  .$first_author %>% 
  stringr::str_detect(pattern="Zipper")

# great! we now have my publication history. now, let's work on citations. 
# the `pubid` field can be used to get annual citations for each publication. 
# we'll loop through all my papers, extract the citations, and put them into a big data frame
# there's probably a vectorized way to do this which might be worth figuring out
# if you have a lot of papers.

for (i in 1:length(pubs$pubid)){
  # grab citations for this paper
  paper_cites <- get_article_cite_history(id = me, 
                                          article = pubs$pubid[i])
  
  # make master data frame
  if (i == 1){
    all_cites <- paper_cites
  } else {
    all_cites <- rbind(all_cites, paper_cites)
  }
}

# now we need to figure out who the first author was for each of these papers - 
# we can join it with the pubs data frame
all_cites <- 
  left_join(all_cites, 
            pubs[, c("pubid", "first_author_me")], 
            by="pubid")

## now we've got all the data! let's prepare it a bit to make plotting easier
# for the plots, we want annual sums
pubs_yr <-
  pubs %>% 
  group_by(year, first_author_me) %>% 
  summarize(number = n(),            # could use any field
            metric = "Publications") # this will come in handy later
cites_yr <-
  all_cites %>% 
  group_by(year, first_author_me) %>% 
  summarize(number = sum(cites),
            metric = "Citations")

# to make a nice faceted plot, we'll want to combine these into a single data frame
pubs_and_cites <- rbind(pubs_yr, cites_yr)

## finally - let's plot!
ggplot(pubs_and_cites, aes(x=factor(year), y=number, fill=first_author_me)) +
  geom_bar(stat="identity") +
  facet_wrap(~factor(metric, levels=c("Publications", "Citations")),
             scales = "free_y") +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Number") +
  # everything below here is just aesthetics
  scale_fill_manual(name = "First Author", 
                    values = c("TRUE"="#e6194b", "FALSE"="#0082c8"),
                    labels = c("TRUE"="Zipper", "FALSE"="Other")) +
  theme_bw(base_size=12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 11, face="bold"),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10, face="bold"),
        legend.position = c(0.01,0.99),
        legend.justification = c(0, 1))
