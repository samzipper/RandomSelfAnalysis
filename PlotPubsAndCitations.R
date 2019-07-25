## PlotPubsAndCitations.R
#' This script is intended to extract publications and citations from Google Scholar
#' and make a good-looking plot you can put on your CV.

# load packages
require(scholar)  # interface with google scholar
require(ggplot2)  # for plotting
require(dplyr)    # for data tidying
require(stringr)  # for working with string data

# set user id (get this from your Google Scholar URL)
me <- "XXIpO1YAAAAJ"

# get publications
pubs <- 
  scholar::get_publications(me) %>% 
  # get rid of non-journal articles (e.g. theses) - in my profiles, these don't have a year
  subset(is.finite(year))

# make new column with first author from each publication
pubs$first_author <- 
  # the 'author' column is a factor by default, so first convert to character
  pubs$author %>% 
  as.character() %>% 
  # the authors are a comma-separated string, so we need to split based on commas and grab the first author
  strsplit(split="[,]") %>% 
  sapply(function(x) x[1])

pubs$first_author

# figure out which papers I wrote
my_name <- "Zipper"

pubs$first_author_me <-
  pubs$first_author %>% 
  stringr::str_detect(pattern = my_name)

# i am co-lead author on a paper in which my name is listed second; 
# i also want to include this, which I will do using the pubid (which I looked in the pubs table to get)
pubs$first_author_me[pubs$pubid %in% c("B3FOqHPlNUQC")] <- TRUE


# use `pubid` to get citations for each paper and combined
for (i in 1:length(pubs$pubid)){
  # grab citations for this paper
  paper_cites <- 
    scholar::get_article_cite_history(id = me, article = pubs$pubid[i])
  
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
  dplyr::left_join(all_cites, 
                   pubs[, c("pubid", "first_author_me")], 
                   by="pubid")

## now we've got all the data! let's prepare it a bit to make plotting easier
# for the plots, we want annual sums
pubs_yr <-
  pubs %>% 
  dplyr::group_by(year, first_author_me) %>% 
  dplyr::summarize(number = n(),            # could use any field
            metric = "Publications") # this will come in handy later
cites_yr <-
  all_cites %>% 
  dplyr::group_by(year, first_author_me) %>% 
  dplyr::summarize(number = sum(cites),
            metric = "Citations")

# to make a faceted plot, we'll want to combine these into a single data frame
pubs_and_cites <- rbind(pubs_yr, cites_yr)

## finally - let's plot!
ggplot(pubs_and_cites, aes(x=factor(year), y=number, fill=first_author_me)) +
  geom_bar(stat="identity") +
  facet_wrap(~factor(metric, levels=c("Publications", "Citations")),
             scales = "free_y") +
  # everything below here is just aesthetics
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Number") +
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
        legend.justification = c(0, 1)) +
  ggsave("PlotPubsAndCitations.png", 
         width = 8, height = 4, units = "in")
