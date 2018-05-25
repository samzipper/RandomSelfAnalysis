## scholar.R
# messing around with Google Scholar package
# https://cran.r-project.org/web/packages/scholar/index.html

require(scholar)
require(ggplot2)

id <- "XXIpO1YAAAAJ"  # my user ID

my_pubs <- get_publications(id)

# my citation history
citation_history <- get_citation_history(id)
ggplot(citation_history, aes(x=year, y=cites)) + geom_line() + geom_point()

predict_h_index(id)
