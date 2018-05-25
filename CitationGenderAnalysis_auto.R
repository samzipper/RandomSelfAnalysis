#devtools::install_github("juba/rwos")  # https://github.com/juba/rwos
#devtools::install_github("ottlngr/bib2df")
require(gender)
require(scholar)
require(rwos)
require(rcrossref)
require(bib2df)

# get my publications
scholar_id <- "XXIpO1YAAAAJ"
my_pubs <- get_publications(scholar_id)

get_article_cite_history(scholar_id, my_pubs$pubid[1])

# web of science version from rwos
sid <- wos_authenticate()

res <- wos_search(sid, "AU=Zipper SC")
pubs <- wos_retrieve_all(res)

cr_cn(pubs$doi[1], "bibtex")

# gender
gender("Sam", method="genderize")
