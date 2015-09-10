# static_help = function(pkg, links = tools::findHTMLlinks()) {
#   pkgRdDB = tools:::fetchRdDB(file.path(find.package(pkg), 'help', pkg))
#   force(links); topics = names(pkgRdDB)
#   for (p in topics) {
#     tools::Rd2HTML(pkgRdDB[[p]], paste('html/',p, '.html', sep = ''),
#                    package = pkg, Links = links, no_links = is.null(links))
#   }
# }

#dir.create(file.path('./', 'html'), showWarnings = FALSE)
#static_help('SBadapter')


# R CMD Rdconv -t html ./man/SBmodel-class.Rd  > SBmodel.html

#RUN THESE COMMANDS TO CREATE THE HELP:
# createHelp = function() {
#   dir.create(file.path('./SBadapter', 'html'), showWarnings = FALSE)
#   setwd("./SBadapter/html")
#   library(knitr)
#   knit_rd("SBadapter")
#   setwd("../../")
# }

#https://github.com/jimhester/knitrBootstrap

#library(devtools)
#install_github('rstudio/rmarkdown')
#install.packages('knitr', repos = c('http://rforge.net', 'http://cran.rstudio.org'),type = 'source')
#install_github('jimhester/knitrBootstrap')

#library(knitrBootstrap)
#library(rmarkdown)
#render('./Radapter/SBadapter/inst/extdata/examples-master.rmd', 'knitrBootstrap::bootstrap_document')
