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

createHelp = function() {
  dir.create(file.path('./', 'html'), showWarnings = FALSE)
  setwd("./html")
  library(knitr)
  setwd("..")
}

# knit_rd("SBadapter")

# R CMD Rdconv -t html ./man/SBmodel-class.Rd  > SBmodel.html
