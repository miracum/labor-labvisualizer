packagename <- "labVisualizeR"

person1 <- person(
  given = "Lorenz A.",
  family = "Kapsner",
  email = "lorenz.kapsner@uk-erlangen.de",
  comment = c(ORCID = "0000-0003-1866-860X"),
  role = c('cre', 'aut')
)
person2 <- person(
  given = "Jakob",
  family = "Zierk",
  role = c('ctb')
)
person3 <- person(
  "MIRACUM - Medical Informatics in Research and Care in University Medicine",
  role = c("fnd")
)
person4 <- person(
  "Universitätsklinikum Erlangen",
  role = "cph"
)

# remove existing description object
unlink("DESCRIPTION")
# Create a new description object
my_desc <- desc::description$new("!new")
# Set your package name
my_desc$set("Package", packagename)
# Set author names
my_desc$set_authors(
  c(
    person1,
    person2,
    person3,
    person4
  )
) #,
#  person("Name2", "Surname2", email = "mail@2", role = 'aut')))
# Remove some author fields
my_desc$del("Maintainer")
# Set the version
my_desc$set_version("0.0.3")
# The title of your package
my_desc$set(Title = paste(
  "labVisualizeR - an interactive web application to",
  "visualize measurements from SQL databases"
))
# The description of your package
my_desc$set(Description = paste(
  "The R tool 'labVisualizeR' provides a shiny web application that aims to",
  "interactively visualize measurements from SQL databases.",
  "It has primarily been developed to visualize laboratory mesaurements,",
  "but can in principal be used to visualize any continuous and discrete",
  "values."
))
# The description of your package
my_desc$set("Date/Publication" = paste(as.character(Sys.time()), "UTC"))
# The urls
my_desc$set("URL", "https://github.com/miracum/labor-labvisualizer")
my_desc$set("BugReports",
            "https://github.com/miracum/labor-labvisualizer/issues")
# License
my_desc$set("License", "GPL-3")
# Save everyting
my_desc$write(file = "DESCRIPTION")

# License
usethis::use_gpl3_license(name = "Universitätsklinikum Erlangen")


# add Imports and Depends
# Listing a package in either Depends or Imports ensures that it’s installed when needed
# Imports just loads the package, Depends attaches it
# Loading will load code, data and any DLLs; register S3 and S4 methods; and run the .onLoad() function.
##      After loading, the package is available in memory, but because it’s not in the search path,
##      you won’t be able to access its components without using ::.
##      Confusingly, :: will also load a package automatically if it isn’t already loaded.
##      It’s rare to load a package explicitly, but you can do so with requireNamespace() or loadNamespace().
# Attaching puts the package in the search path. You can’t attach a package without first loading it,
##      so both library() or require() load then attach the package.
##      You can see the currently attached packages with search().

# Depends

# Imports
usethis::use_package("data.table", type="Imports")
usethis::use_package("utils", type="Imports")
usethis::use_package("shiny", type="Imports")
usethis::use_package("shinydashboard", type="Imports")
usethis::use_package("shinyjs", type="Imports")
usethis::use_package("DBI", type = "Imports")
usethis::use_package("stats", type = "Imports")
usethis::use_package("graphics", type = "Imports")
usethis::use_package("DT", type = "Imports")
usethis::use_package("e1071", type = "Imports")
usethis::use_package("ggplot2", type = "Imports")
usethis::use_package("magrittr", type = "Imports")
usethis::use_package("DIZutils", type = "Imports")

# Suggests
usethis::use_package("testthat", type = "Suggests")
usethis::use_package("processx", type = "Suggests")
usethis::use_package("lintr", type = "Suggests")
usethis::use_package("jsonlite", type = "Suggests")

# buildignore and gitignore
usethis::use_build_ignore("LICENSE.md")
usethis::use_build_ignore(".gitlab-ci.yml")
usethis::use_build_ignore("data-raw")
usethis::use_build_ignore(".vscode")
usethis::use_build_ignore(".lintr")
usethis::use_build_ignore("docker_deployment")

usethis::use_git_ignore("/*")
usethis::use_git_ignore("/*/")
usethis::use_git_ignore("*.log")
usethis::use_git_ignore("!/.gitignore")
usethis::use_git_ignore("!/.Rbuildignore")
usethis::use_git_ignore("!/.gitlab-ci.yml")
usethis::use_git_ignore("!/data-raw/")
usethis::use_git_ignore("!/DESCRIPTION")
usethis::use_git_ignore("!/inst/")
usethis::use_git_ignore("/inst/*")
usethis::use_git_ignore("!/inst/application/")
usethis::use_git_ignore("inst/application/_settings/")
usethis::use_git_ignore("!/LICENSE.md")
usethis::use_git_ignore("!/man/")
usethis::use_git_ignore("!NAMESPACE")
usethis::use_git_ignore("!/R/")
usethis::use_git_ignore("!/README.md")
usethis::use_git_ignore("!/tests/")
usethis::use_git_ignore("/.Rhistory")
usethis::use_git_ignore("!/*.Rproj")
usethis::use_git_ignore("/.Rproj*")
usethis::use_git_ignore("/.RData")
usethis::use_git_ignore("!/docker_deployment")
usethis::use_git_ignore("/.vscode")
usethis::use_git_ignore("!/.lintr")


# code coverage
#covr::package_coverage()

# lint package
#lintr::lint_package()

# test package
#devtools::test()

# R CMD check package
#rcmdcheck::rcmdcheck()
#rcmdcheck::rcmdcheck(args = "--no-vignettes", build_args = "--no-build-vignettes")

# tidy description
usethis::use_tidy_description()
