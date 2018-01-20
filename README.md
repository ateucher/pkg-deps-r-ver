# Check the R versions in your package's dependencies

A Shiny App that inspects the `DESCRIPTION` file of a package,
finds all of the dependencies, and lists the R version required (Depended on)
by each of those packages. This should help help you figure out what the minimum version
of R *your* package should depend on.

It can be found online here: https://ateucher.shinyapps.io/check_r_versions_of_package_dependencies/

This was inspired by [this twitter thread](https://twitter.com/Jemus42/status/951120777864204288) and the subsequent realization that I was not being very careful about this in my own packages.
