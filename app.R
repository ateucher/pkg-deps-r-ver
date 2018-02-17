library(shiny)
library(purrr)
library(dplyr)
library(tidyr)
library(desc)
library(DT)
library(httr)

get_dep_r_versions <- function(desc = NULL, dep_types = c("Depends", "Imports", "Suggests", "Enhances")) {
  dep_types <- match.arg(dep_types, several.ok = TRUE)
  deps <- get_deps(desc = desc, dep_types = dep_types)
  if (!nrow(deps)) {
    return(data_frame(package = character(0), type = character(0), r_version = character(0),
                      r_major = integer(0), r_minor = integer(0), r_patch = integer(0)))
  }
  
  descs <- map(deps$package, get_desc_file)
  
  map_dfr(descs, pkg_rver) %>%
    left_join(deps, by = "package") %>%
    select(package, cran_ver, you_req = version, type, r_version, everything()) %>%
    arrange(desc(r_major), desc(r_minor), desc(r_patch))
}

get_deps <- function(desc, dep_types = c("Depends", "Imports", "Suggests", "Enhances")) {
  dep_types <- match.arg(dep_types, several.ok = TRUE)
  desc::desc_get_deps(desc) %>% 
    filter(type %in% dep_types, package != "R")
}

get_desc_file <- function(pkg) {
  # desc_file <- system.file("DESCRIPTION", package = pkg)
  # 
  # if (!file.exists(desc_file)) {
  # message("No DESCRIPTION file locally for ", pkg,
  #         ".\n Attempting to get from https://github.com/cran/", pkg)
  desc_file <- tempfile()
  url <- paste0("https://raw.githubusercontent.com/cran/", 
                pkg, "/master/DESCRIPTION")
  res <- GET(url, write_disk(desc_file, overwrite = TRUE))
  if (res$status_code > 200) return(NULL)
  # }
  desc_file
}

parse_rver <- function(x) {
  just_ver <- gsub("(\\s*\\(?>?=\\s*)|\\)", "", x)
  if (!length(just_ver)) return(rep(NA_integer_, 3))
  unclass(as.package_version(just_ver))[[1]]
}

pkg_rver <- function(desc_file) {
  if (is.null(desc_file)) return(NULL)
  
  pkg_ver <- as.character(desc::desc_get_version(desc_file))
  
  deps <- desc::desc_get_deps(desc_file)
  if (is.null(deps)) {
    R_ver_txt <- character(0)
  } else {
    R_ver_txt <- filter(deps, package == "R") %>% pull(version)
  }
  if (!length(R_ver_txt)) {
    R_ver_txt <- NA_character_
    R_ver <- NA_integer_
  }  else {
    R_ver <- parse_rver(R_ver_txt)
  }
  list(package = desc_get("Package", desc_file), cran_ver = pkg_ver, r_version = R_ver_txt, 
       r_major = R_ver[1], r_minor = R_ver[2], r_patch = R_ver[3])
}

make_ver <- function(x) {
  all_vers <- paste(x$r_major, x$r_minor, x$r_patch, sep = ".")
  package_version(gsub("NA", "0", all_vers))
}

ui <- fluidPage(
  
  titlePanel("Find appropriate versions of dependencies for your package"),
  p("This app is meant to help you choose a reasonable minimum version of 
    dependencies for your package by showing you the current CRAN version 
    of each dependency, and the R version that each depends on. It's not 
    necessary to require a minimum R version in your package, and you probably 
    shouldn't unless you are required to by ", code("R CMD check"), " or you
    have explicitly verified that your package works on versions of R back to 
    a particular version (new advice from Hadley Wickham). There is a discussion 
    of this issue ", a("on the RStudio Community forum.", 
    href = "https://community.rstudio.com/t/determining-which-version-of-r-to-depend-on/4396/10")),
  
  sidebarLayout(
    sidebarPanel(
      textInput("pkg","Choose a package on CRAN", 
                placeholder = "Name of package"),
      actionButton("checkPkg", "Check Package"),
      hr(),
      fileInput("descFile", "Or upload a DESCRIPTION file"), 
      actionButton("checkFile", "Check DESCRIPTION file"),
      hr(),
      checkboxGroupInput("depTypes", "Select dependency types to include:", 
                         choices = c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo"), 
                         selected = c("Depends", "Imports", "Suggests")),
      hr(),
      p("Source code for this app is", 
        a(href = "https://github.com/ateucher/pkg-deps-r-ver", "here."))
      
    ),
    
    mainPanel(
      h4(htmlOutput("verComp")),
      hr(),
      h3(htmlOutput("pkgname")),
      DT::dataTableOutput("pkgdeps", width = "95%"),
      p("This table lists the packages listed as dependencies by the selected package, 
        their current version on CRAN, the version required by the selected package, 
        and the minimum version of R required by each of those dependencies."), 
      p("*Note that only dependencies on CRAN are checked")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(desc = NULL)
  
  observeEvent(input$checkPkg, {
    req(input$pkg)
    rv$desc <- get_desc_file(input$pkg)
  })
  
  observeEvent(input$checkFile, {
    desc_file <- input$descFile
    if (desc_file$name != "DESCRIPTION")
      stop("File must be called DESCRIPTION")
    rv$desc <- desc_file$datapath
    updateTextInput(session, inputId = "pkg", value = "")
  })
  
  output$pkgname <- renderText({
    req(rv$desc)
    pkg <- desc_get("Package", rv$desc)
    pkg_ver <- desc_get_version(rv$desc)
    paste0("Dependencies* for <em>", pkg, " v", pkg_ver, 
           "</em>, and minimum R version required by those dependencies")
  })
  
  tbl <- reactive(get_dep_r_versions(desc = rv$desc,
                            dep_types = input$depTypes))
  
  output$pkgdeps <- DT::renderDataTable({
    req(rv$desc)
    tbl()
  }, colnames = c("Package", "Version on CRAN", "Required Version", "Dependency Type", "R Version", 
                  "R Major", "R Minor", "R Patch"))
  
  output$verComp <- renderText({
    req(rv$desc)
    
    max_rver <- max(make_ver(tbl()), na.rm = TRUE)
    curr_ver <- make_ver(pkg_rver(rv$desc))

    diff <- if (curr_ver > max_rver) {
      c("a newer", "than")
    } else if (curr_ver < max_rver) {
      c("an older", "than")
    } else {
      c("the same", "as")
    }
    
    paste("The R version required by your package is:", curr_ver, "</br>", 
          "The newest R version required by your dependencies is: ", max_rver, 
          "</br>Your package requires", diff[1], "R version", diff[2], 
          "the newest version of R required by your dependencies.")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
