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
  
  map_dfr(deps$package, pkg_rver) %>%
    left_join(deps, by = "package") %>%
    select(package, type, r_version, everything()) %>%
    arrange(desc(r_major), desc(r_minor), desc(r_patch))
}

get_deps <- function(desc, dep_types = c("Depends", "Imports", "Suggests", "Enhances")) {
  dep_types <- match.arg(dep_types, several.ok = TRUE)
  desc::desc_get_deps(desc) %>% 
    filter(type %in% dep_types, package != "R") %>% 
    select(-version)
}

get_desc_file <- function(pkg) {
  desc_file <- system.file("DESCRIPTION", package = pkg)
  
  if (!file.exists(desc_file)) {
    # message("No DESCRIPTION file locally for ", pkg,
    #         ".\n Attempting to get from https://github.com/cran/", pkg)
    desc_file <- tempfile()
    url <- paste0("https://raw.githubusercontent.com/cran/", 
                  pkg, "/master/DESCRIPTION")
    res <- GET(url, write_disk(desc_file, overwrite = TRUE))
    if (res$status_code > 200) return(NULL)
  }
  desc_file
}

parse_rver <- function(x) {
  just_ver <- gsub("(\\s*\\(?>?=\\s*)|\\)", "", x)
  if (!length(just_ver)) return(rep(NA_integer_, 3))
  unclass(as.package_version(just_ver))[[1]]
}

pkg_rver <- function(pkg) {
  desc_file <- get_desc_file(pkg)
  if (is.null(desc_file)) return(NULL)
  
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
  list(package = pkg, r_version = R_ver_txt, 
       r_major = R_ver[1], r_minor = R_ver[2], r_patch = R_ver[3])
}

ui <- fluidPage(
  
  titlePanel("Find the R version specified in the dependencies of a package"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("pkg","Choose a package on CRAN", 
                placeholder = "Name of package"),
      actionButton("checkPkg", "Check Package"),
      hr(),
      fileInput("descFile", "Or upload a DESCRIPTION file"), 
      hr(),
      checkboxGroupInput("depTypes", "Select dependency types:", 
                         choices = c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo"), 
                         selected = c("Depends", "Imports", "Suggests")),
      hr(),
      p("Source code for this app can be found", 
        a(href = "https://github.com/ateucher/pkg-deps-r-ver", "here."))
      
    ),
    
    mainPanel(
      h2(htmlOutput("pkgname")),
      dataTableOutput("pkgdeps", width = "95%")
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
    paste0("R versions in <em>", pkg, "</em> dependencies")
  })
  
  output$pkgdeps <- renderDataTable({
    req(rv$desc)
    get_dep_r_versions(desc = rv$desc,
                       dep_types = input$depTypes)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
