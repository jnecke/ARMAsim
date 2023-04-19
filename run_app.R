
# packages used for the application
pac <- c("scales", "shiny", "rmarkdown")

# install and/or load packages
checkpac <- function(x) {
      if (!require(x, character.only = TRUE)) {
            install.packages(x)
      }
      require(x, character.only = TRUE)
}

# check if packages are install yet
suppressWarnings(sapply(pac, checkpac))

run_app <- function (url = "https://github.com/jnecke/ARMAsim/archive/refs/heads/main.zip") {
      
      # inputs
      url <- "https://github.com/jnecke/ARMAsim/archive/refs/heads/main.zip"
      
      filetype <- NULL
      subdir <- NULL
      destdir <- NULL #, ...
      
      
      
      if (!is.null(subdir) && ".." %in% strsplit(subdir, "/")[[1]]) {
            stop("'..' not allowed in subdir") 
      }
      if (is.null(filetype)) {
            filetype <- basename(url)
      }
      if (grepl("\\.tar\\.gz$", filetype)) {
            fileext <- ".tar.gz"
      } else if (grepl("\\.tar$", filetype)) {
            fileext <- ".tar"
      } else if (grepl("\\.zip$", filetype)) {
            fileext <- ".zip"
      } else {
            stop("Unknown file extension.")
      }
      message("Downloading ", url)
      if (is.null(destdir)) {
            filePath <- tempfile("shinyapp", fileext = fileext)
            fileDir <- tempfile("shinyapp")
      } else {
            fileDir <- destdir
            filePath <- paste(destdir, fileext)
      }
      dir.create(fileDir, showWarnings = FALSE)
      
      # if (download(url, filePath, mode = "wb", quiet = TRUE) != 0) {
      #   stop("Failed to download URL ", url)
      # }
      
      if (download.file(url, filePath, mode = "wb", quiet = TRUE) != 0) {
            stop("Failed to download URL ", url)
      }
      
      on.exit(unlink(filePath))
      if (fileext %in% c(".tar", ".tar.gz")) {
            first <- untar2(filePath, list = TRUE)[1]
            untar2(filePath, exdir = fileDir)
      } else if (fileext == ".zip") {
            first <- as.character(utils::unzip(filePath, list = TRUE)$Name)[1]
            utils::unzip(filePath, exdir = fileDir)
      }
      if (is.null(destdir)) {
            on.exit(unlink(fileDir, recursive = TRUE), add = TRUE)
      }
      appdir <- file.path(fileDir, first)
      if (!utils::file_test("-d", appdir)) {
            appdir <- dirname(appdir)
      }
      if (!is.null(subdir)) {
            appdir <- file.path(appdir, subdir)
      }
      
      # runApp(appdir)
      
      # file.path <- file.path(appdir, "ShinyDocument.rmd")
      file.path <- file.path(appdir, "armasim")
      file.path
      
      # rmarkdown::run(file = file.path,
      #                shiny_args = list(launch.browser = TRUE))
      shiny::runApp(appDir = file.path,
                    launch.browser = TRUE)
      
      unlink(fileDir, recursive = TRUE)
      
}

run_app(url = "https://github.com/jnecke/ARMAsim/archive/refs/heads/main.zip")
