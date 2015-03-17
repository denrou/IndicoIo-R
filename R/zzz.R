
# Creates new environment for the package
if (!exists(".indicoio")) {
  .indicoio <- new.env()
}

.onAttach <- function(libname, pkgname) {
  # Shows welcome message
  packageStartupMessage("\n========================================================\nindicoio: A simple R wrapper for the indico set of APIs \nFind more at: http://indico.io\n========================================================\n")
}

.onLoad <- function(libname, pkgname) {
  # Sets package-wide variables
  if (exists(".indicoio")) {
    .indicoio$header <- c("Content-type" = "application/json",
                          "Accept" = "text/plain")
    .indicoio$remote_api <- "http://apiv1.indico.io/"
    .indicoio$private_cloud <- "http://%s.indico.domains/"
    .indicoio$auth = FALSE
    .indicoio$cloud = FALSE

    # Paths to search for config files
    globalPath <- path.expand("~/.indicorc")
    localPath <- file.path(getwd(), ".indicorc")

    # Load from global configuration file
    if (file.exists(globalPath)) {
      globalConfig <- Parse.INI(globalPath)
      if (valid_auth_config(globalConfig)) {
        .indicoio$auth <- c(
          globalConfig$auth$username,
          globalConfig$auth$password
        )
      }

      if (valid_private_cloud_config(globalConfig)) {
        .indicoio$cloud <- globalConfig$private_cloud$cloud
      }
    }

    # Load from local configuration file
    if (file.exists(localPath)) {
      localConfig <- Parse.INI(localPath)
      if (valid_auth_config(localConfig)) {
        .indicoio$auth <- c(
          localConfig$auth$username,
          localConfig$auth$password
        )
      }

      if (valid_private_cloud_config(localConfig)) {
        .indicoio$cloud <- localConfig$private_cloud$cloud
      }
    }

    # Load auth from environment variables
    authDefined <- ((Sys.getenv("INDICO_USERNAME") != "") &&
                    (Sys.getenv("INDICO_PASSWORD") != ""))
    if (authDefined) {
      .indicoio$auth <- c(
        Sys.getenv("INDICO_USERNAME"),
        Sys.getenv("INDICO_PASSWORD")
      )
    }

    # Load subdomain from environment variables
    cloudDefined <- (Sys.getenv("INDICO_CLOUD") != "")
    if (cloudDefined) {
      .indicoio$cloud <- Sys.getenv("INDICO_CLOUD")
    }
  }
}

valid_auth_config <- function(config) {
  # ensure .ini file contains the proper fields
  return (("auth" %in% names(config)) && 
          ("username" %in% names(config[['auth']])) &&
          ("password" %in% names(config[['auth']])))
}

valid_private_cloud_config <- function(config) {
  # ensure .ini file contains the proper fields
  return (("private_cloud" %in% names(config)) &&
          ("cloud" %in% names(config[['private_cloud']])))
}

Parse.INI <- function(INI.filename) 
{ 
  # Parse .ini style configuration files (.indicorc)
  connection <- file(INI.filename) 
  Lines  <- readLines(connection) 
  close(connection) 

  # change section headers 
  Lines <- chartr("[]", "==", Lines)

  connection <- textConnection(Lines) 
  d <- read.table(connection, as.is = TRUE, sep = "=", fill = TRUE) 
  close(connection) 

  # location of section breaks 
  L <- d$V1 == "" 
  d <- subset(transform(d, V3 = V2[which(L)[cumsum(L)]])[1:3], 
                           V1 != "") 

  ToParse  <- paste("INI.list$", d$V3, "$",  d$V1, " <- '", 
                    d$V2, "'", sep="")

  INI.list <- list() 
  eval(parse(text=ToParse)) 

  return(INI.list) 
}
