# .onLoad <- function(libname, pkgname) 
# {
#   library.dynam("EFBA", pkgname, libname)
# }

efbaStartupMessage <- function()
{
  dependencies <- c("fields", "momentchi2", "Rcpp", "stats", "fda", "shiny", "viridis", 
                    "grid", "gridExtra", "ggplot2", "shinyjs", "plotly", "signal")
  versions <- c("15.2", "0.1.5", "1.0.13", "4.3.1", "6.1.8", "1.9.1", "0.6.5", "4.3.1", 
                "2.3", "3.5.1", "2.1.0", "4.10.4", "1.8.1")
  res <- logical(0)
  for(i in 1:length(dependencies)){
    res[i] = (packageVersion(dependencies[i]) == versions[i])
  }
  if(all(res)){
    msg <- ""
  } else {
    pkg <- which(!res)
    msg <- "This package and the functions within may not work properly. This is because: "
    for(i in 1:length(pkg)){
      msg <- cat(msg, "\n * The EFBA package was constructed using version", versions[pkg[i]], "of the package", dependencies[pkg[i]], 
                 "and you currently are using version", as.character(packageVersion(dependencies[pkg[i]])), "of that package")
    }
  }
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  # unlock .EFBA variable allowing its modification
  #unlockBinding(".EFBA", asNamespace("EFBA")) 
  # startup message
  msg <- efbaStartupMessage()
  if(!interactive())
    msg[1] <- paste("Package 'EFBA' version", packageVersion("EFBA"))
  packageStartupMessage(msg)      
  invisible()
}
