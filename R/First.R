.First.lib <- function(lib,pkg) {
	library.dynam("Iso", pkg, lib)
	ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
        cat(paste(pkg, ver, "\n"))
        cat("Note: A new argument \"stepfun\" has been added to\n")
        cat("pava() and pava.sa().  A new argument \"type\" has been\n")
	cat("added to ufit().  See the documentation.\n")
}
