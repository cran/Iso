.onLoad <- function(lib, pkg) {
	library.dynam("Iso", pkg, lib)
	ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
        cat(paste(pkg, ver, "\n"))
        cat("Note: This package now has a NAMSPACE.\n")
}
