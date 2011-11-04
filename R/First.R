.onLoad <- function(lib, pkg) {
	library.dynam("Iso", pkg, lib)
	ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
        packageStartupMessage(paste(pkg, ver))
}
