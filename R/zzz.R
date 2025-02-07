.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Version ", packageVersion(pkgname), 
                        " of package ", pkgname, ". You can use function asitis_init() for all necessary settings")
}