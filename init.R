my_packages = c("plumber")
install_if_missing = function(p) {
    if(p %in% rownames(installed.packages()) == FALSE) {
        install.packages(p, dependencies = TRUE)
    }
    else {
        cat(paste("Skipping already installed packages: ", p, "/n"))
    }
}
invisible(sapply(my_packages, install_if_missing))