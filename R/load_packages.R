# Function definitions -------------------
#This function takes a set of packages and installs and loads them.
#There is no return, only a successful output notification.

load_packages <-  function(packages){
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], repos = "http://cran.us.r-project.org")
}
invisible(lapply(packages, library, character.only = TRUE)) #load packages
print("All packages installed and loaded")
}