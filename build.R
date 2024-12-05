Sys.setenv(MAKEFLAGS = "-j8") 
library(devtools)
checks <- check()
num_issues <-
    length(checks$"warnings") +
    length(checks$"errors")
if (num_issues == 0) {
    print("No errors or warnings. Now installing.")
    install()
} else {
    print(checks)
}
