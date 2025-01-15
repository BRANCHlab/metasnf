library(devtools)
checks <- check(
    vignettes = FALSE,
    cran = FALSE,
    incoming = FALSE
)
num_issues <- length(checks$"warnings") + length(checks$"errors")
if (num_issues == 0) {
    print("No errors or warnings. Now installing.")
    install(dependencies = FALSE)
} else {
    print(checks)
}
