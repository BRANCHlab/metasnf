library(devtools)
checks <- check()
num_issues <-
    length(checks$"warnings") +
    length(checks$"errors") +
    length(checks$"notes")
if (num_issues == 0) {
    print("All checks passed. Now installing.")
    install(dependencies = FALSE)
} else {
    print(checks)
}
