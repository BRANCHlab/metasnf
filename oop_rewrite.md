# To-do list JSS OOP rewrite (happening on branch "oop")

- [ ] jss specific
    - [ ] (manuscript specific): overview and summary of implemented methodology (SNF & meta clustering)
    - [ ] (manuscript specific): overview and summary of software design / main functions
    - [x] replace high level `print` with low level `cat`
    - [x] typo "mixed_dist" inconsistent with "mix_dist" in settings matrix
    - [x] settings matrix snf_scheme column is undocumented?
    - [ ] provide code examples (@examples roxygen tag) for every complex function, especially batch_snf
    - [x] p-values should follow unipolar colour palette
    - [x] only run shiny_annotator() `if (interactive())`
    - [ ] add benchmarking code for figure 19
    - [x] strip "save_heatmap" /ggsave /etc. calls in vignettes and instead make a mention of how to use it
- [ ] general
    - [x] replace stops/warnings/alerts with cli wrapper functions
    - [x] replace "subjectkey" and "subject" phrasing with more generic "uid" and "observation"
    - [ ] typo checking
    - [ ] more explicit documentation (nothing like, return settings_matrix A settings matrix)
    - [ ] remove initial noun from @return tags
- [ ] testing (complete coverage may be overkill?)

- subjects -> observations
- more examples for all the functions
- vignette checks
