
input_list <- readRDS("Input_List.rds")
solutions <- readRDS("ClusterSolutions.rds")

devtools::load_all()

calc_nmis(input_list, solutions[5:7, ], verbose = TRUE, processes=1)

