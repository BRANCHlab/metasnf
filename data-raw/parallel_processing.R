###############################################################################
# Done in SickKids HPC
###############################################################################
# Load the package
library(metasnf)
library(abcdutils)
library(here)
library(readr)

proc_path <- path_maker(here(paste0("data/abcd/results/processed")))
fig_path <- path_maker(here(paste0("data/abcd/results/figures")))

# Setting up the data
data_list <- generate_data_list(
    list(abcd_cort_t, "cortical_thickness", "neuroimaging", "continuous"),
    list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "continuous"),
    list(abcd_subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(abcd_income, "household_income", "demographics", "continuous"),
    list(abcd_pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "patient"
)

time_data <- data.frame(
    snf_rows = as.numeric(),
    processes = as.numeric(),
    user_time = as.numeric(),
    system_time = as.numeric(),
    elapsed_time = as.numeric()
)

for (nproc in c(1, 24)) {
    for (rows in c(1, 12, 24, 48, 96, 240)) {
        # 1. Make settings matrix
        settings_matrix <- generate_settings_matrix(
            data_list,
            nrow = rows,
            max_k = 40,
            seed = 42
        )
        # 1. Make settings matrix
        start <- proc.time()
        solutions_matrix <- batch_snf(
            data_list,
            settings_matrix,
            processes = nproc
        )
        end <- proc.time()
        time <- end - start
        time_data <- rbind(
            time_data,
            data.frame(
                snf_rows = rows,
                processes = nproc,
                user_time = time["user.self"] + time["user.child"],
                sys_time = time["sys.self"] + time["sys.child"],
                elapsed_time = time["elapsed"]
            )
        )
    }
}

rownames(time_data) <- NULL

write.csv(time_data, proc_path("hpc_parallel_profiling.csv", TRUE))

library(benchmarkme)

get_cpu()

# vendor_id
# [1] "GenuineIntel"
#
# $model_name
# [1] "Intel Xeon E312xx (Sandy Bridge, IBRS update)"
#
# $no_of_cores
# [1] 30

get_ram()

# 129 GB

sessionInfo()

# R version 4.3.1 (2023-06-16)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: AlmaLinux 8.8 (Sapphire Caracal)

###############################################################################
# On local machine
###############################################################################
# Load the package
library(metasnf)
library(abcdutils)

# Setting up the data
data_list <- generate_data_list(
    list(abcd_cort_t, "cortical_thickness", "neuroimaging", "continuous"),
    list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "continuous"),
    list(abcd_subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(abcd_income, "household_income", "demographics", "continuous"),
    list(abcd_pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "patient"
)

time_data <- data.frame(
    snf_rows = as.numeric(),
    processes = as.numeric(),
    user_time = as.numeric(),
    system_time = as.numeric(),
    elapsed_time = as.numeric()
)

for (nproc in c(1, 4)) {
    for (rows in c(1, 12, 24, 48, 96, 240)) {
        # 1. Make settings matrix
        settings_matrix <- generate_settings_matrix(
            data_list,
            nrow = rows,
            max_k = 40,
            seed = 42
        )
        # 1. Make settings matrix
        start <- proc.time()
        solutions_matrix <- batch_snf(
            data_list,
            settings_matrix,
            processes = nproc
        )
        end <- proc.time()
        time <- end - start
        time_data <- rbind(
            time_data,
            data.frame(
                snf_rows = rows,
                processes = nproc,
                user_time = time["user.self"] + time["user.child"],
                sys_time = time["sys.self"] + time["sys.child"],
                elapsed_time = time["elapsed"]
            )
        )
    }
}


rownames(time_data) <- NULL

proc_path <- path_maker(here(paste0("data/abcd/results/processed")))

write.csv(time_data, proc_path("local_parallel_profiling.csv", TRUE))

library(benchmarkme)

get_cpu()

# $vendor_id
# [1] "GenuineIntel"
#
# $model_name
# [1] "Intel(R) Core(TM) i5-4460 CPU @ 3.20GHz"
#
# $no_of_cores
# [1] 4

get_ram()

# 16.7 GB

sessionInfo()

# R version 4.2.3 (2023-03-15)
# Platform: x86_64-redhat-linux-gnu (64-bit)
# Running under: Fedora Linux 37 (Sapphire Caracal)
###############################################################################
# Comparisons

library(ggplot2)


hpc_times <- read_csv(proc_path("2023_12_06_hpc_parallel_profiling.csv"))[, -1]

local_times <- read_csv(proc_path("2023_12_05_local_parallel_profiling.csv"))[, -1]

local_times$"processes"[local_times$"processes" == 24] <- 4

local_times

h <- 6
w <- 10

local_times |># {~
    ggplot() +
    geom_point(
        aes(
            x = snf_rows,
            y = user_time,
            colour = factor(processes)
        ),
        size = 3
    ) +
    geom_line(
        aes(
            x = snf_rows,
            y = user_time,
            colour = factor(processes)
        )
    ) +
    coord_cartesian(
        ylim = c(0, 125)
    ) +
    labs(
        title = "Local machine",
        x = "SNF rows",
        y = "User time (s)",
        colour = "procs"
    ) +
    theme_bw() +
    theme(
        text = element_text(size = 20)
    )# ~}
ggsave(# {~
    fig_path("local_parallel_profiling.png", TRUE),
    width = w,
    height = h
)# ~}
hpc_times |># {~
    ggplot() +
    geom_point(
        aes(
            x = snf_rows,
            y = user_time,
            colour = factor(processes)
        ),
        size = 3
    ) +
    geom_line(
        aes(
            x = snf_rows,
            y = user_time,
            colour = factor(processes)
        )
    ) +
    coord_cartesian(
        ylim = c(0, 125)
    ) +
    labs(
        title = "HPC",
        x = "SNF rows",
        y = "User time (s)",
        colour = "procs"
    ) +
    theme_bw() +
    theme(
        text = element_text(size = 20)
    )# ~}
ggsave(# {~
    fig_path("hpc_parallel_profiling.png", TRUE),
    width = w,
    height = h
)# ~}
hpc_times$"location" <- "hpc"# {~
local_times$"location" <- "local"

all_times <- rbind(hpc_times, local_times)

all_times

all_times$"condition" <- paste0(all_times$"location", "_", all_times$"processes", "_proc")

all_times |>
    ggplot() +
    geom_line(
        aes(
            x = snf_rows,
            y = user_time,
            group = condition,
            colour = factor(processes),
            linetype = factor(processes)
        )
    ) +
    geom_point(
        aes(
            x = snf_rows,
            y = user_time,
            group = condition,
            colour = factor(processes),
            shape = location
        ),
        size = 4
    ) +
    coord_cartesian(
        ylim = c(0, 15),
        xlim = c(0, 200)
    ) +
    labs(
        title = "HPC and Local",
        x = "SNF rows",
        y = "User time (s)",
        colour = "procs",
        linetype = "procs"
    ) +
    theme_bw() +
    theme(text = element_text(size=20))# ~}
ggsave(# {~
    fig_path("parallel_profiling.png", TRUE),
    width = w,
    height = h
)# ~}

###############################################################################

#time_data
#
#library(future.apply)
#plan(multisession)
#
#library(progressr)
#handlers(global = TRUE)
#handlers("progress", "beepr")
#
#my_fcn <- function(xs) {
#  p <- progressor(along = xs)
#  future_lapply(xs, function(x, ...) {
#    Sys.sleep(6.0-x)
#    p(sprintf("x=%g", x))
#    sqrt(x)
#  })
#}
#
#my_fcn(1:5)
## / [================>-----------------------------]  40% x=2
