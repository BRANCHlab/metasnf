#' Estimate number of clusters for a similarity matrix
#'
#' Calculate eigengap and rotation-cost estimates of the number of clusters
#' to use when clustering a similarity matrix. This function was adapted
#' from `SNFtool::estimateClustersGivenGraph`, but scales up the Laplacian
#' operator prior to eigenvalue calculations to minimize the risk of
#' floating point-related errors.
#'
#' @param W Similarity matrix to calculate number of clusters for.
#' @param NUMC Range of cluster counts to consider among when picking best
#'  number of clusters.
#' @return A list containing the top two eigengap and rotation-cost estimates
#'  for the number of clusters in a given similarity matrix.
#' @export
#' @examples
#' input_dl <- data_list(
#'     list(gender_df, "gender", "demographics", "categorical"),
#'     list(diagnosis_df, "diagnosis", "clinical", "categorical"),
#'     uid = "patient_id"
#' )
#' 
#' sc <- snf_config(input_dl, n_solutions = 1)
#' sol_df <- batch_snf(input_dl, sc, return_sim_mats = TRUE)
#' sim_mat <- sim_mats_list(sol_df)[[1]]
#' estimate_nclust_given_graph(sim_mat)
estimate_nclust_given_graph <- function(W, NUMC = 2:10) {
    # Symmetrize
    W <- (W + t(W))/2
    diag(W) <- 0
    # compute unnormalized Laplacian
    degs <- rowSums(W)
    degs[degs == 0] <- .Machine$double.eps
    D <- diag(degs)
    L <- D - W
    Di <- diag(1 / sqrt(degs))
    ###########################################################################
    # Scaling up the Laplacian is how this function differs from
    # SNFtool::estimateClustersGivenGraph. The scaling reduces the risk of
    # generating zero-valued eigenvectors as a result of floating point
    # errors.
    scaling_factor <- 1e6
    success <- FALSE
    while (!success) {
        scaling_factor <- scaling_factor * 10
        result <- tryCatch(
            {
                L <- Di %*% L %*% Di * scaling_factor
                eigs <- eigen(L)
                eigs_order <- sort(eigs$values, index.return=T)$ix
                eigs$values <- eigs$values[eigs_order]
                eigs$values <- eigs$values / scaling_factor
                list(
                    "L" = L,
                    "eigs" = eigs,
                    "eigs_order" = eigs_order,
                    "success" = TRUE
                )
            }, error = function(e) {
                list(
                    "success" = FALSE
                )
            }
        )
        success <- result$"success"
    }
    L <- result$"L"
    eigs <- result$"eigs"
    eigs_order <- result$"eigs_order"
    eigs$vectors <- eigs$vectors[, eigs_order]
    eigengap <- abs(diff(eigs$values))
    quality <- list()
    for (c_index in 1:length(NUMC)) {
        quality <- tryCatch(
            {
                ck <- NUMC[c_index]
                UU <- eigs$vectors[, 1:ck]
                EigenvectorsDiscrete <- discretisation(UU)
                EigenVectors <- EigenvectorsDiscrete^2
                #MATLAB: sort(EigenVectors,2, 'descend');
                temp1 <- EigenVectors[do.call(order, lapply(1:ncol(EigenVectors),
                     function(i) EigenVectors[, i])), ]
                temp1 <- t(apply(temp1, 1, sort, TRUE))
                quality[[c_index]] <- (1 - eigs$values[ck + 1]) /
                    (1 - eigs$values[ck]) *
                    sum( sum( diag(1 / (temp1[, 1] + .Machine$double.eps) ) %*%
                    temp1[, 1:max(2, ck-1)] ))
                quality
            }, error = function(e) {
                # Ignore this particular NUMC value if the above crashes
                quality[[c_index]] <- Inf
                quality
            }
        )
    }
    #Eigen-gap best two clusters
    t1 <- sort(eigengap[NUMC], decreasing=TRUE, index.return=T)$ix
    K1 <- NUMC[t1[1]]
    K12 <- NUMC[t1[2]]
    #Rotation cost best two clusters
    t2 <- sort(unlist(quality), index.return=TRUE)$ix
    K2 <- NUMC[t2[1]]
    K22 <- NUMC[t2[2]]
    output <- list(
        "Eigen-gap best" = K1,
        "Eigen-gap 2nd best" = K12,
        "Rotation cost best" = K2,
        "Rotation cost 2nd best" = K22
    )
    return (output)
}

#' Internal function for `estimate_nclust_given_graph`
#'
#' Internal function taken from `SNFtool` to use for number of cluster
#' estimation.
#'
#' @keywords internal
#' @param eigenvectors Matrix of eigenvectors.
#' @return "Matrix" class object, intermediate product in spectral clustering.
discretisation <- function(eigenvectors) {
    normalize <- function(x) x / sqrt(sum(x^2))
    eigenvectors = t(apply(eigenvectors,1,normalize))
    n <- nrow(eigenvectors)
    k <- ncol(eigenvectors)
    R <- matrix(0,k,k)
    R[,1] <- t(eigenvectors[round(n/2),])
    mini <- function(x) {
        i <- which(x == min(x))
        return(i[1])
    }
    c <- matrix(0, n, 1)
    for (j in 2:k) {
        c <- c + abs(eigenvectors %*% matrix(R[,j-1],k,1))
        i <- mini(c)
        R[,j] <- t(eigenvectors[i,])
    }
    lastObjectiveValue = 0
    for (i in 1:20) {
        eigenDiscrete <- discretisation_evec_data(eigenvectors %*% R)
        svde <- svd(t(eigenDiscrete) %*% eigenvectors)
        U <- svde[['u']]
        V <- svde[['v']]
        S <- svde[['d']]
        NcutValue <- 2 * (n-sum(S))
        if (abs(NcutValue - lastObjectiveValue) < .Machine$double.eps)
        break
        lastObjectiveValue <- NcutValue
        R <- V %*% t(U)
    }
    return(eigenDiscrete)
}

#' Internal function for `estimate_nclust_given_graph`
#'
#' Internal function taken from `SNFtool` to use for number of cluster
#' estimation.
#'
#' @keywords internal
#' @param eigenvector Matrix of eigenvectors
#' @return "Matrix" class object discretizing provided eigenvector to values 0
#'  or 1.
discretisation_evec_data <- function(eigenvector) {
    Y <- matrix(0, nrow(eigenvector), ncol(eigenvector))
    maxi <- function(x) {
        i <- which(x == max(x))
        return(i[1])
    }
    j <- apply(eigenvector, 1, maxi)
    Y[cbind(1:nrow(eigenvector), j)] <- 1
    return(Y)
}
