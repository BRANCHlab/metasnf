#' Adaptation of SNFtool's discretisation function
#'
#' @param eigenvectors Eigenvectors
#'
#' @export
discretisation <- function(eigenvectors) {
    normalize <- function(x) x / sqrt(sum(x^2))
    eigenvectors <- t(apply(eigenvectors, 1, normalize))
    if (length(which(is.nan(eigenvectors))) > 0) {
        print(paste0(
            "Warning: a zero-valued eigenvector has resulted in a scaled EVs",
            " with NaN values. This could be the result of hardware-specific",
            " issues in handling extremely precise floating point operations.",
            " To proceed with calculations, NaNs have been converted to 0."))
        eigenvectors[which(is.nan(eigenvectors))] <- 0
    }
    n <- nrow(eigenvectors)
    k <- ncol(eigenvectors)
    r_matrix <- matrix(0, k, k)
    r_matrix[, 1] <- t(eigenvectors[round(n / 2), ])
    mini <- function(x) {
        i <- which(x == min(x))
        return(i[1])
    }
    c <- matrix(0, n, 1)
    for (j in 2:k) {
        c <- c + abs(eigenvectors %*% matrix(r_matrix[, j - 1], k, 1))
        i <- mini(c)
        r_matrix[, j] <- t(eigenvectors[i, ])
    }
    lastobjectivevalue <- 0
    for (i in 1:20) {
        eigendiscrete <-
            discretisationeigenvectordata(eigenvectors %*% r_matrix)
        mat <- t(eigendiscrete) %*% eigenvectors
        svde <- svd(mat)
        u_matrix <- svde[["u"]]
        v_matrix <- svde[["v"]]
        s_matrix <- svde[["d"]]
        ncutvalue <- 2 * (n - sum(s_matrix))
        if (abs(ncutvalue - lastobjectivevalue) < .Machine$double.eps)
            break
        lastobjectivevalue <- ncutvalue
        r_matrix <- v_matrix %*% t(u_matrix)
    }
    return(list(discrete = eigendiscrete, continuous = eigenvectors))
}


#' Adaptation of SNFtool's discretisationeigenvectordata
#'
#' @param eigenvector Eigenvector
#'
#' @export
discretisationeigenvectordata <- function(eigenvector) {
    y <- matrix(0, nrow(eigenvector), ncol(eigenvector))
    maxi <- function(x) {
        i <- which(x == max(x))
        return(i[1])
    }
    j <- apply(eigenvector, 1, maxi)
    y[cbind(seq_len(nrow(eigenvector)), j)] <- 1
    return(y)
}


#' Adaptation of SNFtool's spectral clustering function
#'
#' @param affinity A similarity matrix
#' @param K hyperparameter
#' @param type type of spectral clustering
#'
#' @return RETURN
#'
#' @export
spectral_clustering <- function(affinity, K, type = 3) {
    d <- rowSums(affinity)
    d[d == 0] <- .Machine$double.eps
    diag_d <- diag(d)
    l_matrix <- diag_d - affinity
    if (type == 1) {
        nl_matrix <- l_matrix
    } else if (type == 2) {
        diag_inv <- diag(1 / d)
        nl_matrix <- diag_inv %*% l_matrix
    } else if (type == 3) {
        diag_inv <- diag(1 / sqrt(d))
        nl_matrix <- diag_inv %*% l_matrix %*% diag_inv
    }
    eig <- eigen(nl_matrix)
    res <- sort(abs(eig$values), index.return = TRUE) # the smallest eigenvals
    u_matrix <- eig$vectors[, res$ix[1:K]]
    normalize <- function(x) {
        x / sqrt(sum(x^2))
    }
    if (type == 3) {
        u_matrix <- t(apply(u_matrix, 1, normalize))
    }
    if (length(which(is.nan(u_matrix))) > 0) {
        print(paste0(
            "Warning: a zero-valued eigenvector has resulted in a 'U' matrix",
            " with NaN values. This could be the result of hardware-specific",
            " issues in handling extremely precise floating point operations.",
            " To proceed with calculations, NaNs have been converted to 0."))
        u_matrix[which(is.nan(u_matrix))] <- 0
    }
    eigdiscrete <- discretisation(u_matrix)
    eigdiscrete <- eigdiscrete$discrete
    labels <- apply(eigdiscrete, 1, which.max)
    return(labels)
}
