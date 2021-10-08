gmm.fit = function(x, k, niter=500) {
    structure(
        .Call(C_gmm_fit, data.matrix(x), as.integer(k), as.integer(niter))
        , class="gmm")
}

predict.gmm = function(object, x, ...) {
    stopifnot(inherits(object, "gmm"))
    stopifnot(ncol(x) == ncol(object$mixture.centers))
    if (!is.null(colnames(x)) && !is.null(colnames(object$mixture.centers)))
    {
        if (length(setdiff(colnames(x), colnames(object$mixture.centers))))
            stop("unrecognized feature names in new data")
        if (length(setdiff(colnames(object$mixture.centers), colnames(x))))
            stop("missing features in new data")
    }
    .Call(C_gmm_predict, object, data.matrix(x))
}

simulate.gmm = function(object, nsim=1, seed=NULL, ...) {
    stopifnot(inherits(object, "gmm"))
    p = length(object$mixture.variances)
    mix = sample(1:object$k, nsim, replace=TRUE, prob=object$mixture.weights)
    t(matrix(rnorm(nsim*p, t(object$mixture.centers[mix, ]), 
        sqrt(object$mixture.variances)), p, nsim))
}