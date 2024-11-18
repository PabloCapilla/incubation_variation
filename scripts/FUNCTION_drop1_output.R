##
## wrapper function to use stats::drop1() inside gtsummary::tbl_regression
##
## This eases the reporting of results table enormously! 
drop1_output <- function(x, ...){
    x <- stats::drop1(x, test = "Chisq")
    x$AIC <- NULL
    names(x) <- c("Df", "Chisq", "Pr(>Chisq)")
    output <- broom::tidy(x)
    return(output)  
}
