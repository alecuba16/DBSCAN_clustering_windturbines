extract_fit <- function (fit_list, fit_name, return_id = T) {
    for( i in 1:length(fit_list) ) {
        #TODO parche
        if(is.null(fit_list[[i]])) next
        f <- fit_list[[i]]
        if(f$method == fit_name) {
            if(return_id) return(i)
            return(f)
        }
    }
    warning("Fit model ", fit_name, " no found.", "Instead model ", f$method, " returned.")
    return(ifelse(return_id, i, f))
}

# extract_fit_cm <- function (result_h, fit_name) {
#     fit_list <- result_h$fit_list
#     cm_list <- result_h$cm_list
#     for( i in 1:length(fit_list) ) {
#         f <- fit_list[[i]]
#         if(f$method == fit_name) {
#             cm <- cm_list[[i]]
#             return( list(fit = f, cm = cm) )
#         }
#     }
#     warning("Fit model ", fit_name, " no found")
# }