# ncread <- function(file,
#                     varnames = 1L,
#                     range = NULL,
#                     delta = 0,
#                     ntime = -1,
#                     DatePeriod,
#                     grid_type = NULL,
#                     scale = 1,
#                     offset = 0,
#                     value_range = NULL,
#                     adjust_lon = FALSE,
#                     check_date = FALSE)
# {
#     fid  <- nc_open(file, readunlim = !check_date)
#     on.exit(nc_close(fid)) # make sure closed on error
    
#     if (is.numeric(varnames)) {
#         varId <- as.integer(varnames)
#         if (length(varId) == 1) {
#             if (varId == 0 || varId <= -2) {
#                 varnames <- NULL
#             } else if (varId == -1) {
#                 varnames <- names(fid$var)
#             } else {
#                 varnames <- names(fid$var)[varId]
#             }
#         } else if (length(varId) == 0) {
#             varnames <- NULL
#         } else {
#             varnames <- names(fid$var)[varId]
#         }
#     }
# }
