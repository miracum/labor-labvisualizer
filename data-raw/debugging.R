# debugging
lib_path <- "/home/user/development/Rpackages/labvisualizer/ume_labvisualizer_deployment/libs/"
utils_path <- "/home/user/development/Rpackages/labvisualizer/ume_labvisualizer_deployment/utilities/"

envs <- readLines(paste0(lib_path, "../deploy/.env"))

args <- sapply(envs, FUN = function(x) {
  l <- unlist(strsplit(x, "="))
  env <- paste(l[2:length(l)], collapse = "")
  names(env) <- unlist(strsplit(x, "="))[[1]]
  return(env)
}, USE.NAMES = F)

do.call(Sys.setenv, as.list(args))

launch_app(
  db_name = "ORACLE",
  utils_path = utils_path,
  lib_path = lib_path
)