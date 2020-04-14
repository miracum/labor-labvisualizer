# debugging
lib_path <- "/home/user/development/Rpackages/labvisualizer/ume_labvisualizer_deployment/libs/"
utils_path <- "/home/user/development/Rpackages/labvisualizer/ume_labvisualizer_deployment/utilities/"

DIZutils::set_env_vars(paste0(lib_path, "../deploy/.env"))

launch_app(
  db_name = "ORACLE",
  utils_path = utils_path,
  lib_path = lib_path
)