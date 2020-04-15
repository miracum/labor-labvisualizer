labviz_title <- "labVisualizeR"

if (Sys.getenv("LABVIZ_TITLE") != "") {
  labviz_title <- paste(
    Sys.getenv("LABVIZ_TITLE"),
    "labVisualizeR"
  )
}
