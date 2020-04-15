my_tm_iso <- function (col = NA, text = "level", size = 0.5, remove.overlap = TRUE, 
          along.lines = TRUE, overwrite.lines = TRUE, group = NA, ...) 
{
  args <- list(...)
  argsL <- args[intersect(names(formals("tm_lines")), names(args))]
  argsT <- args[intersect(names(formals("tm_text")), names(args))]
  do.call("tm_lines", c(list(col = col), argsL)) +
    do.call("tm_text", c(list(text = text, size = size, col=col,.
                              remove.overlap = remove.overlap, along.lines = along.lines,
                              overwrite.lines = overwrite.lines), argsT))
}
