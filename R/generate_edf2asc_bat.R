#' Takes edf files and converts them to asc
#' This code was written by Dr. Holger Mitterer
#'@param homeDir dir of files
#'@param subDirs end location of files
#'@param deleteExisting overwrite file - set to false
#'@export

generate_edf2asc_bat <- function(homeDir, subDirs, deleteExisting  = F)
{
  cat("",file = "edf2asc_convert.bat", append = F)
  for (i in subDirs)
  {
    filesInDir = dir(paste(mainDir, i, sep = "/"), pattern = "edf$")
    if (length(filesInDir) == 1)
    {
      fullPathFile = paste(mainDir,i,filesInDir, sep = "/")
      fullPathFile = gsub("/", "\\\\", fullPathFile)
      if (deleteExisting) {
        fullPathFile_asc= gsub("edf$", "asc", fullPathFile)
        cat("del ", fullPathFile_asc, "\n", file = "edf2asc_convert.bat", append = T)
      }
      cat("edf2asc ", fullPathFile, "\n", file = "edf2asc_convert.bat", append = T)
    }
  }
  cat("batch file generated for", paste(subDirs, collapse = ","), "\nin:", getwd())
}
