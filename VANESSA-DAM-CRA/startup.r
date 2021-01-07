is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])


if (!require("shiny")) install.packages("shiny",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("WaveletComp")) install.packages("WaveletComp",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("ggetho")) install.packages("ggetho",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("ggplot2")) install.packages("ggplot2",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("zeitgebr")) install.packages("zeitgebr",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("readr")) install.packages("readr",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("damr")) install.packages("damr",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("sleepr")) install.packages("sleepr",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("behavr")) install.packages("behavr",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("dplyr")) install.packages("dplyr",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("shinyalert")) install.packages("shinyalert",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("colourpicker")) install.packages("colourpicker",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("beepr")) install.packages("beepr",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("showtext")) install.packages("showtext",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("shinydashboard")) install.packages("shinydashboard",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("shinycustomloader")) install.packages("shinycustomloader",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("shinythemes")) install.packages("shinythemes",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("shinyWidgets")) install.packages("shinyWidgets",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("shinyhelper")) install.packages("shinyhelper",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("shinyFiles")) install.packages("shinyFiles",dependencies = TRUE, INSTALL_opts = "--no-multiarch")
if (!require("fs")) install.packages("fs",dependencies = TRUE, INSTALL_opts = "--no-multiarch")

if (is.installed("shiny") & is.installed("WaveletComp") & is.installed("ggetho") & is.installed("ggplot2") & is.installed("zeitgebr") & is.installed("readr") & is.installed("damr") &
    is.installed("sleepr") & is.installed("behavr") & is.installed("dplyr") & is.installed("shinyalert") & is.installed("colourpicker") & is.installed("beepr") & is.installed("showtext")
    & is.installed("shinydashboard") & is.installed("shinycustomloader") & is.installed("shinythemes") & is.installed("shinyWidgets") & is.installed("shinyhelper") & is.installed("shinyFiles") & is.installed("fs")){
  print(paste('Initialisation complete! VANESSA-DAM-CRA is now ready for use'), quote=F)
} else {
  print(paste('Error: One or more packages may not have installed correctly'), quote=F)
}