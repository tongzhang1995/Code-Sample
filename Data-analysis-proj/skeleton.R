dir.create("rawdata", showWarnings = TRUE)
dir.create("data", showWarnings = TRUE)
dir.create("report", showWarnings = TRUE)
dir.create("images", showWarnings = TRUE)

download.file(url = "ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/hurdat_format/basin/Basin.NA.ibtracs_hurdat.v03r06.hdat",
              destfile = "rawdata/IBTrACS")
download.file(url = "ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.EP.ibtracs_wmo.v03r06.csv",
              destfile = "rawdata/EastP.csv")
download.file(url = "ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.NA.ibtracs_wmo.v03r06.csv",
              destfile = "rawdata/NorthA.csv")

description = "Description: \n     This project analyzes the frequency, location, duration of storms from East Pacific and North Altantic from 1980s to 2010s. Furthermore, this project reconstructs the trajectory of each storm from 1980s to 2010s.\n\n"
authorInfo = "Author: \n     Tong Zhang, UC Berkeley, Zhiqiang Liao, UC Berkeley\n\n"
organization = "Organization: \n     code: containing all the script files, \n     data: containing csv files that produced by script files, \n     images: containing all the images that produced by script files, \n     rawdata: containing the original files that download from eclipse.ncdc.noaa.gov, \n     report: containing a pdf file that describe all statisical analysis and graphic analysis."
file.create("README.md")
cat(description, file = "README.md", append = FALSE)
cat(authorInfo, file = "README.md", append = TRUE)
cat(organization, file = "README.md", append = TRUE)

