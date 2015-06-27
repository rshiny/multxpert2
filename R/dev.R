

devtools::use_build_ignore("dev.R")
devtools::use_build_ignore("gatkeeperFunctions.R")
file.exists("~/.ssh/id_rsa.pub")

find . -name "*.Rd" -print | xargs sed -i 's/\\title{/\\title{\n something\n/'
