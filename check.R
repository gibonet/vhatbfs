
# git remote add gitlab git@gitlab.com:gibonet/vhatbfs.git

# Push to github and gitlab -----------
# git push -u gitlab master
# git push -u origin master

# git checkout -b progressbar
# git branch
# git push --set-upstream gitlab progressbar
# git push --set-upstream origin progressbar

# devtools --------
library(devtools)

load_all()

document()

check_man()
check()

build()

install(upgrade = "never")


# usethis ---------
library(usethis)

# use_package_doc()

use_testthat()

use_test("n_strata")
use_test("vhat_strata")
use_test("vhat")
use_test("vhat_strata2")
use_test("vhat_strata3")
use_test("vhat_strata4")
use_test("vhat_strata5")
use_test("vhat2")
use_test("vhat3")



# covr::package_coverage() ------------
rstudioapi::restartSession()
library(covr)
cov <- package_coverage()
cov
# as.data.frame(cov)
# report()
report(cov, file = "vhatbfs-report.html")


# pkgdown ---------
# library(pkgdown)
# build_site()
