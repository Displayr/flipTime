
## Call Sys.timezone when the package is loaded to suppress spurious warnings
## about `/etc/timezone` not being accessible and
## "running command 'timedatectl' had status 1" in Displayr and Q
## fixes DS-2159
## Work-around for https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17421
.onLoad(suppressWarnings(Sys.timezone()))
