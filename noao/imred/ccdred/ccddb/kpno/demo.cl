# Demonstration parameter setting script.

# Set package parameters:
ccdred.pixeltype = "real"
ccdred.verbose = yes
ccdred.logfile = "Demo.log"
ccdred.plotfile = "Demo.plots"
ccdred.backup = "B"
ccdred.ssfile = "Demo.subsets"

# Set processing parameters:
ccdproc.fixpix = yes
ccdproc.overscan = yes
ccdproc.trim = yes
ccdproc.zerocor = yes
ccdproc.darkcor = yes
ccdproc.flatcor = yes
ccdproc.illumcor = no
ccdproc.fringecor = no
ccdproc.readcor = no
ccdproc.scancor = no
ccdproc.readaxis = "line"
ccdproc.fixfile = "ccdtest$badpix.dat"
ccdproc.biassec = "image"
ccdproc.trimsec = "image"
ccdproc.zero = ""
ccdproc.dark = ""
ccdproc.flat = ""
ccdproc.illum = ""
ccdproc.fringe = ""
ccdproc.scantype = "shortscan"
ccdproc.nscan = 1
ccdproc.interactive = yes
ccdproc.function = "legendre"
ccdproc.order = 1
ccdproc.sample = "*"
ccdproc.naverage = 1
ccdproc.niterate = 1
ccdproc.low_reject = 3.
ccdproc.high_reject = 3.
ccdproc.grow = 0.

# Set demonstration observation parameters:
observe.ncols = 132
observe.nlines = 100
observe.filter = ""
observe.datasec = "[1:100,1:100]"
observe.trimsec = "[3:98,3:98]"
observe.biassec = "[103:130,*]"
observe.imdata = ""
observe.skyrate = 0.
observe.badpix = "ccdtest$badpix.dat"
observe.biasval = 500.
observe.badval = 500.
observe.zeroval = 100.
observe.darkrate = 1.
observe.zeroslope = 0.01
observe.darkslope = 0.002
observe.flatslope = 3.0000000000000E-4
observe.sigma = 5.
observe.seed = 0
observe.overwrite = no

# Set demonstration subsection readout parameters:
subsection.ncols = 82
subsection.nlines = 50
subsection.ccdsec = "[26:75,26:75]"
subsection.datasec = "[1:50,1:50]"
subsection.trimsec = ""
subsection.biassec = "[51:82,1:50]"
subsection.overwrite = no
