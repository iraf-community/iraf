# Declare the IMFORT test/demo tasks as CL foreign tasks [MACHDEP].
# Usage: uncomment the appropriate declarations, and type `cl < tasks.cl'.

task $args = "$/usr/iraf/sys/imfort/tasks/args.e $1 $2 $3 $4 $5"
task $hello = "$/usr/iraf/sys/imfort/tasks/hello.e"
task $imcopy = "$/usr/iraf/sys/imfort/tasks/imcopy.e $(*)"
task $imdel = "$/usr/iraf/sys/imfort/tasks/imdel.e $(*)"
task $imren = "$/usr/iraf/sys/imfort/tasks/imren.e $(*)"
task $keyw = "$/usr/iraf/sys/imfort/tasks/keyw.e"
task $minmax = "$/usr/iraf/sys/imfort/tasks/minmax.e $(*)"
task $mkim = "$/usr/iraf/sys/imfort/tasks/mkim.e $(1) $2 $3 $4 $(5)"
task $pcube = "$/usr/iraf/sys/imfort/tasks/pcube.e $(1) $2 $3 $4 $5 $6 $7"
task $phead = "$/usr/iraf/sys/imfort/tasks/phead.e $(1) $2"
task $planck = "$/usr/iraf/sys/imfort/tasks/planck.e"
task $readim = "$/usr/iraf/sys/imfort/tasks/readim.e $(*)"

# task $args = "$args:==\$irafdisk:[iraf.sys.imfort.tasks]args.exe!args $1 $2 $3 $4 $5"
# task $hello = "$hello:==\$irafdisk:[iraf.sys.imfort.tasks]hello.exe!hello"
# task $imcopy = "$imcopy:==\$irafdisk:[iraf.sys.imfort.tasks]imcopy.exe!imcopy $(*)"
# task $imdel = "$imdel:==\$irafdisk:[iraf.sys.imfort.tasks]imdel.exe!imdel $(*)"
# task $imren = "$imren:==\$irafdisk:[iraf.sys.imfort.tasks]imren.exe!imren $(*)"
# task $keyw = "$keyw:==\$irafdisk:[iraf.sys.imfort.tasks]keyw.exe!keyw"
# task $minmax = "$minmax:==\$irafdisk:[iraf.sys.imfort.tasks]minmax.exe!minmax $(*)"
# task $mkim = "$mkim:==\$irafdisk:[iraf.sys.imfort.tasks]mkim.exe!mkim $(*) $2 $3 $4"
# task $pcube = "$pcube:==\$irafdisk:[iraf.sys.imfort.tasks]pcube.exe!pcube $(*) $2 $3 $4 $5 $6 $7"
# task $phead = "$phead:==\$irafdisk:[iraf.sys.imfort.tasks]phead.exe!phead $(1) $2"
# task $planck = "$planck:==\$irafdisk:[iraf.sys.imfort.tasks]planck.exe!planck"
# task $readim = "$readim:==\$irafdisk:[iraf.sys.imfort.tasks]readim.exe!readim $(*)"

keep
