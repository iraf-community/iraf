# Package script task for the PHOTCAL package.
#{ PHOTCAL package definition


# Load other packages

images	  # some of the preprocessors require hselect
lists	  # the mkimsets script requires the unique task
proto	  # some of the preprocessors require fields 

# Define photcal package

package photcal

# PHOTCAL tasks

task	apfile,
	catalog,
	config,
	chkconfig,
	evalfit,
	fitparams,
	imgroup,
	invertfit,
	obsfile,
	mkphotcors	= "photcal$x_photcal.e"

# PHOTCAL scripts

task	mkapfile	= "photcal$mkapfile.cl"
task	mkcatalog	= "photcal$mkcatalog.cl"
task	mkconfig	= "photcal$mkconfig.cl"
task	mkimsets	= "photcal$mkimsets.cl"
task	mknobsfile	= "photcal$mknobsfile.cl"
task	mkobsfile	= "photcal$mkobsfile.cl"

# PTOOLS tasks

task	istable,
	tbcrename,
	tbkeycol,
	txdump		= "ptools$x_ptools.e"

# PTOOLS scripts which depend on PTOOLS and TTOOLS tasks

task	pdump		= "ptools$pdump.cl"

# TTOOLS tasks linked into the photcal package

task	tbdump		= "ptools$tbdump.cl"

hidetask catalog, config, imgroup
hidetask istable, tbcrename, tbkeycol, txdump, tbdump, pdump

clbye()
