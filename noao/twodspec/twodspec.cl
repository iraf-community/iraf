#{ TWODSPEC -- Two dimensional spectra reduction package.

set	apextract	= "twodspec$apextract/"
set	longslit	= "twodspec$longslit/"
set	multispec 	= "twodspec$multispec/"

package twodspec

task	observatory	= imred$observatory.cl
task	setairmass	= astutil$x_astutil.e
task	setdisp		= onedspec$x_onedspec.e
task	apextract.pkg	= apextract$apextract.cl
task	longslit.pkg	= longslit$longslit.cl
task	multispec.pkg	= multispec$multispec.cl

clbye()
