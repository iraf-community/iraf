#{ TWODSPEC -- Two dimensional spectra reduction package.

set	apextract	= "twodspec$apextract/"
set	longslit	= "twodspec$longslit/"
set	multispec 	= "twodspec$multispec/"

package twodspec

task	apextract.pkg	= apextract$apextract.cl
task	longslit.pkg	= longslit$longslit.cl
#task	multispec.pkg	= multispec$multispec.cl

clbye
