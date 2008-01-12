#{ FITSIO.CL -- Script to set up tasks in the IRAF fitsio package

package         fitsio

task		catfits,
		gftoxdim,
		xdimtogf,
		fitscopy,
		strfits,
		stwfits  = "fitsio$x_fitsio.e"
if (!defpar("stwfits.dadsfile")){
   print("\n****WARNING: outdated parameter file, please: unlearn stwfits")
       beep; sleep(1); beep; sleep(1); beep
}
if (strfits.template.p_value == '') {
   print("\n****WARNING: outdated parameter file, please: unlearn strfits")
       beep; sleep(1); beep; sleep(1); beep
}

task		$geis        = "fitsio$geis.cl"
task		$fits_exampl = "fitsio$fits_exampl.cl"

clbye()
