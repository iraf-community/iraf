# MKSLIT -- Fraction of Gaussian profile going through a slit aperture
# of specified width and height in units of the profile FWHM.

real	logx, logy, t, erfcc, xval, yval

printf ("## SLIT -- Fraction of Gaussian profile going through a")
printf (" slit\n## aperture as a function of the width and height")
printf (" in units of FWHM.\n\n")

printf ("%4.2f %4.2f %5.3f\n", 0, 0, 0)
for (logx=-1; logx<=0.6; logx=logx+0.2) {
    x = 10.**logx
    printf ("%4.2f %4.2f %5.3f\n", x, 0, 0)
}
for (logy=-1; logy<=0.6; logy=logy+0.2) {
    y = 10.**logy
    z = y * 0.8325546111577
    t = 1. / (1. + 0.5 * z)
    erfcc = t * exp (-z * z - 1.26551223 + t * (1.00002368 +
	t * (0.37409196 + t * (0.09678418 + t * (-0.18628806 +
	t * (0.27886807 + t * (-1.13520398 + t * (1.48851587 +
	t * (-0.82215223 + t * 0.17087277)))))))))
    yval = (1 - erfcc)
    printf ("%4.2f %4.2f %5.3f\n", 0, y, 0)
    for (logx=-1; logx<=0.6; logx=logx+0.2) {
	x = 10.**logx
	z = x * 0.8325546111577
	t = 1. / (1. + 0.5 * z)
	erfcc = t * exp (-z * z - 1.26551223 + t * (1.00002368 +
	    t * (0.37409196 + t * (0.09678418 + t * (-0.18628806 +
	    t * (0.27886807 + t * (-1.13520398 + t * (1.48851587 +
	    t * (-0.82215223 + t * 0.17087277)))))))))
	xval = (1 - erfcc)
	z = xval * yval
	printf ("%4.2f %4.2f %5.3f\n", x, y, z)
    }
}
