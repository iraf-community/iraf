# MKCIRCLE -- Fraction of Gaussian profile going through a circular aperture
# of specified diameter in units of the profile FWHM.

real	d, logd

printf ("## CIRCLE -- Fraction of Gaussian profile going through a")
printf (" cicular\n## aperture as a function of the diameter in units")
printf (" of FWHM.\n\n")

printf ("%4.2f %5.3f\n", 0, 0)
for (logd=-1; logd<=0.6; logd=logd+0.1) {
    d = 10.**logd
    z = d * 0.8325546111577
    z = 1 - exp (-(z * z))
    printf ("%4.2f %5.3f\n", d, z)
}
