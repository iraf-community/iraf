include <ctype.h>

define	VLIGHT	2.997925e18	# Speed of light in Angstroms/sec

# Options
define	OPTION	"|ext|mags|fnu|flam|bands|stars|"
define	EXT	1	# Extinction
define	MAGS	2	# Standard star magnitudes
define	FNU	3	# Standard star fluxes
define	FLAM	4	# Standard star fluxes
define	BANDS	5	# Standard star band passes
define	STARS	6	# Standard stars

# T_LCALIB -- List information in calibration file:
#		1) Extinction vs wavelength
#		2) Magnitude vs wavelength
#		3) F-nu vs wavelength
#		4) F-lambda vs wavelength
#		5) Bandpass vs wavelength
#		6) Standard stars

procedure t_lcalib ()

int	i, nwaves, fd
real	fnu, flam, fnuzero
pointer	sp, str, file, waves, bands, mags, extns

int	getline(), open(), clgwrd()
real	clgetr()
errchk	ext_load, getcalib

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (file, SZ_LINE, TY_CHAR)

	#Switch on the option.
	switch (clgwrd ("option", Memc[str], SZ_LINE, OPTION)) {
	case EXT:
	    call ext_load (waves, extns, nwaves)
	    do i = 1, nwaves {
	        call printf ("%6f %12.5g\n")
		    call pargr (Memr[waves+i-1])
		    call pargr (Memr[extns+i-1])
	    }
	    call ext_free (waves, extns)
	case MAGS:
	    call getcalib (waves, bands, mags, nwaves)
	    do i = 1, nwaves {
	        call printf ("%6f %12.5g\n")
		    call pargr (Memr[waves+i-1])
		    call pargr (Memr[mags+i-1])
	    }
	    call freecalib (waves, bands, mags)
	case FNU:
	    fnuzero = clgetr ("fnuzero")
	    call getcalib (waves, bands, mags, nwaves)
	    do i = 1, nwaves {
		fnu = fnuzero * 10. ** (-0.4 * Memr[mags+i-1])
	        call printf ("%6f %12.5g\n")
		    call pargr (Memr[waves+i-1])
		    call pargr (fnu)
	    }
	    call freecalib (waves, bands, mags)
	case FLAM:
	    fnuzero = clgetr ("fnuzero")
	    call getcalib (waves, bands, mags, nwaves)
	    do i = 1, nwaves {
		fnu = fnuzero * 10. ** (-0.4 * Memr[mags+i-1])
		flam = fnu * VLIGHT /  Memr[waves+i-1] ** 2
	        call printf ("%6f %12.5g\n")
		    call pargr (Memr[waves+i-1])
		    call pargr (flam)
	    }
	    call freecalib (waves, bands, mags)
	case BANDS:
	    call getcalib (waves, bands, mags, nwaves)
	    do i = 1, nwaves {
	        call printf ("%6f %12.5g\n")
		    call pargr (Memr[waves+i-1])
		    call pargr (Memr[bands+i-1])
	    }
	    call freecalib (waves, bands, mags)
	case STARS:
	    call clgstr ("caldir", Memc[str], SZ_LINE)
	    call sprintf (Memc[file], SZ_LINE, "%sstandards.men")
		call pargstr (Memc[str])
	    fd = open (Memc[file], READ_ONLY, TEXT_FILE)
	    while (getline (fd, Memc[file]) != EOF)
		call putline (STDERR, Memc[file]) 
	    call close (fd)
	default:
	    call eprintf ("Unknown option: %s\n")
		call pargstr (Memc[str])
	}

	call sfree (sp)
end
