include "rvpackage.h"
include "rvflags.h"
include "rvcomdef.h"
include "rvfilter.h"

.help filtpars
.nf ___________________________________________________________________________
FILTPARS  - Support routines for the 'filtpars' named external pset.  

	This file include routines for opening/closing the filter structure 
as well as command handling.  Command handling is limited to changing the 
parameter values or resetting them to the default values.  Routines included
here are as follows.

	  	       filt_open (rv)
	              filt_close (rv)
	           filt_get_pars (rv)
		  filt_parupdate (rv)
		    filt_unlearn (rv)
		       filt_show (rv, fd)
		      filt_colon (rv, cmdstr)
		    cmd_filttype (rv)
		       cmd_cuton (rv)
		      cmd_cutoff (rv)
		      cmd_fullon (rv)
		     cmd_fulloff (rv)

	The 'cmd_' prefix indicates that the routine is called from a colon 
command to either print the current value or set the new value for that
field.  Other routines should be self-explanatory

.endhelp _____________________________________________________________________

#  Default values for the FILTERPARS pset
define	DEF_FILT_TYPE		RAMP		# Filter type
define	DEF_CUTON		0		# Filter cuton component no.
define	DEF_CUTOFF		0		# Filter cutoff component no.
define	DEF_FULLON		0		# Filter fullon component no.
define	DEF_FULLOFF		0		# Filter fulloff component no.


# FILT_OPEN - Open the Process parameters substructure.  This is used to
# reduce the size of the already over-burdened main RV struct.

procedure filt_open (rv)

pointer	rv					#I RV struct pointer

pointer	filt

begin
	iferr (call calloc (filt, SZ_FILTSTRUCT, TY_STRUCT))
	    call error (0, "Error allocating sub-structure RV_FILTP.")

	RV_FILTP(rv) = filt

	# Initlialize the values
	call filt_get_pars (rv)			# set to defaults
end


# FILT_CLOSE - Close the process structure.

procedure filt_close (rv)

pointer	rv					#I RV struct pointer

begin
	call mfree (RV_FILTP(rv), TY_STRUCT)
end


# FILT_GET_PARS - Read the filter pset into the struct.

procedure filt_get_pars (rv)

pointer	rv					#U RV struct pointer

pointer	fp, clopset()
char	buffer[SZ_FNAME]
int	clgpseti(), cod_filttype()
errchk  clopset

begin
	fp = clopset ("filtpars")

	RVF_CUTON(rv) = clgpseti (fp, "cuton")
	RVF_CUTOFF(rv) = clgpseti (fp, "cutoff")
	RVF_FULLON(rv) = clgpseti (fp, "fullon")
	RVF_FULLOFF(rv) = clgpseti (fp, "fulloff")

	call clgpset (fp, "f_type", buffer, SZ_LINE)
	RVF_FILTTYPE(rv) = cod_filttype (buffer)
	RVF_LASTKEY(rv) = 'p'			# plot power spec. by default

	call clcpset (fp)
end


# FILT_PARUPDATE - Update the parameter file with the current values in the
# filter structure.

procedure filt_parupdate (rv)

pointer	rv					#I RV struct pointer

pointer	sp, bp
pointer	fp, clopset()

begin
	call smark (sp)
	call salloc (bp, SZ_LINE, TY_CHAR)

	# Update filter params
	fp = clopset ("filtpars")

	call clppseti (fp, "cuton", RVF_CUTON(rv))
	call clppseti (fp, "cutoff", RVF_CUTOFF(rv))
	call clppseti (fp, "fullon", RVF_FULLON(rv))
	call clppseti (fp, "fulloff", RVF_FULLOFF(rv))

	call nam_filttype (rv, Memc[bp])
	call clppset (fp, "f_type", Memc[bp])

	call clcpset (fp)
	call sfree (sp)
end


# FILT_UNLEARN -- Reset all of the filter parameters to their default values.

procedure filt_unlearn (rv)

pointer	rv					#I RV struct pointer

begin
	RVF_FILTTYPE(rv) = DEF_FILT_TYPE	# RAMP
	RVF_CUTON(rv) = DEF_CUTON		# 0
	RVF_CUTOFF(rv) = DEF_CUTOFF		# 0
	RVF_FULLON(rv) = DEF_FULLON		# 0
	RVF_FULLOFF(rv) = DEF_FULLOFF		# 0

	RVF_LASTKEY(rv) = 'f'
end


# FILT_SHOW - Show the current filter parameters.

procedure filt_show (rv, fd)

pointer	rv			#I RV struct pointer
pointer	fd			#I output file descriptor

pointer	sp, bp

begin
	if (fd == NULL)
	    return

	call smark (sp)
	call salloc (bp, SZ_LINE, TY_CHAR)

	call fprintf (fd, "%6tFilterpars PSET Values\n")
	call fprintf (fd, "%6t----------------------\n\n")

	# Print the filter info
	call fprintf (fd, "Cuton %25t= %-10d\n")
	    call pargi (RVF_CUTON(rv))
	call fprintf (fd, "Cutoff %25t= %-10d\n")
	    call pargi (RVF_CUTOFF(rv))
	call fprintf (fd, "Fullon %25t= %-10d\n")
	    call pargi (RVF_FULLON(rv))
	call fprintf (fd, "Fulloff %25t= %-10d\n")
	    call pargi (RVF_FULLOFF(rv))

	call nam_filttype (rv, Memc[bp])
	call fprintf (fd, "Filter type %25t= '%-.10s'\n")
	    call pargstr (Memc[bp])

	call fprintf (fd, "\n\n")
	call sfree (sp)
end


# FILT_COLON -- Process the FILTERPARS task colon commands.

procedure filt_colon (rv, cmdstr)

pointer	rv				#I pointer to the RV structure
char	cmdstr[SZ_LINE]			#I command string

pointer	sp, cmd, buf
int	strdic()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)

	# Unpack the keyword from the string and look it up in the
	# dictionary.  Switch on command and call the appropriate routines.

	switch (strdic(Memc[cmd], Memc[cmd], SZ_FNAME, FILT_KEYWORDS)) {
	case FILT_FILT_TYPE:
	    # Function type of filter
	    call cmd_filttype (rv)

	case FILT_CUTON:
	    # Cuton frequency component
	    call cmd_cuton (rv)

	case FILT_CUTOFF:
	    # Cutoff frequency component
	    call cmd_cutoff (rv)

	case FILT_FULLON:
	    # Fullon frequency component
	    call cmd_fullon (rv)

	case FILT_FULLOFF:
	    # Fulloff frequency component
	    call cmd_fulloff (rv)
	}

	call sfree (sp)
end


# CMD_CUTOFF - Set/Show the cutoff wavenumber for the filter.

procedure cmd_cutoff (rv)

pointer rv

int	ival, nscan()

begin
	call gargi (ival)
	if (nscan() == 2) {
	    RVF_CUTOFF(rv) = ival
	    if (RV_AUTODRAW(rv) == YES && RV_FILTER(rv) != NONE)
		RV_NEWGRAPH(rv) = YES
	} else {
	    call printf ("filtpars.cutoff = %d")
	        call pargi (RVF_CUTOFF(rv))
	}
end


# CMD_CUTON - Set/Show the cuton wavenumber for the filter.

procedure cmd_cuton (rv)

pointer	rv

int	ival, nscan()

begin
	call gargi(ival)
	if (nscan() == 2) {
	    RVF_CUTON(rv) = ival
	    if (RV_AUTODRAW(rv) == YES && RV_FILTER(rv) != NONE)
		RV_NEWGRAPH(rv) = YES
	} else {
	    call printf ("filtpars.cuton = %d")
	        call pargi (RVF_CUTON(rv))
	}
end


# CMD_FILTTYPE - Set the type of filter to be used in FFT correlation.

procedure cmd_filttype (rv)

pointer	rv

pointer	sp, buf, bp
int	cod_filttype()

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (bp, SZ_LINE, TY_CHAR)

	call gargstr (Memc[buf], SZ_FNAME)
	if (Memc[buf] != EOS) {
	    RVF_FILTTYPE(rv) = cod_filttype (Memc[buf+1])
	    if (RV_AUTODRAW(rv) == YES && RV_FILTER(rv) != NONE)
		RV_NEWGRAPH(rv) = YES
	} else {
	    call nam_filttype (rv, Memc[bp])
	    call printf ("filtpars.filttype = '%s'")
	    	call pargstr (Memc[bp])
	}

	call sfree (sp)
end


# CMD_FULLOFF - Set/Show the wavenumber at which the filter falls to zero.

procedure cmd_fulloff (rv)

pointer	rv

int	ival, nscan()

begin
	call gargi (ival)
	if (nscan() == 2) {
	    RVF_FULLOFF(rv) = ival
	    if (RV_AUTODRAW(rv) == YES && RV_FILTER(rv) != NONE)
		RV_NEWGRAPH(rv) = YES
	} else {
	    call printf ("filtpars.fulloff = %d")
	        call pargi (RVF_FULLOFF(rv))
	}
end


# CMD_FULLON - Set/Show the wavenumber at which the filter reaches full value.

procedure cmd_fullon (rv)

pointer	rv

int	ival, nscan()

begin
	call gargi (ival)
	if (nscan() == 2) {
	    RVF_FULLON(rv) = ival
	    if (RV_AUTODRAW(rv) == YES && RV_FILTER(rv) != NONE)
		RV_NEWGRAPH(rv) = YES
	} else {
	    call printf ("filtpars.fullon = %d")
	        call pargi (RVF_FULLON(rv))
	}
end
