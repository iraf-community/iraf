include	"identify.h"

# ID_SHOW -- Show parameter information.

procedure id_show (id, file)

pointer	id			# ID pointer
char	file[ARB]		# File

char	line[SZ_LINE]
int	fd

int	open(), ic_geti()
errchk	open()

begin
	fd = open (file, APPEND, TEXT_FILE)

	call sysid (line, SZ_LINE)
	call fprintf (fd, "%s\n")
	call pargstr (line)

	call fprintf (fd, "image %s\n")
	    call pargstr (Memc[ID_IMAGE(id)])
	call fprintf (fd, "nsum %d\n")
	    call pargi (ID_NSUM(id,1))
	switch (ID_FTYPE(id)) {
	case EMISSION:
	    call fprintf (fd, "ftype emission\n")
	case ABSORPTION:
	    call fprintf (fd, "ftype absorption\n")
	case GEMISSION:
	    call fprintf (fd, "ftype gemission\n")
	case GABSORPTION:
	    call fprintf (fd, "ftype gabsorption\n")
	}
	switch (ID_LABELS(id)) {
	case 2:
	    call fprintf (fd, "labels index\n")
	case 3:
	    call fprintf (fd, "labels pixel\n")
	case 4:
	    call fprintf (fd, "labels coords\n")
	case 5:
	    call fprintf (fd, "labels user\n")
	case 6:
	    call fprintf (fd, "labels both\n")
	default:
	    call fprintf (fd, "labels none\n")
	}
	call fprintf (fd, "maxfeatures %d\n")
	    call pargi (ID_MAXFEATURES(id))
	call fprintf (fd, "match %g\n")
	    call pargr (ID_MATCH(id))
	call fprintf (fd, "zwidth %g\n")
	    call pargr (ID_ZWIDTH(id))
	call fprintf (fd, "fwidth %g\n")
	    call pargr (ID_FWIDTH(id))
	call fprintf (fd, "database %s\n")
	    call pargstr (Memc[ID_DATABASE(id)])
	call fprintf (fd, "coordlist %s\n")
	    call pargstr (Memc[ID_COORDLIST(id)])
	call fprintf (fd, "cradius %g\n")
	    call pargr (ID_CRADIUS(id))
	call fprintf (fd, "threshold %g\n")
	    call pargr (ID_THRESHOLD(id))
	call fprintf (fd, "minsep %g\n")
	    call pargr (ID_MINSEP(id))
	if (ID_CV(id) != NULL) {
	    call fprintf (fd, "function = %s\n")
	        call ic_gstr (ID_IC(id), "function", line, SZ_LINE)
		call pargstr (line)
	    call fprintf (fd, "order = %d\n")
		call pargi (ic_geti (ID_IC(id), "order"))
	    call fprintf (fd, "Fit at first pixel = %0.8g\n")
		call pargd (FITDATA(id,1))
	    call fprintf (fd, "Average fit interval = %0.8g\n")
		call pargd ((FITDATA(id,ID_NPTS(id))-FITDATA(id,1))/
		    (ID_NPTS(id)-1))
	}

	call close (fd)
end
