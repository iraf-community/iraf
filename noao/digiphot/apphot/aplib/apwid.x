include "../lib/apphot.h"

# define the #N, #U and #K id strings

define	ID_NSTR	"#N%4tIMAGE%24tXINIT%34tYINIT%44tID%49tCOORDS%69tLID%80t\\\n"
define	ID_USTR	"#U%4timagename%24tpixels%34tpixels%44t##%49tfilename%69t##%80t\\\n"
define	ID_FSTR	"#F%4t%%-23s%24t%%-10.2f%34t%%-10.2f%44t%%-5d%49t%%-20s%69t%%-5d%80t \n"
define	ID_WSTR "%-23.23s%24t%-10.2f%34t%-10.2f%44t%-5d%49t%-20.20s%69t%-5d%80t%c\n"


# AP_IDHDR -- Print the id header banner strings.

procedure ap_idhdr (ap, fd)

pointer	ap		# apphot descriptor
int	fd		# output file descriptor

begin
	if (fd == NULL)
	    return
	call fprintf (fd, ID_NSTR)
	call fprintf (fd, ID_USTR)
	call fprintf (fd, ID_FSTR)
	call fprintf (fd, "#\n")
end


# AP_WID -- Procedure to write the id record to an apphot output file.

procedure ap_wid (ap, fd, xpos, ypos, id, lid, lastchar)

pointer	ap		# pointer to apphot structure
int	fd		# output file descriptor
real	xpos		# x position
real	ypos		# y position
int	id		# id of the star
int	lid		# list number
int	lastchar	# last character in record

pointer	sp, imname, clname 

begin
	if (fd == NULL)
	    return

	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (clname, SZ_FNAME, TY_CHAR)

	# Print description of object.
	call apstats (ap, IMNAME, Memc[imname], SZ_FNAME)
	call apstats (ap, CLNAME, Memc[clname], SZ_FNAME)
	if (Memc[clname] == EOS)
	    call strcpy ("nullfile", Memc[clname], SZ_FNAME)

	call fprintf (fd, ID_WSTR)
	    call pargstr (Memc[imname])
	    call pargr (xpos)
	    call pargr (ypos)
	    call pargi (id)
	    call pargstr (Memc[clname])
	    call pargi (lid)
	    call pargi (lastchar)

	call sfree (sp)
end
