include <error.h>
include <fset.h>
include <imhdr.h>
include <imio.h>
include <time.h>
include <mach.h>
include "export.h"


# EX_WHEADER - Write the output file header information.

procedure ex_wheader (ex, outfile)

pointer	ex				#i task struct pointer
char	outfile[ARB]			#i output file name

pointer	sp, tfile, buf, cbuf
int	file_type, nchars

int	fd, open(), access(), strlen()
long	fsize, fstatl()

errchk	open, access

begin
	if (EX_HEADER(ex) == HDR_SHORT || EX_HEADER(ex) == HDR_LONG) {

	    call smark (sp)
	    call salloc (tfile, SZ_PATHNAME, TY_CHAR)
	    call salloc (buf, SZ_LINE, TY_CHAR)
	    call salloc (cbuf, SZ_LINE, TY_CHAR)
	    call aclrc (Memc[buf], SZ_LINE)
	    call aclrc (Memc[cbuf], SZ_LINE)

	    # Write the generic header.
	    call mktemp ("tmp$ex", Memc[tfile], SZ_PATHNAME)
	    fd = open (Memc[tfile], NEW_FILE, TEXT_FILE)
	    call ex_mkheader (ex, fd)
	    call close (fd)

	    if (EX_FORMAT(ex) != FMT_LIST)
	        fd = open (Memc[tfile], READ_ONLY, BINARY_FILE)
	    else
	        fd = open (Memc[tfile], READ_ONLY, TEXT_FILE)
	    fsize = fstatl (fd, F_FILESIZE) * SZB_CHAR
	    nchars = fsize + 27 #+ fsize/10
	    call sprintf (Memc[buf], SZ_LINE, "format = EXPORT\nhdrsize = %d\n")
	        call pargi (nchars)
	    nchars = strlen (Memc[buf])
	    if (EX_FD(ex) != STDOUT && EX_FORMAT(ex) != FMT_LIST) {
	        call strpak (Memc[buf], Memc[cbuf], nchars)
	        call write (EX_FD(ex), Memc[cbuf], nchars/SZB_CHAR)
	        call fcopyo (fd, EX_FD(ex))
	        call close (fd)
	    } else {
		call fprintf (EX_FD(ex), "%s")
		    call pargstr (Memc[buf])
	        if (EX_FORMAT(ex) == FMT_LIST)
	            call fcopyo (fd, EX_FD(ex))
		else
	            call fcopy (Memc[tfile], "STDOUT")
		
	        call close (fd)
	    }

	    call delete (Memc[tfile])
	    call sfree (sp)

	} else if (EX_HEADER(ex) == HDR_USER) {
	    # Copy user file to output.
	    iferr {
		# If the user header is a text file we need to reopen the
		# output file so the copy is done correctly.  Afterwards
		# we'll reopen it as a binary file.
	        if (access (HDRFILE(ex), 0, TEXT_FILE) == YES) {
		    file_type = TEXT_FILE
		    call close (EX_FD(ex))
		    EX_FD(ex) = open (outfile, APPEND, file_type)
	        } else
		    file_type = BINARY_FILE
	    
		fd = open (HDRFILE(ex), READ_ONLY, file_type)
	        call fcopyo (fd, EX_FD(ex)) 
		if (EX_FD(ex) != STDOUT)
		    call close (fd)

	        if (file_type == TEXT_FILE) {
		    if (EX_FD(ex) != STDOUT)
		    call close (EX_FD(ex))
		    if (EX_FORMAT(ex) != FMT_LIST)
		        EX_FD(ex) = open (outfile, APPEND, BINARY_FILE)
		}
	    } then
		call error (2, "Error writing user header.")
	}
end


# EX_MKHEADER - Write the generic binary file header.  Since we need to
# output the size we'll write out just the trailer part to the temp file
# and copy it to the real output file later.

procedure ex_mkheader (ex, fd)

pointer	ex					#i task struct pointer
int	fd					#i temp file descriptor

long	clktime()		# seconds since 00:00:00 10-Jan-80
int	tm[LEN_TMSTRUCT]	# broken down time structure

begin
	# Write the time stamp string.
	call brktime (clktime(0), tm)
	call fprintf (fd, "date = '%d/%d/%d'\n")
	    call pargi (TM_MDAY(tm))
	    call pargi (TM_MONTH(tm))
	    call pargi (TM_YEAR(tm))

	# ... and the rest of the header
	call fprintf (fd, "ncols = %d\n")	# image dimensions
	    call pargi (EX_OCOLS(ex))
	call fprintf (fd, "nrows = %d\n")
	    call pargi (EX_OROWS(ex))
	call fprintf (fd, "nbands = %d\n")
	    call pargi (EX_NEXPR(ex))

	call fprintf (fd, "datatype = '%s'\n")	# pixel type
	    call pargstr (Memc[EX_OTPTR(ex)])

	call fprintf (fd, "outbands = '%s'\n")	# outbands expressions
	    call pargstr (Memc[EX_OBPTR(ex)])

	call fprintf (fd, "interleave = %d\n")	# pixel interleave type
	    call pargi (EX_INTERLEAVE(ex))

	call fprintf (fd, "bswap = %s\n")	# byte swapping flag
	    switch (EX_BSWAP(ex)) {
	    case S_NONE: 	call pargstr ("none")
	    case S_ALL: 	call pargstr ("all")
	    case S_I2: 		call pargstr ("i2")
	    case S_I4: 		call pargstr ("i4")
	    }

	if (EX_HEADER(ex) == HDR_LONG)
	    call ex_wimhdr (ex, fd) 		# write image headers

	# Terminate header.
	call fprintf (fd, "end\n")
end


# EX_WIMHDR - Write the image header information.  Include the headers if this
# is a verbose output.

procedure ex_wimhdr (ex, fd)

pointer	ex					#i task struct pointer
int	fd					#i temp file descriptor

pointer sp, lbuf, ip, im
int     i, in, ncols, min_lenuserarea 
int     stropen(), getline(), envgeti()

define  USER_AREA       Memc[($1+IMU-1)*SZ_STRUCT + 1]
define  LMARGIN         4

begin
        call smark (sp)
        call salloc (lbuf, SZ_LINE, TY_CHAR)

	do i = 1, EX_NIMAGES(ex) {

	    im = IO_IMPTR(IMOP(ex,i))
	    call fprintf (fd, "image%d = '%s'\n")
		call pargi (i)
		call pargstr (IM_HDRFILE(im))
	    call fprintf (fd, "header%d {\n")
		call pargi (i)

            # Open user area in header.
            min_lenuserarea = (LEN_IMDES+IM_LENHDRMEM(im)-IMU) * SZ_STRUCT - 1
            in = stropen (USER_AREA(im), min_lenuserarea, READ_ONLY)
            ncols = envgeti ("ttyncols") - LMARGIN

            # Copy header records to the output, stripping any trailing
            # whitespace and clipping at the right margin.

            while (getline (in, Memc[lbuf]) != EOF) {
                for (ip=lbuf;  Memc[ip] != EOS && Memc[ip] != '\n';  ip=ip+1)
                    ;
                while (ip > lbuf && Memc[ip-1] == ' ')
                    ip = ip - 1
                if (ip - lbuf > ncols)
                    ip = lbuf + ncols 
                Memc[ip] = '\n'
                Memc[ip+1] = EOS
            
                call putline (fd, "    ")
                call putline (fd, Memc[lbuf])
            }

	    call fprintf (fd, "}\n")
        }

        call close (in)
        call sfree (sp)
end
