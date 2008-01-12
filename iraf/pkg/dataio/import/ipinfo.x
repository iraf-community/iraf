include "import.h"


# IP_INFO -- Print information about the binary file.

procedure ip_info (ip, fname, verbose)

pointer	ip					#i task struct pointer
char	fname[ARB]				#i binary file name
int	verbose					#i verbose output?

pointer	sp, buf
pointer	fmt
int	fdb
int	locpr(), fdb_opendb()

pointer	fdb_scan_records()
extern	ip_getop(), ip_dbfcn()

begin
	if (IP_BLTIN(ip) == YES) {
	    call ip_bltin_info (ip, fname, verbose)

	} else if (IP_FORMAT(ip) == IP_NONE) {
	    call ip_prinfo (ip, "User Specified Format", fname, verbose)

	} else {
	    call smark (sp)
	    call salloc (buf, SZ_LINE, TY_CHAR)

	    if (IP_FSYM(ip) == NULL) {
                fdb = fdb_opendb ()
                fmt = fdb_scan_records (fdb, "image_id",
                    locpr(ip_getop), ip, locpr(ip_dbfcn), ip)
	        call fdbgstr (fmt, "id_string", Memc[buf], SZ_LINE)
                call fdb_strip_quote (Memc[buf], Memc[buf], SZ_EXPR)
                call ip_prinfo (ip, Memc[buf], fname, verbose)
	        call fdb_close (fmt)
                call fdb_closedb (fdb)
	    } else {
	        call fdbgstr (IP_FSYM(ip), "id_string", Memc[buf], SZ_LINE)
                call fdb_strip_quote (Memc[buf], Memc[buf], SZ_EXPR)
                call ip_prinfo (ip, Memc[buf], fname, verbose)
	    }

	    call sfree (sp)
	}
end


# IP_PRINFO -- Print information about the binary file.

procedure ip_prinfo (ip, format, fname, verbose)

pointer	ip					#i task struct pointer
char	format					#i format name
char	fname[ARB]				#i binary file name
int	verbose					#i verbose output?

int	i
bool	itob()

define	done_	99

begin
        #call printf ("Input file:\n\t")
	if (verbose == NO) {
	    call printf ("%s: %20t")
	        call pargstr (fname)
	    do i = 1, IP_NDIM(ip) {
	        call printf ("%d ")
		    call pargi (IP_AXLEN(ip,i))
	        if (i < IP_NDIM(ip))
	            call printf ("x ")
	    }
	    call printf ("   \t%s\n")
	        call pargstr (format)

	    # Print out the format comment if any.
#	    if (IP_COMPTR(ip) != NULL) {
#		if (COMMENT(ip) != '\0') {
#	    	    call printf ("%s\n")
#	        	call pargstr (COMMENT(ip))
#		}
#		call strcpy ("\0", COMMENT(ip), SZ_LINE)
#	    }
	    return
	}

	# Print a more verbose description.
	call printf ("%s: %20t%s\n")
	    call pargstr (fname)
	    call pargstr (format)

	# Print out the format comment if any.
	if (IP_COMPTR(ip) != NULL) {
	    if (COMMENT(ip) != '\0') {
	        call printf ("%s\n")
	       	    call pargstr (COMMENT(ip))
	    }
	    call strcpy ("\0", COMMENT(ip), SZ_LINE)
	}

	# Print the image size.
	if (IP_NDIM(ip) > 0) {
	    call printf ("%20tResolution:%38t")
	    do i = 1, IP_NDIM(ip) {
	        call printf ("%d ")
		    call pargi (IP_AXLEN(ip,i))
	        if (i < IP_NDIM(ip))
	            call printf ("x ")
	    }
	    call printf ("\n")
	}

	# Print other information.
	if (PTYPE(ip,1) != NULL) {
	    call printf ("%20tPixel type: %38t%d-bit ")
	    call pargi (8 * IO_NBYTES(PTYPE(ip,1)))
	    switch (IO_TYPE(PTYPE(ip,1))) {
	    case PT_UINT:
	        call printf ("unsigned integer\n")
	    case PT_INT:
	        call printf ("signed integer\n")
	    case PT_IEEE:
	        call printf ("IEEE floating point\n")
	    case PT_NATIVE:
	        call printf ("native floating point\n")
	    default:
	        call printf ("\n")
	    }
	}

	call printf ("%20tPixel storage: %38t%s\n")
            if (BAND_INTERLEAVED(ip))
	        call pargstr ("non-interleaved")
            else if (LINE_INTERLEAVED(ip))
	        call pargstr ("line-interleaved")
            else if (PIXEL_INTERLEAVED(ip))
	        call pargstr ("pixel-interleaved")
            else
	        call pargstr ("unknown")
	call printf ("%20tHeader length: %38t%d bytes\n")
	    call pargi (IP_HSKIP(ip))
	call printf ("%20tByte swapped: %38t%b\n")
	    call pargb (itob(IP_SWAP(ip)))
end


# IP_OBINFO - Print information about the output image contents.

procedure ip_obinfo (ip, imname)

pointer	ip					#i ip struct pointer
char	imname[ARB]				#i image name

int	i, nb

begin
        call printf ("    Output image:\n")

	if (IP_NBANDS(ip) != ERR) {
	    nb = IP_NBANDS(ip)
	    do i = 1, nb {
	        call printf ("\t%s[*,*,%d]:%30t==> %s %s\n")
		    call pargstr (imname)
		    call pargi (i)
		    call pargstr (O_EXPR(ip,i))
		    if (i == 1)
		        call pargstr ("    # outbands expr")
		    else
		        call pargstr (" ")
	    }
	} else {
	    nb = max (IP_AXLEN(ip,3), max (IP_INTERLEAVE(ip), IP_NPIXT(ip)))
	    do i = 1, nb {
	        call printf ("\t%s[*,*,%d]:%30t==> %s%d %s\n")
		    call pargstr (imname)
		    call pargi (i)
		    call pargstr ("b")
		    call pargi (i)
		    if (i == 1)
		        call pargstr ("    # outbands expr")
		    else
		        call pargstr (" ")
	    }
	}

end


# IP_LIST_FORMATS -- List the formats in the database.  The DB is scanned
# and the format name for each record found, as well as the verbose ID 
# string is printed on the standard output.  The file position is left at
# the same place on exit.

procedure ip_list_formats (fd)

int	fd					#i input binary file descriptor

pointer	sp, format, idstr, alias
pointer	fmt, ap[5]
int	i, nsym, cur_offset

int	note()
pointer	stfindall(), fdb_next_rec()

begin
	# Save current file offset.
	cur_offset = note (fd)

	call smark (sp)
	call salloc (format, SZ_EXPR, TY_CHAR)
	call salloc (idstr, SZ_EXPR, TY_CHAR)
	call salloc (alias, SZ_LINE, TY_CHAR)

        # Loop through the database records.
	call seek (fd, BOF)
	fmt = NULL
        call printf ("Format%15tAliases%36tFormat Identification\n")
        call printf ("------%15t-------%36t---------------------\n")			
        repeat {
            fmt = fdb_next_rec (fd)
            if (fmt == NULL) 
                break
            call fdbgstr (fmt, "format", Memc[format], SZ_EXPR)
            call fdbgstr (fmt, "id_string", Memc[idstr], SZ_EXPR)
            call fdb_strip_quote (Memc[idstr], Memc[idstr], SZ_EXPR)
 
	    # Generate a list of aliases for the format.
	    call aclrc (Memc[alias], SZ_LINE)
	    nsym = stfindall (fmt, "alias", ap, 5)
	    if (nsym >= 1) {
	        do i = nsym, 1, -1 {
		    call strcat (Memc[P2C(ap[i])], Memc[alias], SZ_LINE)
		    if (i > 1)
		        call strcat (",", Memc[alias], SZ_LINE)
	        }
	    } else
	        Memc[alias] = EOS

	    # Print the information
            call printf ("%s%15t%.20s%36t%s\n")			
                call pargstr (Memc[format])
                call pargstr (Memc[alias])
                call pargstr (Memc[idstr])

            call fdb_close (fmt)
	    call flush (STDOUT)
        }

	# Restore file offset.
	call seek (fd, cur_offset)

	call sfree (sp)
end
