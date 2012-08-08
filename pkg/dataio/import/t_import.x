include <error.h>
include <ctype.h>
include <evvexpr.h>
include <imhdr.h>
include "import.h"

define	DEBUG	false


# T_IMPORT -- Convert a generic binary raster file to an IRAF image.  The
# binary file is described either from the task parameters, or as an entry
# in a database of known formats.  Access to the database is either by 
# specifying the format explicitly, or by scanning the database and evaluating
# an expression which identifies the format.  Output is either in the form 
# of information about the file to be converted, a list of the file's pixels
# or an IRAF image whose bands are computed from a list of expressions.

procedure t_import ()

pointer	ip					# task structure pointer
int	binfiles				# binary files list pointer
pointer	imfiles					# output image list pointer
int	fdb					# format database descriptor
int	im					# image pointer
pointer	sp, bfname, imname			# local storage
pointer	format, output, fmt, idstr

int	clpopni(), clplen(), imtlen()		# function definitions
int	clgfil(), open()
int	locpr(), imtgetim(), fdb_opendb()
int	ip_fcode(), ip_is_builtin()
pointer	imtopenp(), ip_init(), fdb_scan_records(), immap()

extern	ip_getop(), ip_dbfcn()
errchk	clpopni, clgfil, imtopenp, open, immap

define	done_		99

begin
	call smark (sp)				# local storage
	call salloc (bfname, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (format, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (fmt, SZ_FNAME, TY_CHAR)
	call salloc (idstr, SZ_FNAME, TY_CHAR)

	ip = ip_init () 			# allocate task struct pointer

        call ieemapr (YES, YES)			# enable IEEE NaN mapping
        call ieemapd (YES, YES)

	# Get file names and image lists.
        binfiles = clpopni ("binfiles")
        imfiles = imtopenp ("images")

        # Get the format parameter.
	call clgstr ("format", Memc[format], SZ_FNAME)
        call ip_do_fmtpar (ip, Memc[format])

	# Get task output parameters.
	call ip_gout_pars (ip)

	# See if the image lists match.  If the lists are empty and we're 
	# asked for info, just dump the database and leave.
	if (IP_OUTPUT(ip) != IP_INFO && IP_OUTPUT(ip) != IP_NONE) {
	    if (clplen(binfiles) != imtlen(imfiles) && imtlen(imfiles) != 0) {
                # Clean up and print an error.
	        call clpcls (binfiles)
	        call imtclose (imfiles)
	        call sfree (sp)
	        call error (1, "Input and output lists not the same length.")
	    }
	} else if (IP_OUTPUT(ip) == IP_INFO) {
	    if (clplen(binfiles) == 0 && imtlen(imfiles) == 0) {
		fdb = fdb_opendb ()
		call ip_list_formats (fdb)
		call fdb_closedb (fdb)
		goto done_
	    }
	}

	while (clgfil (binfiles, Memc[bfname], SZ_FNAME) != EOF) {
            iferr (IP_FD(ip) = open (Memc[bfname], READ_ONLY, BINARY_FILE)) {
                call eprintf ("Error opening file '%s'.\n")
		    call pargstr (Memc[bfname])
                break
            }

	    # Process the outbands parameter.
	    call ip_reset_outbands (ip)

	    if (IP_FORMAT(ip) == IP_SENSE) {
		# Scan the database and get symtab pointer to format record.
		fdb = fdb_opendb ()
		call ip_lseek (fdb, BOF)
		IP_FSYM(ip) = fdb_scan_records (fdb, "image_id",
		    locpr(ip_getop), ip, locpr(ip_dbfcn), ip)
		if (IP_FSYM(ip) == NULL) {
		    # Try it byte-swapped.
                    IP_SWAP(ip) = S_ALL
		    call ip_lseek (fdb, BOF)
		    IP_FSYM(ip) = fdb_scan_records (fdb, "image_id",
		        locpr(ip_getop), ip, locpr(ip_dbfcn), ip)
                    IP_SWAP(ip) = NULL

		    if (IP_FSYM(ip) == NULL) {
		        if (IP_OUTPUT(ip) == IP_INFO) {
		            call printf ("%.19s%20tUnrecognized format\n")
			        call pargstr (Memc[bfname])
		            call fdb_closedb (fdb)
			    next
		        } else {
		            call printf (
			    "Unrecognized format. Known formats include:\n\n")
			    call ip_lseek (fdb, BOF)
		            call ip_list_formats (fdb)
		            call fdb_closedb (fdb)
		            break
		        }
		    }
		}
		call fdb_closedb (fdb)
	    }

	    # See if this is a 'builtin' format.
	    if (IP_FSYM(ip) != NULL) {
                call fdbgstr (IP_FSYM(ip), "format", Memc[fmt], SZ_LINE)
                call fdbgstr (IP_FSYM(ip), "id_string", Memc[idstr], SZ_LINE)
                call fdb_strip_quote (Memc[idstr], Memc[idstr], SZ_LINE)
	        IP_BLTIN(ip) = ip_is_builtin (Memc[fmt])
		IP_FCODE(ip) = ip_fcode (Memc[fmt])
	    } else
		IP_BLTIN(ip) = NO


	    if (IP_FORMAT(ip) != IP_NONE) {
		# Evaluate database expressions for this binary file.
		call ip_eval_dbrec (ip)
	    }

	    if (IP_OUTPUT(ip) == IP_INFO) {
		# Just print some information about the file.
		call ip_info (ip, Memc[bfname], IP_VERBOSE(ip))
	  
	    } else {
	        if (IP_OUTPUT(ip) != IP_NONE) {
	            # Get an output image name.
	            if (IP_OUTPUT(ip) == IP_IMAGE) {
		        if (imtgetim (imfiles, Memc[imname], SZ_FNAME) == EOF)
			    call error (1, "Short image list.")
		    } else if (IP_OUTPUT(ip) == IP_LIST) {
                        # Get a temporary image name.
		        call mktemp ("tmp$imp", Memc[imname], SZ_FNAME)
		    }

		    # Open the output image.
		    iferr (im = immap(Memc[imname], NEW_IMAGE, 0)) {
			call erract (EA_WARN)
		  	next
		    }
		    IP_IM(ip) = im

	            # Calculate the size of output image and number of bands.
		    IM_LEN(im,1) = IP_AXLEN(ip,1)
		    IM_LEN(im,2) = IP_AXLEN(ip,2)
		    IM_LEN(im,3) = IP_NBANDS(ip)
		    if (IP_NBANDS(ip) > 1)
		        IM_NDIM(im) = 3
		    else
		        IM_NDIM(im) = IP_NDIM(ip)
		    IM_PIXTYPE(im) = IP_OUTTYPE(ip)
		}

		if (IP_VERBOSE(ip) == YES && IP_OUTPUT(ip) != IP_LIST) {
		    # Print chatter about the conversion.
		    call printf ("%s -> %s\n    ")
			call pargstr (Memc[bfname])
			call pargstr (Memc[imname])
		    call ip_info (ip, Memc[bfname], NO)
            	    call ip_obinfo (ip, Memc[imname])
		    call flush (STDOUT)
		}

		if (IP_BLTIN(ip) == YES) {
		    call ip_prbuiltin (ip, Memc[bfname])
		} else {
		    # This is it, process the binary file.
	            if (BAND_INTERLEAVED(ip))
	                # Input file is band interleaved.
		        call ip_prband (ip, IP_FD(ip), IP_IM(ip), NULL)
	            else if (LINE_INTERLEAVED(ip))
	                # Input file is line interleaved.
		        call ip_prline (ip, IP_FD(ip), IP_IM(ip), NULL)
	            else if (PIXEL_INTERLEAVED(ip))
	                # Input file is pixel interleaved.
		        call ip_prpix (ip, IP_FD(ip), IP_IM(ip), NULL)
	            else 
		        call error (0, "Unrecognized pixel storage.")

        	    if (IP_VERBOSE(ip) == YES) {
            		call eprintf ("    Status: Done          \n")
            		call flush (STDERR)
        	    }
		}


		if (IP_IMHEADER(ip) != NULL && IP_OUTPUT(ip) != IP_NONE)
		    # Copy header info to new image (can contain wcs info)
		    call ip_mkheader (IP_IM(ip), Memc[IP_IMHEADER(ip)])

	        if (IP_OUTPUT(ip) == IP_LIST) {
		    # List the image pixels band by band.
		    call ip_listpix (IP_IM(ip))
		    call imdelete (Memc[imname])
		}

	        if (IP_IM(ip) != NULL)
		    call imunmap (IP_IM(ip)) 	     	# close the output image
	    }

	    call close (IP_FD(ip))
	    if (IP_FORMAT(ip) == IP_SENSE)
		call fdb_close (IP_FSYM(ip)) 		# free format pointer
	}

	# Free task structure ptr and clean up.
	call fdb_close (IP_FSYM(ip))
done_	call ip_free (ip)
	call clpcls (binfiles)
        call imtclose (imfiles)
	call sfree (sp)
end


# IP_INIT -- Initialize the task structure pointers.

pointer procedure ip_init ()

pointer	ptr

begin
	# Allocate task structure pointer.
	iferr (call calloc (ptr, SZ_IMPSTRUCT, TY_STRUCT))
	    call error (0, "Error allocating IMPORT task structure.")

	# Allocate the pixtype, outbands, and buffer struct pointers.
	call calloc (IP_PIXTYPE(ptr), MAX_OPERANDS, TY_POINTER)
	call calloc (IP_OUTBANDS(ptr), MAX_OPERANDS, TY_POINTER)
	call calloc (IP_BUFPTR(ptr), MAX_OPERANDS, TY_POINTER)

	# Initialize some parameters
	IP_IM(ptr) = NULL
	IP_FD(ptr) = NULL
	IP_OFFSET(ptr) = 1
	IP_FLIP(ptr) = FLIP_NONE

	return (ptr)
end


# IP_FREE -- Free the task structure pointers.

procedure ip_free (ip)

pointer	ip					#i task struct pointer

int	i

begin
	# Free pixtype pointers.
	for (i=1; i < IP_NPIXT(ip); i=i+1)
	    call mfree (PTYPE(ip,i), TY_STRUCT)
	call mfree (IP_PIXTYPE(ip), TY_POINTER)

	# Free outbands pointers.
	for (i=1; i < MAX_OPERANDS; i=i+1)
	    call mfree (OBANDS(ip,i), TY_STRUCT)
	call mfree (IP_OUTBANDS(ip), TY_POINTER)

	# Free buffer pointers.
	call mfree (IP_BUFPTR(ip), TY_POINTER)

	if (IP_COMPTR(ip) != NULL)
	    call mfree (IP_COMPTR(ip), TY_CHAR)
	call mfree (ip, TY_STRUCT)
end


# IP_GIN_PARS -- Get the task input file parameters.

procedure ip_gin_pars (ip)

pointer	ip					#i task struct pointer

pointer	sp, dims, bswap, pixtype

int	clgeti()

begin
	call smark (sp)
	call salloc (dims, SZ_FNAME, TY_CHAR)
	call salloc (bswap, SZ_FNAME, TY_CHAR)
	call salloc (pixtype, SZ_FNAME, TY_CHAR)

	# Get the storage parameters.
        IP_HSKIP(ip) = clgeti ("hskip")
        IP_TSKIP(ip) = clgeti ("tskip")
        IP_BSKIP(ip) = clgeti ("bskip")
        IP_LSKIP(ip) = clgeti ("lskip")
        IP_LPAD(ip) = clgeti ("lpad")

        # Process the dims parameter.
	call aclrc (Memc[dims], SZ_FNAME)
        call clgstr ("dims", Memc[dims], SZ_FNAME)
        call ip_do_dims (ip, Memc[dims])

        # Process the bswap parameter.
	call aclrc (Memc[bswap], SZ_FNAME)
        call clgstr ("bswap", Memc[bswap], SZ_FNAME)
        call ip_do_bswap (ip, Memc[bswap])

        # Process the pixtype parameter.
	call aclrc (Memc[pixtype], SZ_FNAME)
        call clgstr ("pixtype", Memc[pixtype], SZ_FNAME)
        call ip_do_pixtype (ip, Memc[pixtype])

	if (IP_NPIXT(ip) > 1)
	    IP_INTERLEAVE(ip) = 0	# composite pixtype, ignore interleave
	else
            IP_INTERLEAVE(ip) = clgeti ("interleave")

	# Do a little sanity checking.
	if (IP_NPIXT(ip) > 1 && IP_NDIM(ip) > IP_NPIXT(ip))
	    call error (1, 
		"Image dimensions don't match `pixtype' specification.")
	if (IP_NPIXT(ip) == 1 && IP_NDIM(ip) > 2 && (IP_INTERLEAVE(ip) != 0 &&
	    IP_INTERLEAVE(ip) != IP_AXLEN(ip,3)))
	        call error (1, 
		    "Dimensions don't match `pixtype' and `interleave' params.")

	if (DEBUG) { call zzi_prstruct ("init inpars", ip) }
	call sfree (sp)
end


# IP_GOUT_PARS -- Get the task output file parameters.

procedure ip_gout_pars (ip)

pointer	ip					#i task struct pointer

pointer	sp, out, otype, obands, imhead
int	btoi(), clgeti()
bool	clgetb(), streq()

begin
	call smark (sp)
	call salloc (out, SZ_FNAME, TY_CHAR)
	call salloc (otype, SZ_FNAME, TY_CHAR)
	call salloc (obands, SZ_FNAME, TY_CHAR)
	call salloc (imhead, SZ_FNAME, TY_CHAR)

	# Get the type of output to do.
	call aclrc (Memc[out], SZ_FNAME)
	call clgstr ("output", Memc[out], SZ_FNAME)
	switch (Memc[out]) {
	case 'i':
	    if (Memc[out+1] == 'n')		# info
	        IP_OUTPUT(ip) = IP_INFO
	    else if (Memc[out+1] == 'm')	# image
	        IP_OUTPUT(ip) = IP_IMAGE
	case 'l':				# list
	    IP_OUTPUT(ip) = IP_LIST
	case 'n':				# none, no
	    IP_OUTPUT(ip) = IP_NONE
	default:
	    call error (2, "Unrecognized output type in 'output'.")
	}

	# Get the output image type.
	call aclrc (Memc[otype], SZ_FNAME)
	call clgstr ("outtype", Memc[otype], SZ_FNAME)
	switch (Memc[otype]) {
	case 'u':
	    IP_OUTTYPE(ip) = TY_USHORT
	case 's':
	    IP_OUTTYPE(ip) = TY_SHORT
	case 'i':
	    IP_OUTTYPE(ip) = TY_INT
	case 'l':
	    IP_OUTTYPE(ip) = TY_LONG
	case 'r':
	    IP_OUTTYPE(ip) = TY_REAL
	case 'd':
	    IP_OUTTYPE(ip) = TY_DOUBLE
	default:
	    IP_OUTTYPE(ip) = NULL
	    call error (2, "Unrecognized output image type in 'outtype'.")
	}

	# Process the outbands parameter.
	#call ip_reset_outbands (ip)

	# Get optional image header info file name.
	call aclrc (Memc[imhead], SZ_FNAME)
	call clgstr ("imheader", Memc[imhead], SZ_FNAME)
	if (streq (Memc[imhead],"")) {
	    IP_IMHEADER(ip) = NULL
	} else {
	    call calloc (IP_IMHEADER(ip), SZ_FNAME, TY_CHAR)
	    call strcpy (Memc[imhead], Memc[IP_IMHEADER(ip)], SZ_FNAME)
	}
        IP_VERBOSE(ip) = btoi (clgetb("verbose"))
        IP_SZBUF(ip) = clgeti ("buffer_size")

	if (DEBUG) { call zzi_prstruct ("init outpars", ip) }
	call sfree (sp)
end


# IP_RESET_OUTBANDS - Initialize the 'outbands' parameter structure to the
# default values.

procedure ip_reset_outbands (ip)

pointer	ip					#i task struct pointer

pointer	sp, obands
int	i

begin
	if (IP_OUTPUT(ip) == IP_INFO)
	    return 

	call smark (sp)
	call salloc (obands, SZ_FNAME, TY_CHAR)

	do i = 1, IP_NBANDS(ip)
	    call ip_free_outbands (OBANDS(ip,i))

	# Process the outbands parameter.
	call aclrc (Memc[obands], SZ_FNAME)
	call clgstr ("outbands", Memc[obands], SZ_FNAME)
        call ip_do_outbands (ip, Memc[obands])

	call sfree (sp)
end


# IP_DO_BSWAP -- Read the byte-swap string an load the ip structure.

procedure ip_do_bswap (ip, bswap)

pointer	ip					#i task struct pointer
char	bswap[ARB]				#i byte swap string

char	ch, flag[SZ_FNAME]
int	sp, i

int	strdic()

begin
	if (DEBUG) { call eprintf("swap='%s'\n");call pargstr (bswap) }

        sp = 1
        IP_SWAP(ip) = NULL
        while (bswap[sp] != EOS) {
            i = 1
            for (ch=bswap[sp];  ch != EOS && ch != ',';  ch=bswap[sp]) {
                flag[i] = ch
                i = i + 1
                sp = sp + 1
            }
            flag[i] = EOS
	    if (DEBUG) { call eprintf("\tflag='%s'\n");call pargstr (flag) }

            switch (strdic (flag, flag, SZ_FNAME, SWAP_STR)) {
            case 1, 2:
                IP_SWAP(ip) = or (IP_SWAP(ip), S_NONE)
            case 3:
                IP_SWAP(ip) = or (IP_SWAP(ip), S_ALL)
            case 4:
                IP_SWAP(ip) = or (IP_SWAP(ip), S_I2)
            case 5:
                IP_SWAP(ip) = or (IP_SWAP(ip), S_I4)
            default:
                break
            }
        }
end


# IP_DO_DIMS -- Parse the 'dims' parameter to get number of axes and dimensions.

procedure ip_do_dims (ip, dims)

pointer	ip					#i task struct pointer
char	dims[ARB]				#i dimension string

char	ch
int	sp, ndim, npix 
int	ctoi()

begin
	if (DEBUG) { call eprintf("dims='%s'\n");call pargstr (dims) }

        ndim = 0
        for (sp=1;  ctoi(dims[1],sp,npix) > 0;  ) {
            ndim = ndim + 1
            IP_AXLEN(ip,ndim) = npix
            for (ch=dims[sp];  IS_WHITE(ch) || ch == ',';  ch=dims[sp])
                sp = sp + 1
        }
	if (ndim == 1)
	    IP_AXLEN(ip,2) = 1
        IP_NDIM(ip) = ndim
end


# IP_DO_FMTPAR -- Given the format parameter, figure out what to do with it.

procedure ip_do_fmtpar (ip, format)

pointer	ip					#i task struct pointer
char	format[ARB]				#i format string

pointer	fsym
int	fd

int	fdb_opendb()
pointer	fdb_get_rec()
bool	streq()

begin
	if (DEBUG) { call eprintf("format='%s'\n");call pargstr(format) }

	IP_FSYM(ip) = NULL
	if (streq(format,"none")) {
	    # Get the task input parameters.
	    IP_FORMAT(ip) = IP_NONE
	    call ip_gin_pars (ip)

	} else if (streq(format,"sense")) {
	    # Set a flag and figure it out from the database later.
	    IP_FORMAT(ip) = IP_SENSE

	} else {
	    # Get a pointer to a symtab entry for the requested format
            IP_FORMAT(ip) = IP_NAME
            fd = fdb_opendb ()
            fsym = fdb_get_rec (fd, format)
            call fdb_closedb (fd)
            if (fsym == NULL) {
		call error (2,"Requested format not found in the database.")
	    } else 
                IP_FSYM(ip) = fsym
	}
end


# IP_DO_PIXTYPE -- Process the pixtype parameter

procedure ip_do_pixtype (ip, pixtype)

pointer	ip					#i task struct pointer
char	pixtype[ARB]				#i pixtype string

int	i, pp, npix, nbytes
pointer	op

int	ctoi()

begin
	if (DEBUG) { call eprintf("pixtype=:%s:\n");call pargstr(pixtype) }

	# Check for a bonehead user.
	if (pixtype[2] == EOS || pixtype[2] == ',') {
	    call error (0, "Invalid `pixtype' parameter: no size given")
	}

	pp = 1
	npix = 0
	nbytes = ERR
	repeat {
	    npix = npix + 1

	    call ip_alloc_operand (PTYPE(ip,npix))
	    op = PTYPE(ip,npix)

	    # Get pixel type.
	    switch (pixtype[pp]) {		
	    case 'b': 
		IO_TYPE(op) = PT_BYTE
	    case 'u': 
		IO_TYPE(op) = PT_UINT
	    case 'i': 
		IO_TYPE(op) = PT_INT
	    case 'r': 
		IO_TYPE(op) = PT_IEEE
	    case 'n': 
		IO_TYPE(op) = PT_NATIVE
	    case 'x': 
		IO_TYPE(op) = PT_SKIP
	    } 
	    pp = pp + 1

	    # Get the number of bytes.
	    i = ctoi (pixtype, pp, IO_NBYTES(op))

	    # Force equivalence of 'b1' and 'u1' pixtypes.
	    if (IO_TYPE(op) == PT_UINT && IO_NBYTES(op) == 1)
		IO_TYPE(op) = PT_BYTE
	    
	    # Get a tag name or create one.
	    if (pixtype[pp] == ',' || pixtype[pp] == EOS) {	# no tag given
		call sprintf (OP_TAG(op), SZ_TAG, "b%d")
		    call pargi (npix)
		if (pixtype[pp] != EOS)
	            pp = pp + 1
	    } else if (pixtype[pp] == ':') {	# get the tag
		pp = pp + 1
		for (i=1; (pixtype[pp] != ',' && pixtype[pp] != EOS) ; i=i+1) {
		    Memc[IO_TAG(op)+i-1] = pixtype[pp]
		    pp = pp + 1
		} 
	        pp = pp + 1
	    }

	    # Make sure all of the pixtypes are the same datatype.
	    if (nbytes != ERR) {
		if (nbytes != IO_NBYTES(op))
		    call error (0, "Pixtypes must all be the same size")
	    } else
		nbytes = IO_NBYTES(op)

	    if (DEBUG) { call zzi_prop (op) }

	} until (pixtype[pp] == EOS)
	IP_NPIXT(ip) = npix
end


# IP_DO_OUTBANDS -- Get the outbands parameter and break it up into a list
# of individual expressions.

procedure ip_do_outbands (ip, outbands)

pointer	ip					#i task struct pointer
char	outbands[ARB]				#i outbands string

pointer	sp, buf
int	i, op, nbands, level

int	strsearch()

begin
	# If there is no outbands parameter specified, warn the user, we'll
	# make something up later.
	IP_USE_CMAP(ip) = YES
	if (outbands[1] == EOS && IP_OUTPUT(ip) != IP_INFO) {
	    call eprintf ("Warning: No 'outbands' parameter specified: ")
	    call eprintf ("Converting all pixels.\n")
	    IP_NBANDS(ip) = ERR
	    return
	}

	call smark (sp)
	call salloc (buf, SZ_EXPR, TY_CHAR)
	call aclrc (Memc[buf], SZ_EXPR)

	if (DEBUG) { call eprintf("outbands='%s'\n");call pargstr(outbands) }

	op = 1
	nbands = 0
	while (outbands[op] != EOS) {
	    level = 0
	    nbands = nbands + 1
	    # Copy expr up to the delimiting comma into a buffer.
	    call aclrc (Memc[buf], SZ_EXPR)
	    for (i=0; i < SZ_EXPR; i = i + 1) {
		if (outbands[op] == '(') {
		    level = level + 1
		    Memc[buf+i] = outbands[op]
		} else if (outbands[op] == ')') {
		    level = level - 1
		    Memc[buf+i] = outbands[op]
                } else if ((outbands[op] == ',' && level == 0) ||
                    outbands[op] == EOS) {
		        Memc[buf+i] = EOS
		        op = op + 1
		        break
		} else if (! IS_WHITE(outbands[op]))
		    Memc[buf+i] = outbands[op]
		op = op + 1
	    }

	    if (Memc[buf] != EOS) {
	        # Save expression to main outbands structure.
	        call ip_alloc_outbands (OBANDS(ip,nbands))
	        call strcpy (Memc[buf], O_EXPR(ip,nbands), SZ_EXPR)

	        if (strsearch(Memc[buf], "red") > 0 ||
	            strsearch(Memc[buf], "green") > 0 ||
	            strsearch(Memc[buf], "blue") > 0)
		        IP_USE_CMAP(ip) = NO

	        # Load the operand struct.
	        call strcpy (Memc[buf], OP_TAG(O_OP(ip,nbands)), SZ_EXPR)

	        if (DEBUG) { call zzi_proband (ip, nbands) }
	    }
	}
	IP_NBANDS(ip) = nbands
	IP_AXLEN(ip,3) = nbands

	call sfree (sp)
end


# IP_ALLOC_OUTBANDS -- Allocate an outbands structure.

procedure ip_alloc_outbands (op)

pointer	op					#i outbands struct pointer

begin
	call calloc (op, LEN_OUTBANDS, TY_STRUCT)
	call calloc (OB_EXPR(op), SZ_EXPR, TY_CHAR)
	call ip_alloc_operand (OB_OP(op))
end


# IP_FREE_OUTBANDS -- Free an outbands structure.

procedure ip_free_outbands (op)

pointer	op					#i outbands struct pointer

begin
	call ip_free_operand (OB_OP(op))
	call mfree (OB_EXPR(op), TY_CHAR)
	call mfree (op, TY_STRUCT)
end


# IP_ALLOC_OPERAND -- Allocate an operand structure.

procedure ip_alloc_operand (op)

pointer	op					#i operand struct pointer

begin
	call calloc (op, LEN_OPERAND, TY_STRUCT)
	call calloc (IO_TAG(op), SZ_FNAME, TY_CHAR)
end


# IP_FREE_OPERAND -- Free an operand structure.

procedure ip_free_operand (op)

pointer	op					#i operand struct pointer

begin
	call mfree (IO_TAG(op), TY_CHAR)
	call mfree (op, TY_STRUCT)
end
