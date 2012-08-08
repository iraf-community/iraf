include <error.h>
include <ctype.h>
include <evvexpr.h>
include <mach.h>
include <fset.h>
include <imhdr.h>
include "export.h"

define	DEBUG 		false


# T_EXPORT -- Task entry.  Convert one or more IRAF image to an output binary
# file. Output may be a raw binary raster, with or without header information,
# a pixel listing, or a specified (supported) format.  Arbitrary expressions
# may be applied to the input images before conversion.

procedure t_export ()

pointer	ex				# task struct pointer
pointer	sp, blist, bfname		# stack pointers
pointer	imname[MAX_OPERANDS]
pointer	imlist				# image list pointer
pointer	im				# image descriptor
int	binlist				# binary file list pointer
int	imdim				# dimensionality of images
int	imtype				# datatype of images
int	i

pointer	ex_init(), immap()
int	ex_getpars()
int	clgfil(), access(), fntopnb()
int	imtlen(), imtopenp(), open(), imtgetim()
bool	streq()

errchk	open, immap, ex_chkimlist

define	quit_	99

begin
	# Allocate local stack storage.
	call smark (sp)
	call salloc (bfname, SZ_FNAME, TY_CHAR)
	call salloc (blist, SZ_FNAME, TY_CHAR)
	call aclrc (Memc[blist], SZ_FNAME)
	call aclrc (Memc[bfname], SZ_FNAME)
	do i = 1, MAX_OPERANDS {
	    call salloc (imname[i], SZ_FNAME, TY_CHAR)
	    call aclrc (Memc[imname[i]], SZ_FNAME)
	}

	# Get the image and file lists.
	imlist = imtopenp ("images")
	call clgstr ("binfiles", Memc[blist], SZ_FNAME)
	if (!streq("", Memc[blist]) && !streq(" ", Memc[blist])) {
	    binlist = fntopnb (Memc[blist], YES)
	    iferr (call ex_chkimlist (imlist, binlist, imdim, imtype)) {
	        call imtclose (imlist)
	        call clpcls (binlist)
	        call sfree (sp)
	        call erract (EA_FATAL)
	    }
	    call clprew (binlist)
	} else {
	    binlist = -1
	    iferr (call ex_chkimlist (imlist, binlist, imdim, imtype)) {
	        call imtclose (imlist)
	        call sfree (sp)
	        call erract (EA_FATAL)
	    }
	}
	call imtrew (imlist)		# rewind the list ptrs

	# Allocate structure and get the task parameters.
	ex = ex_init ()
	EX_IMDIM(ex) = imdim
	EX_IMTYPE(ex) = imtype
	if (ex_getpars (ex) != OK)
	    goto quit_

	# Do some last minute error checking.
	if (imtlen(imlist) < EX_NIMAGES(ex))
	    call error (0, "Too many image operands in expression list")

	# Start processing the files.
	repeat {

	    # Open the output binary file.
	    if (binlist > 0) {
		if (clgfil(binlist, Memc[bfname], SZ_FNAME) == EOF)
		    break
	    
	        # If this is a builtin format append the format suffix if it's
	        # not already there and then open the file.
	        call ex_mkfname (ex, Memc[bfname])
	        if (access (BFNAME(ex), 0, 0) == YES) {
		    call eprintf ("Output file `%s' already exists.\n")
		        call pargstr (BFNAME(ex))
		    goto quit_
	        }
		if (EX_FORMAT(ex) != FMT_LIST)
	            EX_FD(ex) = open (BFNAME(ex), NEW_FILE, BINARY_FILE)
		else
	            EX_FD(ex) = open (BFNAME(ex), NEW_FILE, TEXT_FILE)
	    } else {
		call strcpy ("STDOUT", Memc[bfname], SZ_FNAME)
		call strcpy ("STDOUT", BFNAME(ex), SZ_FNAME)
	        EX_FD(ex) = STDOUT
	    }

	    # Open the image pointers.  If no outbands expressions were given
	    # we're converting only one image, but we need to fake up the
	    # image operands.
	    if (EX_NIMAGES(ex) == EX_UNDEFINED) {
		i = imtgetim(imlist, Memc[imname[1]], SZ_FNAME)
	        im = immap (Memc[imname[1]], READ_ONLY, 0)
		EX_NIMAGES(ex) = 1
		EX_NEXPR(ex) = max (1, IM_LEN(im,3))
		EX_NCOLS(ex) = IM_LEN(im,1)
		EX_NLINES(ex) = IM_LEN(im,2)
	 	EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), BAND_STORAGE)
		if (EX_IMDIM(ex) == 0)
		    EX_IMDIM(ex) = IM_NDIM(im)
		if (EX_IMTYPE(ex) == 0) {
		    EX_IMTYPE(ex) = IM_PIXTYPE(im)
		    EX_OUTTYPE(ex) = IM_PIXTYPE(im)
		}

		# Fake the expressions and break out the operands.
	        do i = 1, EX_NEXPR(ex) {
	    	    call ex_alloc_outbands (OBANDS(ex,i))
		    call sprintf (O_EXPR(ex,i), SZ_LINE, "b%d")
			call pargi (i)
		}
		call ex_parse_operands (ex)
		if (EX_NEXPR(ex) > 1) {
		    EX_OUTFLAGS(ex) = and (EX_OUTFLAGS(ex), not(BAND_STORAGE))
		    EX_OUTFLAGS(ex) = and (EX_OUTFLAGS(ex), not(LINE_STORAGE))
		    EX_OUTFLAGS(ex) = or  (EX_OUTFLAGS(ex), PIXEL_STORAGE)
		}
		IO_IMPTR(IMOP(ex,1)) = im

		# Print some status stuff so we know what's being converted.
	 	call eprintf ("%s -> %s\n")
		    call pargstr (Memc[imname[1]])
		    call pargstr (BFNAME(ex))
	    } else {
	  	EX_NLINES(ex) = 0
	        do i = 1, EX_NIMAGES(ex) {
		    if (imtgetim(imlist, Memc[imname[i]], SZ_FNAME) == EOF)
		        call error (1, "Short image list")
		    im = immap (Memc[imname[i]], READ_ONLY, 0)
		    EX_NCOLS(ex) = IM_LEN(im,1)
		    EX_NLINES(ex) = max (EX_NLINES(ex), IM_LEN(im,2))
		    IO_IMPTR(IMOP(ex,i)) = im
		    if (EX_IMDIM(ex) == 0)
		        EX_IMDIM(ex) = IM_NDIM(im)
		    if (EX_IMTYPE(ex) == 0) {
		        EX_IMTYPE(ex) = IM_PIXTYPE(im)
		        EX_OUTTYPE(ex) = IM_PIXTYPE(im)
		    }

		    # Print some status stuff so we know what's being converted.
	 	    call eprintf ("%s")
		        call pargstr (Memc[imname[i]])
		    if (i < EX_NIMAGES(ex))
	 	        call eprintf (",")
		    else {
	 	        call eprintf (" -> %s\n")
			    call pargstr (BFNAME(ex))
		    }
		    call flush (STDERR)
	        }
	    }

	    # For 3-D data we only have one image, but we may have multiple
	    # image operands (bands) within the image.  If this is the case
	    # then copy the image pointer to the remaining operand structs.
	    if (EX_NIMAGES(ex) == 1 && EX_NIMOPS(ex) > 1) {
		do i = 2, EX_NIMOPS(ex)
		    IO_IMPTR(IMOP(ex,i)) = IO_IMPTR(IMOP(ex,1))
	    }

	    # Now patch up any zscale calls in the expression string.
	    do i = 1, EX_NEXPR(ex)
	        call ex_patch_zscale (ex, i)

	    # Now that we have all the image information and things are going
	    # well, compute the size of the output image.
	    call ex_outsize (ex)

	    # If we're being verbose the print some more information on the
	    # input images and output file.
	    if (EX_VERBOSE(ex) == YES) 
		call ex_prinfo (ex, imname)

	    # Write the header now if this is a generic raster.
	    if (EX_HEADER(ex) != HDR_NONE && EX_FORMAT(ex) != FMT_BUILTIN)
		call ex_wheader (ex, Memc[bfname])

	    # Process the image.
	    call ex_process_image (ex)

	    # Unmap the image pointer(s).
	    do i = 1, EX_NIMAGES(ex) {
		im = IO_IMPTR(IMOP(ex,i))
		if (im != NULL)
		    call imunmap (im)
	    }

	    # Close the output file descriptor.
	    if (EX_FD(ex) != NULL)
	        call close (EX_FD(ex))

	    # If we created a temp image then delete that now.
	    if (EX_TIMPTR(ex) != NULL)
		call imdelete (TIMNAME(ex))

	    if (binlist < 0)
		break
	}

	# Clean up.
quit_	call imtclose (imlist)
	if (binlist > 0)
	    call clpcls (binlist)
	call sfree (sp)
end


# EX_INIT - Initialize the export task structure.

pointer procedure ex_init ()

pointer	ex

begin
	# Allocate the task structure pointer.
	iferr (call calloc (ex, SZ_EXPSTRUCT, TY_STRUCT))
	    call error (0, "Error allocating EXPORT task structure.")

	# Allocate internal pointers.
	call calloc (EX_HDRPTR(ex), SZ_FNAME, TY_CHAR)
	call calloc (EX_CMPTR(ex), SZ_FNAME, TY_CHAR)
	call calloc (EX_LUTPTR(ex), SZ_FNAME, TY_CHAR)
	call calloc (EX_BFNPTR(ex), SZ_FNAME, TY_CHAR)
	call calloc (EX_OBANDS(ex), MAX_OBEXPR, TY_STRUCT)
	call calloc (EX_IMOPS(ex), MAX_OPERANDS, TY_STRUCT)
	call calloc (EX_OTPTR(ex), SZ_LINE, TY_CHAR)
	call calloc (EX_OBPTR(ex), SZ_EXPSTR, TY_CHAR)

	# Initialize some parameters.
	EX_OUTFLAGS(ex)   = NULL
	EX_NLUTEL(ex)     = INDEFI
	EX_NCOLORS(ex)    = CMAP_SIZE
	EX_PSDPI(ex)      = EPS_DPI
	EX_PSSCALE(ex)    = EPS_SCALE
	EX_BRIGHTNESS(ex) = 0.5
	EX_CONTRAST(ex)   = 1.0

	return (ex)
end


# EX_FREE - Free the export task structure.

procedure ex_free (ex)

pointer	ex				#i task struct pointer

int	i

begin
	# Free internal pointers.
	call mfree (EX_HDRPTR(ex), TY_CHAR)
	call mfree (EX_CMPTR(ex),  TY_CHAR)
	call mfree (EX_LUTPTR(ex), TY_CHAR)
	call mfree (EX_BFNPTR(ex), TY_CHAR)
	call mfree (EX_TIMPTR(ex), TY_CHAR)
	call mfree (EX_OTPTR(ex),  TY_CHAR)
	call mfree (EX_OBPTR(ex),  TY_CHAR)

        # Free outbands pointers.
        for (i=1; i < MAX_OBEXPR; i=i+1)
            call ex_free_outbands (OBANDS(ex,i))
        call mfree (EX_OBANDS(ex), TY_POINTER)

        # Free operand pointers.
        for (i=1; i < MAX_OPERANDS; i=i+1)
            call ex_free_operand (IMOP(ex,i))
        call mfree (EX_IMOPS(ex), TY_POINTER)

	# Free the colormap.
	if (EX_CMAP(ex) != NULL)
	    call mfree (EX_CMAP(ex), TY_CHAR)

	call mfree (ex, TY_STRUCT)
end


# EX_GETPARS - Get the task parameters.

int procedure ex_getpars (ex)

pointer	ex				#i task struct pointer

pointer	sp, format, header, bswap
pointer	outtype, outbands

int	ex_chkpars(), clgeti(), btoi()
bool	clgetb()

errchk	ex_do_format, ex_do_header, ex_do_bswap
errchk	ex_do_outtype, ex_do_outbands

begin
	call smark (sp)
	call salloc (format, SZ_FNAME, TY_CHAR)
	call salloc (header, SZ_FNAME, TY_CHAR)
	call salloc (bswap, SZ_FNAME, TY_CHAR)
	call salloc (outtype, SZ_LINE, TY_CHAR)
	call salloc (outbands, SZ_EXPSTR, TY_CHAR)

	call aclrc (Memc[format], SZ_FNAME)
	call aclrc (Memc[header], SZ_FNAME)
	call aclrc (Memc[bswap], SZ_FNAME)
	call aclrc (Memc[outtype], SZ_FNAME)
	call aclrc (Memc[outbands], SZ_EXPSTR)

	# Get the string valued parameters.
	call clgstr ("format", Memc[format], SZ_FNAME)
	call clgstr ("header", Memc[header], SZ_FNAME)
	call clgstr ("bswap", Memc[bswap], SZ_FNAME)
	call clgstr ("outtype", Memc[outtype], SZ_LINE)
	call strcpy (Memc[outtype], Memc[EX_OTPTR(ex)], SZ_LINE)
	call clgstr ("outbands", Memc[outbands], SZ_EXPSTR)
	call strcpy (Memc[outbands], Memc[EX_OBPTR(ex)], SZ_EXPSTR)

	# Get the simple params.
	EX_INTERLEAVE(ex) = clgeti ("interleave")
	EX_VERBOSE(ex) = btoi (clgetb ("verbose"))

	# Process the parameter values, due error checking
	iferr {
	    call ex_do_format (ex, Memc[format])
	    call ex_do_header (ex, Memc[header])
	    call ex_do_bswap (ex, Memc[bswap])
	    call ex_do_outtype (ex, Memc[outtype])
	    call ex_do_outbands(ex, Memc[outbands])
	} then {
	    call sfree (sp)
	    call erract (EA_FATAL)
	}

	call sfree (sp)

        if (DEBUG) { 
	    call eprintf("ex_format=%d\n"); call pargi (EX_FORMAT(ex)) 
	    call eprintf("ex_bswap=%d\n"); call pargi (EX_BSWAP(ex)) 
	    call eprintf("ex_outtype=%d\n"); call pargi (EX_OUTTYPE(ex)) 
	    call eprintf("ex_header=%d\n"); call pargi (EX_HEADER(ex)) 
	}

	# Do a sanity check on the params so we can exit now if needed.
	return (ex_chkpars (ex))
end


# EX_CHKPARS - Check task parameters to be sure we have a valid conversion.

int procedure ex_chkpars (ex)

pointer	ex				#i task struct pointer

int	flags, exb_chkpars()

begin
	flags = EX_OUTFLAGS(ex)
	if (EX_FORMAT(ex) == FMT_BUILTIN && !bitset(EX_OUTFLAGS(ex),OF_MKCMAP)){
	    return (exb_chkpars(ex))
	} else {
	    if (bitset (flags, OF_CMAP)) {
		call error (1, "Colormap creation not supported for raw output")
		return (ERR)
	    }
	}

	return (OK)
end


# EX_CHKIMLIST - Check the image list to be sure it's valid.

procedure ex_chkimlist (images, files, ndim, type)

int	images				#i image list pointer
int	files				#i binary files list pointer
int	ndim				#o dimensionality of images
int	type				#o datatype of images

pointer	im, sp, imname
int	dim

pointer	immap()
int	imtlen(), imtgetim(), clplen()

errchk	immap

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call aclrc (Memc[imname], SZ_FNAME)

	# Get dimension of first image.
	if (imtgetim (images, Memc[imname], SZ_FNAME) != EOF) {
	    im = immap (Memc[imname], READ_ONLY, 0)
	    ndim = IM_NDIM(im)
	    type = IM_PIXTYPE(im)
	    call imunmap (im)
	} else
	    call error (0, "Unexpected EOF in image list.\n")

	# Loop over remaining images in the list.
	while (imtgetim (images, Memc[imname], SZ_FNAME) != EOF) {
	    im = immap (Memc[imname], READ_ONLY, 0)
	    dim = IM_NDIM(im)
	    call imunmap (im)
	    if (dim != ndim)
		call error (0, "Images must all be the same dimension.\n")
	}

	if (files > 0) {
	    if (ndim == 3 && (imtlen (images) != clplen (files)))
	        call error (0, "No. of images must equal no. of output files\n")
	}

	call sfree (sp)
end


# EX_OUTSIZE -  Compute the output file dimensions.  We don't require that
# the expressions all evaluate to same length so we'll patch up the expr
# string to pad with zeroes to the maximum width.

procedure ex_outsize (ex)

pointer	ex				#i task struct pointer

pointer	sp, expr
int	i, ip, imnum, plev
int	height, maxlen, maxhgt
char	ch

pointer	op, ex_evaluate()
int	ctoi(), strncmp()

begin
	call smark (sp)
	call salloc (expr, SZ_EXPSTR, TY_CHAR)
	call aclrc (Memc[expr], SZ_EXPSTR)

        call ex_getpix (ex, 1)
        maxlen = 0
        do i = 1, EX_NEXPR(ex) {            # get length of each expr
            op = ex_evaluate (ex, O_EXPR(ex,i))
            O_WIDTH(ex,i) = O_LEN(op)
            maxlen = max (maxlen, O_WIDTH(ex,i))
            call evvfree (op)
        }	

        do i = 1, EX_NEXPR(ex) {            # patch expressions

            if (O_WIDTH(ex,i) <= 1) {
		# If the width is 1 we have a constant, meaning we only want
		# one line on output and need to pad the constant.
		O_HEIGHT(ex,i) = 1
		O_WIDTH(ex,i) = maxlen
		call aclrc (Memc[expr], SZ_EXPSTR)
                call sprintf (Memc[expr], SZ_EXPSTR, "repl(%s,%d)")
                    call pargstr (O_EXPR(ex,i))
                    call pargi (maxlen)
		call strcpy (Memc[expr], O_EXPR(ex,i), SZ_EXPSTR)

            } else if (O_WIDTH(ex,i) <= maxlen) {
		# If this is a vector expression then look for image operands.
		# The 'height' of the expression will be the largest height
		# of the found operands.

		ip = 1
		maxhgt = 1
		call strcpy (O_EXPR(ex,i), Memc[expr], SZ_EXPSTR)
		repeat {
		    while (Memc[expr+ip-1] != 'i' && Memc[expr+ip-1] != 'b' && 
			Memc[expr+ip-1] != EOS)
		            ip = ip + 1
		    if (Memc[expr+ip-1] == EOS)
			break
		    if (IS_DIGIT(Memc[expr+ip])) {
		        ip = ip + 1
                        if (ctoi (Memc[expr], ip, imnum) == 0)
                            call error (4, "ex_outsize: can't parse operand")
			maxhgt = max (maxhgt,IM_LEN(IO_IMPTR(IMOP(ex,imnum)),2))

		    } else if (strncmp(Memc[expr+ip-1], "block", 5) == 0) {
		        ip = ip + 1

			# This is a "block" function call to fill a vertical
			# area.  The syntax is "block(constant, width, height)"
			# so get the height argument.
			while (Memc[expr+ip] != '(')
		            ip = ip + 1
			plev = 0
			repeat {		# skip over 1st arg
		            ip = ip + 1
		            ch = Memc[expr+ip]
			    if (ch == '(')  plev = plev + 1
			    if (ch == ')')  plev = plev - 1
			    if (ch == ',' && plev == 0)
				break
			}
		        # Should be the start of arg2.
		        ip = ip + 2		# should be the width
                        if (ctoi (Memc[expr], ip, height) == 0)
                            call error (4, "ex_outsize: block() syntax error")
		        ip = ip + 1		# should be the height
                        if (ctoi (Memc[expr], ip, height) == 0)
                            call error (4, "ex_outsize: block() syntax error")
			
			maxhgt = max (maxhgt, height)
		    } else
		        ip = ip + 1
		}
	        O_HEIGHT(ex,i) = maxhgt

                if (O_WIDTH(ex,i) < maxlen) {
		    call aclrc (Memc[expr], SZ_EXPSTR)
                    call sprintf (Memc[expr], SZ_EXPSTR, "%s//repl(0,%d)")
                        call pargstr (O_EXPR(ex,i))
                        call pargi (maxlen - O_WIDTH(ex,i))
		    call strcpy (Memc[expr], O_EXPR(ex,i), SZ_EXPSTR)
		    O_WIDTH(ex,i) = maxlen
		}
            }

	    if (DEBUG) { call eprintf ("%d: len=%d maxlen=%d height=%d\n")
		call pargi(i) ; call pargi(O_WIDTH(ex,i))
		call pargi(maxlen) ; call pargi (O_HEIGHT(ex,i)) }

        }
        EX_OCOLS(ex) = maxlen

	# Now compute the total number of rows.
	if (EX_IMDIM(ex) == 3) {
	    if (!bitset (EX_OUTFLAGS(ex), PIXEL_STORAGE)) {
	        if (EX_NEXPR(ex) > 1 && bitset (EX_OUTFLAGS(ex), OF_BAND))
	            EX_OROWS(ex) = IM_LEN(IO_IMPTR(IMOP(ex,1)),3)*EX_NLINES(ex)
	        else
	            EX_OROWS(ex) = EX_NLINES(ex)
	    } else
	        EX_OROWS(ex) = EX_NLINES(ex)
	} else if (bitset (EX_OUTFLAGS(ex), OF_BAND)) {
	    EX_OROWS(ex) = 0
	    do i = 1, EX_NEXPR(ex)
		EX_OROWS(ex) = EX_OROWS(ex) + O_HEIGHT(ex,i)
	} else
	    EX_OROWS(ex) = EX_NLINES(ex)

	call sfree (sp)
end


# EX_DO_FORMAT - Get the task format parameter and set appropriate flags.

procedure ex_do_format (ex, format)

pointer	ex				#i task struct pointer
char	format[ARB]			#i format parameter value

bool	streq()

begin
        if (DEBUG) { call eprintf("format='%s'\n");call pargstr (format) }

	EX_COLOR(ex) = NO
	if (streq(format,"raw"))
	    EX_FORMAT(ex) = FMT_RAW
	else if (streq(format,"list"))
	    EX_FORMAT(ex) = FMT_LIST
	else {
	    EX_FORMAT(ex) = FMT_BUILTIN
	    call exb_do_format (ex, format)
	}
end


# EX_DO_HEADER - Process the header parameter.  

procedure ex_do_header (ex, header)

pointer	ex				#i task struct pointer
char	header[ARB]			#i header parameter string

bool	streq()
int	access()

begin
        if (DEBUG) { call eprintf("header='%s'\n") ; call pargstr (header) }

	if (streq(header,"no"))
	    EX_HEADER(ex) = HDR_NONE
	else if (streq(header,"yes"))
	    EX_HEADER(ex) = HDR_SHORT
	else if (streq(header,"long"))
	    EX_HEADER(ex) = HDR_LONG
	else {
	    EX_HEADER(ex) = HDR_USER
            if (access (header, 0, 0) == NO)
	        call error (2, "User-defined header file does not exist.")
	    else
	        call strcpy (header, HDRFILE(ex), SZ_FNAME)
	}
end


# EX_DO_OUTTYPE - Process the output pixel type parameter.

procedure ex_do_outtype (ex, outtype)

pointer	ex				#i task struct pointer
char	outtype[ARB]			#i outtype parameter string

int	pixtype, nbytes

int	ex_ptype(), stridx()

begin
        if (DEBUG) { call eprintf("outtype='%s'\n");call pargstr (outtype) }

	if (outtype[1] == EOS) {
	    EX_OUTTYPE(ex) = EX_IMTYPE(ex) 	# use type of input image
	    return
	}

	pixtype = stridx(outtype[1],"buirn")
	if (pixtype == 0)
	    call error (2, "Invalid 'outtype' value specified\n")

	if (outtype[2] == EOS) {
	    if (outtype[1] == 'b')		# set minimal sizes
		nbytes = 1
	    else if (outtype[1] == 'u')
		nbytes = 2
	    else 
		nbytes = 4
	} else
	    nbytes = outtype[2] - '1' + 1

	# Set struct param.
	EX_OUTTYPE(ex) = ex_ptype (pixtype, nbytes)
	call sprintf (Memc[EX_OTPTR(ex)], SZ_FNAME, "%c%d")
	    call pargc (Memc[EX_OTPTR(ex)])
	    call pargi (nbytes)
end


# EX_DO_BSWAP -- Read the byte-swap string an load the ip structure.

procedure ex_do_bswap (ex, bswap)

pointer ex                              #i task struct pointer
char    bswap[ARB]                      #i byte swap string

char    ch, flag[SZ_FNAME]
int     sp, i

int     strdic()

begin
        if (DEBUG) { call eprintf("swap='%s'\n");call pargstr (bswap) }

        sp = 1
        EX_BSWAP(ex) = NULL
        while (bswap[sp] != EOS) {
            i = 1
            for (ch=bswap[sp];  ch != EOS && ch != ',';  ch=bswap[sp]) {
                flag[i] = ch
                i = i + 1
                sp = sp + 1
            }
            flag[i] = EOS

            switch (strdic (flag, flag, SZ_FNAME, SWAP_STR)) {
            case 1, 2:
                EX_BSWAP(ex) = or (EX_BSWAP(ex), S_NONE)
            case 3:
                EX_BSWAP(ex) = or (EX_BSWAP(ex), S_ALL)
            case 4:
                EX_BSWAP(ex) = or (EX_BSWAP(ex), S_I2)
            case 5:
                EX_BSWAP(ex) = or (EX_BSWAP(ex), S_I4)
            default:
                break
            }
        }
end


# EX_DO_OUTBANDS - Parse the 'outbands' expressions.  The operand tags are
# caught and space allocated.

procedure ex_do_outbands (ex, outbands)

pointer	ex				#i task struct pointer
char	outbands[ARB]			#i outbands expression string

pointer	sp, exp, expr
int	fd, nchars, nexpr
int	j, ip, plevel

int	open(), fstatl(), strlen()
char	getc()

errchk	open

begin
        if (DEBUG) { call eprintf("outbands='%s'\n");call pargstr (outbands) }

	if (outbands[1] == EOS) {
	    EX_NIMAGES(ex) = EX_UNDEFINED 	# convert the whole image
	    EX_NEXPR(ex) = EX_UNDEFINED
	    return
	}

	call smark (sp)
	call salloc (exp, SZ_EXPSTR, TY_CHAR)
	call aclrc (Memc[exp], SZ_EXPSTR)

	# If the outbands parameter is an @-file read in the expression from
	# the file, otherwise just copy the param to the working buffer.
        if (outbands[1] == '@') {
            fd = open (outbands[2], READ_ONLY, TEXT_FILE)
            nchars = fstatl (fd, F_FILESIZE) + 1
            call calloc (expr, max(SZ_EXPSTR,nchars), TY_CHAR)
	    ip = 0
	    for (j=0; j<nchars && ip != EOF; j=j+1)
	        ip = getc (fd, Memc[expr+j])
	    Memc[expr+nchars-1] = EOS
            call close (fd)
        } else {
            nchars = strlen (outbands) + 1
            call calloc (expr, max(SZ_EXPSTR,nchars), TY_CHAR)
	    call strcpy (outbands, Memc[expr], nchars)
	}

	nexpr = 0			# initialize variables

	# Preprocess the expression string to strip out functions that aren't
	# really evaluated for each line in the image.  The processing is
	# done in-place and the returned string should contain only processing
	# functions.
	call ex_preprocess (ex, Memc[expr])
        if (DEBUG) { call eprintf("\texpr1='%s'\n");call pargstr(Memc[expr]) }

	ip = 0
	while (Memc[expr+ip] != EOS) {
	    # Parse each expression into an outbands struct buffer.
	    plevel = 0
	    for (j=0; j<SZ_LINE && Memc[expr+ip] != EOS; j=j+1) {
		Memc[exp+j] = Memc[expr+ip]
		if (Memc[expr+ip] == '(')
		    plevel = plevel + 1
		else if (Memc[expr+ip] == ')')
		    plevel = plevel - 1
		else if (Memc[expr+ip] == ',' && plevel == 0)
		    break
		else if (Memc[expr+ip] == EOS)
		    break

		ip = ip + 1
	    }
	    if (Memc[expr+ip] != EOS)
	        ip = ip + 1
	    Memc[exp+j] = '\0'
	    nexpr = nexpr + 1

	    if (DEBUG) {
		call eprintf ("\texpr[%d] = `%s'\n")
		    call pargi(nexpr);call pargstr(Memc[exp])
	    }

	    # Save expression in outbands struct.
	    call ex_alloc_outbands (OBANDS(ex,nexpr))
	    call strcpy (Memc[exp], O_EXPR(ex,nexpr), SZ_EXPSTR)
	}
	EX_NEXPR(ex) = nexpr

	# Now that we have the expressions break out the operands.
	call ex_parse_operands (ex)

	# Set the output type flag if not already defined in preprocessing.
	if (EX_OUTFLAGS(ex) == 0) {
	    if (EX_INTERLEAVE(ex) == 0 && EX_NEXPR(ex) > 1)
	        EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), PIXEL_STORAGE)
	    else if (EX_INTERLEAVE(ex) > 0 && EX_NEXPR(ex) > 1)
	        EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), LINE_STORAGE)
	    else
	        EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), BAND_STORAGE)
	}

	call mfree (expr, TY_CHAR)
	call sfree (sp)
end


# EX_PARSE_OPERANDS - Parse each expression string to break out the image
# operands.  If the input image list is 2-D data we'll be generous and
# allow either 'b1' or 'i1', otherwise require the bands number.

define	SZ_TAG	7

procedure ex_parse_operands (ex)

pointer	ex				#i task struct pointer

pointer	sp, expr
int	i, ip, opnum
char	ch, tag[SZ_TAG]

int	ctoi()

begin
	call smark (sp)
	call salloc (expr, SZ_EXPSTR, TY_CHAR)

	EX_NIMOPS(ex) = 0
	EX_NIMAGES(ex) = 0
	do i = 1, EX_NEXPR(ex) {
	    call aclrc (Memc[expr], SZ_EXPSTR)
	    call strcpy (O_EXPR(ex,i), Memc[expr], SZ_EXPSTR)

	    ip = 1
	    while (Memc[expr+ip] != EOS) {
		ch = Memc[expr+ip-1]

		# See if we have an operand descriptor.
		if ((ch == 'b' || ch == 'i') && IS_DIGIT(Memc[expr+ip])) {
		    ip = ip + 1
		    if (ctoi (Memc[expr], ip, opnum) == 0)
			call error (4, "can't parse operand")
		    call sprintf (tag, SZ_TAG, "%c%d")
			call pargc (ch)
		 	call pargi (opnum)

		    # Allocate the operand structure
		    if (IMOP(ex,opnum) == NULL) {
		        call ex_alloc_operand (IMOP(ex,opnum))
			call strcpy (tag, OP_TAG(IMOP(ex,opnum)), SZ_TAG)
			EX_NIMOPS(ex) = EX_NIMOPS(ex) + 1
		    }

		    # For 2-D images allow either name interchangeably.  Here
		    # we set the struct image band, we'll load the image de-
		    # scriptor later.
	            if (EX_IMDIM(ex) == 2) {
			IO_BAND(IMOP(ex,opnum)) = 1
			EX_NIMAGES(ex) = EX_NIMOPS(ex)
	            } else if (EX_IMDIM(ex) == 3) {
			if (ch == 'i')
			    call error (4, "Image operand illegal w/ 3-D lists")
			IO_BAND(IMOP(ex,opnum)) = opnum
			EX_NIMAGES(ex) = 1
	            }
		    if (DEBUG)   call zze_prop (IMOP(ex,opnum))
	        }
		ip = ip + 1
	    }
	}

	call sfree (sp)
end


# EX_PROCESS_IMAGE - Process the image pixels.

procedure ex_process_image (ex)

pointer	ex				#i task struct pointer

int	flags

begin
        flags = EX_OUTFLAGS(ex)

	# Create the (if any) requested colormap first.
	if (bitset (flags, OF_MKCMAP))
	    call ex_mkcmap (ex)

	# Process the images.
	if (EX_FORMAT(ex) == FMT_BUILTIN) {
            # Write the builtin format.
	    call exb_process_image (ex)

        } else {
            if (bitset (flags, OF_BAND) || bitset (flags, BAND_STORAGE))
                call ex_no_interleave (ex)
            else if (bitset (flags, OF_LINE) || bitset (flags, LINE_STORAGE))
                call ex_ln_interleave (ex)
            else if (bitset (flags, PIXEL_STORAGE))
                call ex_px_interleave (ex)
            else
		call error (0, "Unknown processing param.")
        }

	#if (EX_VERBOSE(ex) == YES) {
	    call eprintf ("    Status: Done.          \n")
	    call flush (STDERR)
	#}
end


# EX_PRINFO - Print verbose information about the conversion.

procedure ex_prinfo (ex, np)

pointer ex                              #i task struct pointer
pointer	np[ARB]				#i ptr to image names

pointer im
int	i, j, flags

begin
	# Print information about the input images.
        call eprintf ("    Input images:\n")
        do i = 1, EX_NIMAGES(ex) {
            im = IO_IMPTR(IMOP(ex,i))
            call eprintf ("\t%s:  %s %40t")
                call pargstr (OP_TAG(IMOP(ex,i)))
                call pargstr (Memc[np[i]])
            do j = 1, IM_NDIM(im) {
                call eprintf ("%d ")
                    call pargi (IM_LEN(im,j))
                if (j < IM_NDIM(im))
                    call eprintf ("x ")
            }
            call eprintf ("    `%s'\n")
                call pargstr (IM_TITLE(im))
        }

	# Print information about the output file.
	flags = EX_OUTFLAGS(ex)
        call eprintf ("    Output file:\n")
	call eprintf ("\tName: %30t%s\n")
	    call pargstr (BFNAME(ex))
	call eprintf ("\tFormat: %30t%s\n")
            switch (EX_FORMAT(ex)) {
            case FMT_RAW:    call pargstr ("Raw")
            case FMT_LIST:   call pargstr ("List")
            case FMT_BUILTIN:
                call exb_pname (ex)
            }

	if (EX_FORMAT(ex) == FMT_RAW) {
	    call eprintf ("\tHeader: %30t%s%s\n")
                switch(EX_HEADER(ex)) {
                case HDR_NONE:  call pargstr ("None")  ; call pargstr ("")
                case HDR_SHORT: call pargstr ("Short") ; call pargstr ("")
                case HDR_LONG:  call pargstr ("Long")  ; call pargstr ("")
                case HDR_USER:  call pargstr ("User: ")
				call pargstr (HDRFILE(ex))
                }
	}

	call eprintf ("\tByte Order: %30t%s\n")
	    if (EX_FORMAT(ex) == FMT_BUILTIN)
		call exb_pendian (ex)
	    else if (EX_BSWAP(ex) == 0 && (BYTE_SWAP2==NO || BYTE_SWAP4==NO))
		call pargstr ("Most Significant Byte First")
	    else
		call pargstr ("Least Significant Byte First")

	call eprintf ("\tResolution: %30t%d x %d\n")
	    call pargi (EX_OCOLS(ex))
	    call pargi (EX_OROWS(ex))

	call eprintf ("\tPixel Storage: %30t%s\n")
	    if (EX_FORMAT(ex) == FMT_BUILTIN)
		call exb_pstorage (ex)
	    else if (bitset(flags, OF_BAND) || bitset(flags,BAND_STORAGE))
		call pargstr ("Band Interleaved")
	    else if (bitset(flags, OF_LINE) || bitset(flags,LINE_STORAGE))
		call pargstr ("Line Interleaved")
	    else if (bitset(flags,PIXEL_STORAGE))
		call pargstr ("Pixel Interleaved")
	    else
		call pargstr ("Unknown")

	if (bitset(flags, OF_CMAP) || bitset(flags, OF_MKCMAP))
	    call eprintf ("\tType: %30t8-bit Color Indexed\n")
	else {
	    if (bitset(flags, OF_BAND) && EX_NEXPR(ex) > 1)
	        call eprintf ("\tType: %30tGrayscale\n")
	    else
	        call eprintf ("\tType: %30tRGB\n")
	}

	if (bitset(flags, OF_CMAP) || bitset(flags, OF_MKCMAP)) {
	    call eprintf ("\tColor Table: %30t%d entries\n")
	        call pargi (EX_NCOLORS(ex))
	} else
	    call eprintf ("\tColor Table: %30tnone\n")

	if (DEBUG && EX_NEXPR(ex) != 0) {
	    call eprintf ("\tEvaluated Expressions:\n")
	    do i = 1, EX_NEXPR(ex) {
	        call eprintf ("\t    %d)  %s\n")
		    call pargi (i)
		    call pargstr (O_EXPR(ex,i))
	    }
	}
end


# EX_PTYPE -- For a given outtype parameter return the corresponding IRAF
# data type.

define  NTYPES          6
define  NBITPIX         4

int procedure ex_ptype (type, nbytes)

int     type                            #i pixel type
int     nbytes                          #i number of bytes

int     i, pt, pb, ptype
int     tindex[NTYPES], bindex[NBITPIX], ttbl[NTYPES*NBITPIX]

data    tindex  /PT_BYTE, PT_UINT, PT_INT, PT_IEEE, PT_NATIVE, PT_SKIP/
data    bindex  /1, 2, 4, 8/

data    (ttbl(i), i= 1, 4)    /TY_UBYTE,  TY_USHORT,  TY_INT,      0/      # B
data    (ttbl(i), i= 5, 8)    /TY_UBYTE,  TY_USHORT,    0,         0/      # U
data    (ttbl(i), i= 9,12)    /TY_UBYTE,  TY_SHORT,   TY_INT,      0/      # I
data    (ttbl(i), i=13,16)    /   0,         0,       TY_REAL, TY_DOUBLE/  # R
data    (ttbl(i), i=17,20)    /   0,         0,       TY_REAL, TY_DOUBLE/  # N
data    (ttbl(i), i=21,24)    /TY_UBYTE,  TY_USHORT,  TY_REAL, TY_DOUBLE/  # X

begin
        if (type == 0 || nbytes == 0)           # uninitialized values
            return (0)

        pt = NTYPES
        do i = 1, NTYPES {
            if (tindex[i] == type)
                pt = i
        }
        pb = NBITPIX
        do i = 1, NBITPIX {
            if (bindex[i] == nbytes)
                pb = i
        }

        ptype = ttbl[(pt-1)*NBITPIX+pb]

	if (DEBUG) { call eprintf("pt=%d pb=%d -> ptype=%d\n")
	    call pargi (pt) ; call pargi (pb) ; call pargi (ptype) }

        if (ptype == 0)
            call error (0, "Invalid outtype specified.")
        else
            return (ptype)
end


# EX_MKFNAME - Create an output filename based on the requested format.

procedure ex_mkfname (ex, fname)

pointer ex                              #i task struct pointer
char	fname[ARB]			# generate the output filename

pointer	sp, suffix, test
int	fnextn()
bool 	streq()
pointer	exb_fmt_ext()

begin
	call smark (sp)
	call salloc (test, SZ_FNAME, TY_CHAR)

	if (EX_FORMAT(ex) == FMT_BUILTIN)
	     suffix =  exb_fmt_ext (ex)
	else if (EX_FORMAT(ex) == FMT_RAW || EX_FORMAT(ex) == FMT_LIST) {
	    call strcpy (fname, BFNAME(ex), SZ_FNAME)
	    call sfree (sp)
	    return
	}

	# If the current extension is not the same as the format extn add it.
	if (fnextn (fname, Memc[test], SZ_FNAME) > 0) {
	    if (streq(Memc[test], Memc[suffix+1])) {
		call strcpy (fname, BFNAME(ex), SZ_FNAME)
	        call sfree (sp)
		return
	    }
	}

	call sprintf (BFNAME(ex), SZ_FNAME, "%s%s")
	    call pargstr (fname)
	    call pargstr (Memc[suffix])

	call mfree (suffix, TY_CHAR)
	call sfree (sp)
end


# EX_ALLOC_OUTBANDS -- Allocate an outbands structure.

procedure ex_alloc_outbands (op)

pointer	op				#i outbands struct pointer

begin
	call calloc (op, LEN_OUTBANDS, TY_STRUCT)
	call calloc (OB_EXPSTR(op), SZ_EXPSTR, TY_CHAR)
end


# EX_FREE_OUTBANDS -- Free an outbands structure.

procedure ex_free_outbands (op)

pointer	op				#i outbands struct pointer

begin
	call mfree (OB_EXPSTR(op), TY_CHAR)
	call mfree (op, TY_STRUCT)
end


# EX_ALLOC_OPERAND -- Allocate an operand structure.

procedure ex_alloc_operand (op)

pointer	op				#i operand struct pointer

begin
	call calloc (op, LEN_OPERAND, TY_STRUCT)
	call calloc (IO_TAG(op), SZ_FNAME, TY_CHAR)
end


# EX_FREE_OPERAND -- Free an operand structure.

procedure ex_free_operand (op)

pointer	op				#i operand struct pointer

begin
	call mfree (IO_TAG(op), TY_CHAR)
	call mfree (op, TY_STRUCT)
end
