include <imio.h>
include <fset.h>
include <ctype.h>
include <imhdr.h>
include <ctotok.h>
include <mwset.h>

# Define some limits on the input file

define	MAX_FIELDS	100		# maximum number of fields in the list
define	TABSIZE		8		# spacing of the tab stops

# Define the supported units

define	WT_UNITSTR	"|hours|native|"
define	WT_UHOURS	1
define	WT_UNATIVE	2

define	WT_WCSSTR	"|logical|tv|physical|world|"
define	WT_LOGICAL	1
define	WT_TV		2
define	WT_PHYSICAL	3
define	WT_WORLD	4

# Define the supported wcs.
# T_WCSCTRAN -- Transform a list of image coordinates from one coordinate 
# system to another using world coordinate system information stored in
# the header of a reference image.

procedure t_wcsctran()

bool	verbose
int	i, csp, imlist,inlist, outlist, limlist, linlist, loutlist
int	icl, ocl, ndim, wcsndim, ncolumns, nunits, inwcs, outwcs, min_sigdigits
pointer	sp, image, columns, units, iwcs, owcs, fmtstr, fmtptrs
pointer	str, name, im, mw, ct, tmp

bool	clgetb()
int	imtopenp(), imtlen(), imtgetim(), fntopnb(), fntlenb(), fntgfnb()
int	open(), mw_stati(), wt_getlabels(), ctoi(), strdic(), clgeti(), nscan()
int	errget()
pointer	immap(), mw_openim(), mw_sctran()
errchk	mw_openim(), mw_gwattrs(), mw_sctran()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (columns, IM_MAXDIM, TY_INT)
	call salloc (units, IM_MAXDIM, TY_INT)
	call salloc (iwcs, SZ_FNAME, TY_CHAR)
	call salloc (owcs, SZ_FNAME, TY_CHAR)
	call salloc (fmtstr, SZ_FNAME, TY_CHAR)
	call salloc (fmtptrs, IM_MAXDIM, TY_POINTER)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (name, SZ_FNAME, TY_CHAR)

	# Get the input and output image and file lists.
	imlist = imtopenp ("image")
	limlist = imtlen (imlist)
	call clgstr ("input", Memc[str], SZ_FNAME)
	inlist = fntopnb (Memc[str], NO)
	linlist = fntlenb (inlist)
	call clgstr ("output", Memc[str], SZ_FNAME)
	if (Memc[str] == EOS)
	    call strcpy ("STDOUT", Memc[str], SZ_FNAME)
	outlist = fntopnb (Memc[str], NO)
	loutlist = fntlenb (outlist)

	# Get the input coordinate file format.
	call clgstr ("columns", Memc[str], SZ_FNAME)
	ncolumns = 0
	csp = 1
	while (wt_getlabels (Memc[str], csp, Memc[name], SZ_FNAME) != EOF) {
	    i = 1
	    if (ctoi(Memc[name], i, Memi[columns+ncolumns]) <= 0)
		break
	    ncolumns = ncolumns + 1
	}

	# Get the input coordinate units. Fill in any missing information
	# with native units
	call clgstr ("units", Memc[str], SZ_FNAME)
	nunits = 0
	csp = 1
	while (wt_getlabels (Memc[str], csp, Memc[name], SZ_FNAME) != EOF) {
	    i = strdic (Memc[name], Memc[name], SZ_FNAME, WT_UNITSTR)
	    if (i <= 0)
		break
	    Memi[units+nunits] = i
	    nunits = nunits + 1
	}
	do i = nunits + 1, IM_MAXDIM
	    Memi[units+i-1] = WT_UNATIVE

	# Get the input and output transform.
	call clgstr ("inwcs", Memc[iwcs], SZ_FNAME)
	inwcs = strdic (Memc[iwcs], Memc[iwcs], SZ_FNAME, WT_WCSSTR)
	call clgstr ("outwcs", Memc[owcs], SZ_FNAME)
	outwcs = strdic (Memc[owcs], Memc[owcs], SZ_FNAME, WT_WCSSTR)

	# Get the format strings and minimum number of significant digits.
	call clgstr ("formats", Memc[fmtstr], SZ_FNAME)
	min_sigdigits = clgeti ("min_sigdigits")

	# Get the remaining parameters.
	verbose = clgetb ("verbose")

	# Check that the image and output list lengths match. The number
	# of input coordinate lists must be 1 or equal to the number of
	# input images.
	if (limlist < 1 || (linlist > 1 && linlist != limlist)) {
	    call imtclose (imlist)
	    call fntclsb (inlist)
	    call fntclsb (outlist)
	    call error (0,
	        "Incompatable image and input coordinate list lengths.")
	}

	# Check that the image and output list lengths match. The number
	# of output coordinate lists must be 1 or equal to the number of
	# input images.
	if (loutlist > 1 && loutlist != limlist) {
	    call imtclose (imlist)
	    call fntclsb (inlist)
	    call fntclsb (outlist)
	    call error (0,
	        "Incompatable image and output coordinate list lengths.")
	}

	# Loop over the input images.
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the input image.
	    im = immap (Memc[image], READ_ONLY, 0)
	    ndim = IM_NDIM(im)

	    # Open the input coordinate file.
	    if (linlist <= 0)
	        icl = NULL
	    else if (fntgfnb (inlist, Memc[str], SZ_FNAME) != EOF)
	        icl = open (Memc[str], READ_ONLY, TEXT_FILE)
	    else 
		call seek (icl, BOF)

	    # Open the output coordinate file.
	    if (fntgfnb (outlist, Memc[str], SZ_FNAME) != EOF) {
	        ocl = open (Memc[str], NEW_FILE, TEXT_FILE)
		if (ocl == STDOUT)
		    call fseti (ocl, F_FLUSHNL, YES)
	    }

	    # Print optional banner string.
	    if (verbose) {
		call fprintf (ocl, "\n# Image: %s  Wcsin: %s Wcsout: %s\n")
		    call pargstr (Memc[image])
		    call pargstr (Memc[iwcs])
		    call pargstr (Memc[owcs])
	    }

	    # Set up the coordinate transform.
	    mw = NULL
	    iferr {

	        tmp = mw_openim (im); mw = tmp

		call mw_seti (mw, MW_USEAXMAP, NO)
		if (inwcs == WT_TV && outwcs == WT_TV)
		    ct = mw_sctran (mw, "logical", "logical", 0)
		else if (inwcs == WT_TV)
		    ct = mw_sctran (mw, "logical", Memc[owcs], 0)
		else if (outwcs == WT_TV)
		    ct = mw_sctran (mw, Memc[iwcs], "logical", 0)
		else
		    ct = mw_sctran (mw, Memc[iwcs], Memc[owcs], 0)
		wcsndim = mw_stati (mw, MW_NPHYSDIM)

		if (ndim == 0)
		    ndim = wcsndim

		call sscan (Memc[fmtstr])
		do i = 1, IM_MAXDIM {
	    	    call malloc (Memi[fmtptrs+i-1], SZ_FNAME, TY_CHAR)
		    call gargwrd (Memc[Memi[fmtptrs+i-1]], SZ_FNAME)
		    if (nscan() != i || Memc[Memi[fmtptrs+i-1]] == EOS) {
			if (outwcs == WT_WORLD) {
			    iferr (call mw_gwattrs (mw, i, "format",
			        Memc[Memi[fmtptrs+i-1]], SZ_FNAME))
			        Memc[Memi[fmtptrs+i-1]] = EOS
			} else
			    Memc[Memi[fmtptrs+i-1]] = EOS
		    }
		}

	    } then {
		if (verbose) {
		    i = errget (Memc[str], SZ_LINE)
		    call fprintf (ocl, "# \tWarning: %s\n")
			call pargstr (Memc[str])
		}
		if (mw != NULL)
		    call mw_close (mw)
		mw = NULL
		ct = NULL
	    }

	    # Check that the transform is valid.
	    if (ct == NULL) {

		# Skip the image if the transform is undefined.
		if (verbose) {
		    call fprintf (ocl,
		        "# \tSkipping: Unable to compile requested transform\n")
		}

	    # For input or output tv coordinates the image must be 2D
	    } else if (ndim != 2 && (inwcs == WT_TV || outwcs == WT_TV)) {

		# Skip the image if the transform is undefined.
		if (verbose) {
		    call fprintf (ocl,
		    "# \tSkipping: Image must be 2D for wcs type tv\n")
		}

	    # Check that the number of input columns is enough for images.
	    } else if ((ncolumns < ndim) || (ncolumns < wcsndim && inwcs !=
	        WT_LOGICAL && inwcs != WT_TV)) {

		if (verbose) {
		    call fprintf (ocl,
		        "# \tSkipping: Too few input coordinate columns\n")
		}

	    } else {

	        # Check the dimension of the wcs versus the dimension of the
	        # image and issue a warning if dimensional reduction has taken
	        # place.
	        if (wcsndim > ndim) {
		    if (verbose) {
		        call fprintf (ocl,
		        "# \tWarning: Image has been dimensionally reduced\n")
		    }
	        }
	        if (verbose) {
		    call fprintf (ocl, "\n")
	        }

		# Transform the coordinate file.
		call wt_transform (im, icl, ocl, Memi[columns], Memi[units],
		    ndim, inwcs, outwcs, mw, ct, Memi[fmtptrs], wcsndim,
		    min_sigdigits)

	    }

	    # Free the format pointers.
	    do i = 1, IM_MAXDIM
	        call mfree (Memi[fmtptrs+i-1], TY_CHAR)

	    # Close the input image.
	    if (mw != NULL)
		call mw_close (mw)
	    call imunmap (im)

	    # Close the input coordinate file if it is not going to be used.
	    if (linlist == limlist)
		call close (icl)

	    # Close the output coordinate file if it is not going to be
	    # appended to.
	    if (loutlist == limlist)
		call close (ocl)
	}

	# Close the input coordinate file
	if (linlist > 0 && linlist < limlist)
	    call close (icl)
	if (loutlist < limlist)
	    call close (ocl)

	call imtclose (imlist)
	call fntclsb (inlist)
	call fntclsb (outlist)

	call sfree (sp)
end


# WT_TRANSFORM -- Transform the input coordinates from the input coordinate
# system to the output coordinate system.

procedure wt_transform (im, icl, ocl, columns, units, ndim, inwcs, outwcs, mw,
	ct, fmtptrs, wcsndim, min_sigdigits)

pointer	im			#I the input image descriptor
int	icl			#I the input coordinate file descriptor
int	ocl			#I the output coordinate file descriptor
int	columns[ARB]		#I the input coordinate columns
int	units[ARB]		#I the input coordinate units
int	ndim			#I the number of input coordinates
int	inwcs			#I the input wcs type
int	outwcs			#I the output wcs type
pointer	mw			#I the wcs descriptor
pointer	ct			#I the pointer to the compiled transformation
pointer	fmtptrs[ARB]		#I the array of format pointers
int	wcsndim			#I the dimensions of the wcs
int	min_sigdigits		#I the minimum number of significant digits

int	nline, ip, nread, nwrite, max_fields, nfields, offset
pointer	sp, inbuf, linebuf, field_pos, outbuf, voff, vstep, paxno, laxno, incoo
pointer	lincoo, outcoo, nsig
int	getline(), li_get_numd()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (inbuf, SZ_LINE, TY_CHAR)
	call salloc (linebuf, SZ_LINE, TY_CHAR)
	call salloc (field_pos, MAX_FIELDS, TY_INT)
	call salloc (outbuf, SZ_LINE, TY_CHAR)

	call salloc (voff, wcsndim, TY_DOUBLE)
	call salloc (vstep, wcsndim, TY_DOUBLE)
	call salloc (paxno, wcsndim, TY_INT)
	call salloc (laxno, wcsndim, TY_INT)
	call salloc (incoo, wcsndim, TY_DOUBLE)
	call salloc (lincoo, wcsndim, TY_DOUBLE)
	call salloc (outcoo, wcsndim, TY_DOUBLE)
	call salloc (nsig, wcsndim, TY_INT)

	call mw_gaxmap (mw, Memi[paxno], Memi[laxno], wcsndim)
	call wt_laxmap (outwcs, Memi[paxno], wcsndim, Memi[laxno], ndim)
	call wt_vmap (im, Memd[voff], Memd[vstep], ndim)

	# Compute the number of coordinates to be read and written.
	if (inwcs == WT_LOGICAL && ndim < wcsndim)
	    nread = ndim
	else
	    nread = wcsndim
	if (outwcs == WT_LOGICAL && ndim < wcsndim)
	    nwrite = ndim
	else
	    nwrite = wcsndim
	call amovkd (INDEFD, Memd[outcoo], wcsndim)

	max_fields = MAX_FIELDS
	for (nline = 1; getline (icl, Memc[inbuf]) != EOF; nline = nline + 1) {

	    # Skip over leading white space.
	    for (ip = inbuf; IS_WHITE(Memc[ip]); ip = ip + 1)
		;

	    # Pass on comment and blank lines unchanged.
	    if (Memc[ip] == '#') {
                # Pass comment lines on to the output unchanged.
                call putline (ocl, Memc[inbuf])
                next
            } else if (Memc[ip] == '\n' || Memc[ip] == EOS) {
                # Blank lines too.
                call putline (ocl, Memc[inbuf])
                next
            }

	    # Expand tabs into blanks, determine field offsets.
            call strdetab (Memc[inbuf], Memc[linebuf], SZ_LINE, TABSIZE)
            call li_find_fields (Memc[linebuf], Memi[field_pos], max_fields,
                nfields)

	    # Decode the coordinates checking for valid input.
	    call aclri (Memi[nsig], wcsndim)
	    do ip = 1, nread {

		if (columns[ip] > nfields) {
		    call fstats (icl, F_FILENAME, Memc[outbuf], SZ_LINE)
		    call eprintf ("\tNot enough fields in file %s line %d\n")
			call pargstr (Memc[outbuf])
			call pargi (nline)
		    call putline (ocl, Memc[linebuf])
		    break
		}

		offset = Memi[field_pos+columns[ip]-1]
		if (li_get_numd (Memc[linebuf+offset-1],
		    Memd[incoo+ip-1], Memi[nsig+ip-1]) == 0) {
		    call fstats (icl, F_FILENAME, Memc[outbuf], SZ_LINE)
		    call eprintf ("\tBad value in file %s line %d column %d\n")
			call pargstr (Memc[outbuf])
			call pargi (nline)
			call pargi (ip)
		    call putline (ocl, Memc[linebuf])
		    break
		}

		if (IS_INDEFD(Memd[incoo+ip-1])) {
		    call fstats (icl, F_FILENAME, Memc[outbuf], SZ_LINE)
		    call eprintf ("\tBad value in file %s line %d column %d\n")
			call pargstr (Memc[outbuf])
			call pargi (nline)
			call pargi (ip)
		    call putline (ocl, Memc[linebuf])
		    break
		}

	    }

	    # Skip to next line if too few fields were read.
	    if (ip <= nread)
		next

	    # Adjust the input coordinate units if necessary.
	    switch (inwcs) {
	    case WT_TV:
		call wt_tvlogd (Memd[incoo], Memd[incoo], nread, Memd[voff],
		    Memd[vstep])
	    case WT_WORLD:
		call wt_cunits (Memd[incoo], units, nread)
	    default:
		;
	    }

	    # Compute the transform.
	    call wt_ctrand (ct, Memd[incoo], Memd[lincoo], Memi[paxno],
	        Memd[outcoo], wcsndim, nread)

	    # Adjust the output coordinate units if necessary.
	    switch (outwcs) {
	    case WT_TV:
		call wt_logtvd (Memd[outcoo], Memd[outcoo], wcsndim,
		    Memi[laxno], Memd[voff], Memd[vstep])
	    default:
		;
	    }

	    # Create the output file line.
	    call rg_apack_lined (Memc[linebuf], Memc[outbuf], SZ_LINE,
	        Memi[field_pos], nfields, columns, nread, Memd[outcoo],
	        Memi[laxno], fmtptrs, Memi[nsig], nwrite, min_sigdigits)
		    
	    # Write out the reformatted output line.
	    call putline (ocl, Memc[outbuf])

	}

	call sfree (sp)
end


# WT_LAXMAP (paxno, wcsndim, laxno, ndim)

procedure wt_laxmap (outwcs, paxno, wcsndim, laxno, ndim)

int	outwcs			#I the output wcs
int	paxno[ARB]		#I the physical axis map
int	wcsndim			#I the number of physical axis dimensions
int	laxno[ARB]		#O the physical axis map
int	ndim			#I the number of logical axis dimensions

int	i, j

begin
	if (outwcs == WT_LOGICAL && ndim < wcsndim) {
	    do i = 1, ndim {
	        laxno[i] = 0
	        do j = 1, wcsndim {
		    if (paxno[j] != i)
		        next
		    laxno[i] = j
		    break
	        }
	    }
	    do i = ndim + 1, wcsndim
		laxno[i] = 0
	} else {
	    do i = 1, wcsndim
		laxno[i] = i
	}
end


# WT_VMAP -- Fetch the image i/o section map. Tecnically this routine
# violates a system interface and uses the internal definitions in
# the imio.h file. However this routine is required to support tv coordinates
# which are coordinates with respect to the current section, and not identical
# to physcial coordinates.

procedure wt_vmap (im, voff, vstep, ndim)

pointer	im			#I the input image descriptor
double	voff[ARB]		#O the array of offsets
double	vstep[ARB]		#O the array of step sizes
int	ndim			#I the number of dimensions

int	i, dim

begin
	do i = 1, ndim {
	    dim = IM_VMAP(im,i)
	    voff[i] = IM_VOFF(im,dim)
	    vstep[i] = IM_VSTEP(im,dim)
	}
end


# WT_UNITS -- Correct the units of the input coordinates if necessary.

procedure wt_cunits (incoo, units, ncoo)

double	incoo[ARB]		#I the array of input coordinates
int	units[ARB]		#I the array of units
int	ncoo			#I the number of coordinates

int	i

begin
	do i = 1, ncoo {
	    switch (units[i]) {
	    case WT_UHOURS:
		incoo[i] = 15.0d0 * incoo[i]
	    default:
		;
	    }
	}
end


# WT_TVLOGD -- Linearly transform a vector of coordinates using an
# array of voffsets and scale factors.

procedure wt_tvlogd (incoo, outcoo, ndim, voff, vstep)

double	incoo[ARB]		#I array of input coordinates
double	outcoo[ARB]		#O array of output coordinates
int	ndim			#I number of coordinates
double	voff[ARB]		#I array of zero points
double	vstep[ARB]		#I array of scale factors

int	i

begin
	do i = 1, ndim
	    outcoo[i] = (incoo[i] - voff[i]) / vstep[i]
end


# WT_CTRAND -- Transform the coordinates.

procedure wt_ctrand (ct, incoo, lincoo, paxno, outcoo, wcsndim, nread)

pointer	ct			#I pointer to the compiled transform
double	incoo[ARB]		#I array of input coordinates
double	lincoo[ARB]		#U scratch array of input coordinates
int	paxno[ARB]		#I the physical axis map
double	outcoo[ARB]		#O array of output coordinates
int	wcsndim			#I the dimension of the wcs
int	nread			#I the number of input coordinates.

int	i

begin
	if (nread < wcsndim) {
	    do i = 1, wcsndim {
		if (paxno[i] == 0)
		    lincoo[i] = 1.0d0
		else
		    lincoo[i] = incoo[paxno[i]]
	    }
	    if (ct == NULL)
		call amovd (lincoo, outcoo, wcsndim)
	    else
		call mw_ctrand (ct, lincoo, outcoo, wcsndim)

	} else {
	    if (ct == NULL)
		call amovd (incoo, outcoo, wcsndim)
	    else
		call mw_ctrand (ct, incoo, outcoo, wcsndim)
	}

end


# WT_LOGTVD -- Linearly transform a vector of coordinates using an
# array of voffsets and scale factors.

procedure wt_logtvd (incoo, outcoo, wcsndim, laxno, voff, vstep)

double	incoo[ARB]		#I array of input coordinates
double	outcoo[ARB]		#O array of output coordinates
int	wcsndim			#I number of coordinates
int	laxno[ARB]		#I the logical axis map
double	voff[ARB]		#I array of zero points
double	vstep[ARB]		#I array of scale factors

int	i

begin
	do i = 1, wcsndim {
	    if (laxno[i] != 0)
	        outcoo[laxno[i]] = (incoo[laxno[i]] * vstep[laxno[i]]) +
		    voff[laxno[i]]
	}
end


# WT_GETLABELS -- Get the next label from a list of labels.

int procedure wt_getlabels (list, ip, label, maxch)

char    list[ARB]               #I list of labels
int     ip                      #U pointer in to the list of labels
char    label[ARB]              #O the output label
int     maxch                   #I maximum length of a column name

int     op, token
int     ctotok(), strlen()

begin
        # Decode the column labels.
        op = 1
        while (list[ip] != EOS) {

            token = ctotok (list, ip, label[op], maxch)
            if (label[op] == EOS)
                next
            if ((token == TOK_UNKNOWN) || (token == TOK_CHARCON))
                break
            if ((token == TOK_PUNCTUATION) && (label[op] == ',')) {
                if (op == 1)
                    next
                else
                    break
            }

            op = op + strlen (label[op])
            break
        }

        label[op] = EOS
        if ((list[ip] == EOS) && (op == 1))
            return (EOF)
        else
            return (op - 1)
end

