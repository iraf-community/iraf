#
# VODATA -- Query and Access VO Data Services

include <ctype.h>
include	<error.h>
include <imhdr.h>
include <mwset.h>
include <tbset.h>


define 	DEBUG		FALSE
define 	DEF_BASE   	"res"

define  BP_NAMES    "|any|radio|millimeter|infrared|optical|uv|x-ray|gamma-ray|"
define	BP_ANY		1
define	BP_RADIO	2
define	BP_MILLIMETER	3
define	BP_INFRARED	4
define	BP_OPTICAL	5
define	BP_UV		6
define	BP_XRAY		7
define	BP_GAMMARAY	8

define  TYP_NAMES	"|any|catalog|image|spectral|"
define	TYP_ANY		1
define  TYP_CATALOG	2
define  TYP_IMAGE	3
define  TYP_SPECTRAL	4

define	OUT_FMTS	"|ascii|csv|tsv|html|raw|fits|"
define	FMT_ASCII	1
define	FMT_CSV		2
define	FMT_TSV		3
define	FMT_HTML	4
define	FMT_RAW		5
define	FMT_FITS	6


#    resources	list of Resources to query (or "" or ServiceURL)
#    objects	list of Object names/images to query (or VOTable of positions)
#    ra		RA of query position
#    dec	Dec of query position
#    sr		Search radius (degrees, or min or sec)
#
#    bandpass	bandpass constraint (radio|infrared|optical|uv|xray|gamma)
#    type	service type to query (catalog|image)
#    count	print only a count of the results
#    all	query all data sources
#    extract	extract positions and URLs
#
#    format	output format (|fits|ascii|csv|tsv|html|raw)
#    output	output filename (or 'samp' to broadcast table)
#    sequential	use sequential file numbers (or object name)
#
#    verbose	verbose output?
#


procedure t_vodata ()

char	resources[SZ_LINE], objects[SZ_FNAME]
char	format[SZ_FNAME], verb[SZ_FNAME], output[SZ_FNAME]
char	bandpass[SZ_FNAME], svctype[SZ_FNAME], dbgflag[SZ_FNAME]
char	fmt[SZ_FNAME], sr[SZ_FNAME], allflag[SZ_FNAME]
char	resdb[SZ_FNAME], resfile[SZ_FNAME], objfile[SZ_FNAME]
char	pos[SZ_FNAME], size[SZ_FNAME], ivorn[SZ_FNAME], listflag[SZ_FNAME]
char	sname[SZ_FNAME], url[SZ_FNAME], type[SZ_FNAME]

pointer	sp, name, tout, old, new, extn
pointer	cmd, bpass, stype
int	fd, verbose, ndat
int	objlist, reslist, nres, nobjs, n
bool	quiet, do_samp, count, all
double	ra, dec, srad

int	clgeti(), strdic(), open(), imaccess(), vod_rename()
int	access(), fntopnb(), fntgfnb(), fntlenb()
real	clgetr()
bool	clgetb(), streq(), rdb_lookup()

begin
	call smark (sp)
	call salloc (name, SZ_FNAME, TY_CHAR)
	call salloc (tout, SZ_PATHNAME, TY_CHAR)
	call salloc (old,  SZ_PATHNAME, TY_CHAR)
	call salloc (new,  SZ_PATHNAME, TY_CHAR)
	call salloc (cmd,  SZ_PATHNAME, TY_CHAR)
	call salloc (extn, SZ_PATHNAME, TY_CHAR)

	call salloc (bpass, SZ_FNAME, TY_CHAR)
	call salloc (stype, SZ_FNAME, TY_CHAR)


	# Get the task parameters.
	call clgstr ("resources", resources, SZ_LINE)	# query params
	all        = clgetb ("all")
	if (!all)
	    call clgstr ("objects", objects, SZ_LINE)
	call clgstr ("size", sr, SZ_FNAME)

	count      = clgetb ("count")
	verbose    = clgeti ("verbose")
	quiet      = clgetb ("quiet")
	do_samp    = clgetb ("samp")

	call clgstr ("output", output, SZ_FNAME)	# output params
	call clgstr ("format", format, SZ_FNAME)
	call clgstr ("bandpass", bandpass, SZ_FNAME)	# constraints
	call clgstr ("type", svctype, SZ_FNAME)
	call clgstr ("resdb", resdb, SZ_FNAME)


	################################
	# Build up arguments
	################################

	# Get the resources to query.
	call mktemp ("/tmp/res", resfile, SZ_FNAME)
	fd = open (resfile, NEW_FILE, TEXT_FILE)

	if (resources[1] == EOS || streq (resources, "any")) {
	    call fprintf (fd, "any\n")
	    nres = INDEFI
	    call strcpy ("+l", listflag, SZ_FNAME)
	} else {
	    reslist = fntopnb (resources, NO)
	    nres = fntlenb (reslist)
            while (fntgfnb (reslist, Memc[name], SZ_FNAME) != EOF) {
		if (rdb_lookup (resdb, Memc[name], type, sname, ivorn, url)) {
		    # Found an alias, print the unambiguous IVORN.
	            call fprintf (fd, "%s\n")
		        call pargstr (ivorn)
		} else {
		    # Not an alias, let the task resolve it.
	            call fprintf (fd, "%s\n")
		        call pargstr (Memc[name])
		}
	    }
	    call fntclsb (reslist)
	    call strcpy ("+n", listflag, SZ_FNAME)
	}
	call close (fd)


	# Get the object names to query.
	call mktemp ("/tmp/obj", objfile, SZ_FNAME)
	fd = open (objfile, NEW_FILE, TEXT_FILE)

	if (objects[1] != EOS) {
	    objlist = fntopnb (objects, NO)
	    nobjs = fntlenb (objlist)
            while (fntgfnb (objlist, Memc[name], SZ_FNAME) != EOF) {

		if (imaccess (Memc[name], READ_ONLY) == YES) {
		    call vod_img_query (Memc[name], ra, dec, srad, pos, size)

	    	    call fprintf (fd, "%g %g\n")
			call pargd (ra)
			call pargd (dec)
	    	    call sprintf (sr, SZ_FNAME, "%.6g")
			call pargd (srad)

		} else {
		    # Object name, let the task resolve it.
	            call fprintf (fd, "%s\n")
			call pargstr (Memc[name])
		}
            }
	    call fntclsb (objlist)

	} else {
	    # No names given, use the explicit position in the parameters.
	    nobjs = 1
	    call fprintf (fd, "%g %g\n")
		call pargr (clgetr ("ra"))
		call pargr (clgetr ("dec"))
	}
	call close (fd)


	# FIXME -- In this release we're limited to image services for blind
	#	   queries due to the catalog services querying non-std 
	#	   interfaces in VODATA>
	# until we figure out the threading error problem.
	if (IS_INDEFI(nres)) {
	    if (svctype[1] == EOS || !streq (svctype, "image")) {
	      call eprintf (
		"Error: Blind resource queries limited to 'image' services\n")
	      return
	    }
	}


	# Check constraints.
	call aclrc (Memc[bpass], SZ_FNAME)
	if (bandpass[1] == EOS || bandpass[1] == NULL) {
	    call strcpy ("any", Memc[bpass], SZ_FNAME)
	} else {
	    n = strdic (bandpass, Memc[bpass], SZ_FNAME, BP_NAMES)
	    if (!quiet && n == 0) {
	        call eprintf ("Invalid bandpass constraint '%s', ignoring.\n")
		    call pargstr (bandpass)
	    }
	}

	call aclrc (Memc[stype], SZ_FNAME)
	if (svctype[1] == EOS || svctype[1] == NULL) {
	    call strcpy ("any", Memc[stype], SZ_FNAME)
	} else {
	    n = strdic (svctype, Memc[stype], SZ_FNAME, TYP_NAMES)
	    if (!quiet && n == 0) {
	        call eprintf ("Invalid service constraint '%s', ignoring.\n")
		    call pargstr (svctype)
	    }
	}

	# Get output processing options.
	call vod_get_format (format, fmt, Memc[extn])		
	if (IS_INDEFI(nres) || do_samp || all) {
	    # Unknown multiple resources.
	    call strcpy (output, Memc[tout], SZ_FNAME)
	} else
	    call strcpy ("/tmp/vod", Memc[tout], SZ_FNAME)
	if (output[1] == EOS)
	    call strcpy ("vod", output, SZ_FNAME)	# set default name


	call strcpy ("+n", verb, SZ_FNAME)		# query verbosity
	if (verbose > 1)
	    call strcpy ("-v", verb, SZ_FNAME)
	if (verbose > 2)
	    call strcpy ("-vv", verb, SZ_FNAME)

	if (all)					# query all data svcs?
	    call strcpy ("-a", allflag, SZ_FNAME)
	else
	    call strcpy ("+n", allflag, SZ_FNAME)

	if (access ("/tmp/VOC_VDEBUG", 0, 0) == YES)	# debug flag
	    call strcpy ("+d", dbgflag, SZ_FNAME)
	else
	    call strcpy ("+n", dbgflag, SZ_FNAME)


	######################################
	# Call the VODATA task interface.
	######################################
	call vx_vodata (16,    				# argc
			"-t", Memc[stype],		# type constraint
	        	"-b", Memc[bpass], 		# bandpass constraint
	        	"-O", Memc[tout], 		# output filename (temp)
			verb,				# query verbosity
			allflag,			# query all data?
			dbgflag,			# debug flag
			listflag,			# svc_list flag
			"-q",				# suppress output
			"-N",				# numeric output name
			"-R",				# output format
			resfile,			# resources to query
			objfile, sr)			# objects to query


	if (!quiet && (nres > 1 || IS_INDEFI(nres)) && streq(output,"STDOUT")) {
	    call printf("# Rows  Cols  File         \tResource Title\n")
	    call printf("#\n")
	}

	# Clean up the output filenames.  For a single resource/object we
	# still end up with a name file 'foo_000_000.xml', use a more 
	# logical naming scheme.
	if (!all) {
	    ndat = vod_rename (output, Memc[tout], format, Memc[extn], 
	        nres, nobjs, quiet, count, do_samp)
	}

	if (!quiet && (nres > 1 || IS_INDEFI(nres)) && streq(output,"STDOUT")) {
	    call printf ("#\n# Data found for %d of %d resources queried.\n")
	        call pargi (ndat)
	        call pargi (nres)
	}

	# Clean up the temporary resource and object/position files.
	if (access (resfile, 0, 0) == YES)
	    call delete (resfile)
	if (access (objfile, 0, 0) == YES)
	    call delete (objfile)
end


#  VOD_RENAME -- Clean up the output filenames.  For a single resource/object
#  we still end up with a name file 'foo_000_000.xml', use a more logical 
#  naming scheme.

int procedure vod_rename (output, tout, format, extn, nres, nobjs, 
				quiet, count, samp)

char	output[ARB]				#i output base name
char	tout[ARB]				#i temp output name
char	format[ARB]				#i output format
char	extn[ARB]				#i filename extension
int	nres					#i number of resources
int	nobjs					#i number of objects
bool	quiet					#i print summary?
bool	count					#i print result count only?
bool	samp					#i broadcast as SAMP message?

pointer	sp, line, svc, obj, dir, new, old
int	i, j, ip, sfd, nch, nfound, nrows, ncols, stat

int	access(), stridx(), strldx(), getline(), open(), ctoi()
int	vot_convert()
bool	streq()

begin
	call smark (sp)
	call salloc (svc, SZ_FNAME, TY_CHAR)
	call salloc (obj, SZ_FNAME, TY_CHAR)
	call salloc (new, SZ_FNAME, TY_CHAR)
	call salloc (old, SZ_FNAME, TY_CHAR)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (dir, SZ_PATHNAME, TY_CHAR)

	nrows = 0
	ncols = 0
	nfound = 0

	# Get the cwd.
	call fpathname (".", Memc[dir], SZ_PATHNAME)
	ip = stridx ('/', Memc[dir])
	call strcpy (Memc[dir+ip-1], Memc[dir], SZ_PATHNAME)

	call sprintf (Memc[obj], SZ_FNAME, "%s.objects")
	    call pargstr (output)

	sfd = NULL
	if (IS_INDEFI(nres)) {
	    # Get the number of services queried.
	    call sprintf (Memc[svc], SZ_FNAME, "%s.services")
		call pargstr (output)
	    sfd = open (Memc[svc], READ_ONLY, TEXT_FILE)
	    nch = getline (sfd, Memc[line])

	    ip = 20
	    nch = ctoi (Memc[line], ip, nres)

	    nch = getline (sfd, Memc[line]) 		# Skip ahead
	    nch = getline (sfd, Memc[line])
	}

	# Process the output files as needed.

	for (i=0; i < nres; i=i+1) {
	    if (IS_INDEFI(nres))
	        nch = getline (sfd, Memc[line])

	    for (j=0; j < nobjs; j=j+1) {
		# Construct the vodata filename.
	    	call sprintf (Memc[old], SZ_FNAME, "%s_%03d_%03d.xml")
		    call pargstr (tout)
		    call pargi (i)
		    call pargi (j)

	    	if (streq ("STDOUT", output) || streq ("STDERR", output)) {
		    call strcpy (output, Memc[new], SZ_FNAME)
		    if (access (Memc[old], 0, 0) == YES) {
	    	        call fcopy (Memc[old], Memc[new])
	        	call delete (Memc[old])
			nfound = nfound + 1
		    }

	    	} else {

		    if (nres == 1 && nobjs == 1) {
	    	        call sprintf (Memc[new], SZ_FNAME, "%s%s%s")
		    	    call pargstr (Memc[dir])
			    call pargstr (output)
			    call pargstr (extn)
		    } else if (nres == 1) {
	    	        call sprintf (Memc[new], SZ_FNAME, "%s%s_%03d%s")
		    	    call pargstr (Memc[dir])
			    call pargstr (output)
			    call pargi (j)
			    call pargstr (extn)
		    } else if (nobjs == 1) {
	    	        call sprintf (Memc[new], SZ_FNAME, "%s%s_%03d%s")
		    	    call pargstr (Memc[dir])
			    call pargstr (output)
			    call pargi (i)
			    call pargstr (extn)
		    } else {
	    	        call sprintf (Memc[new], SZ_FNAME, "%s%s_%03d_%03d%s")
		    	    call pargstr (Memc[dir])
			    call pargstr (output)
			    call pargi (i)
			    call pargi (j)
			    call pargstr (extn)
		    }

		    if (access (Memc[old], 0, 0) == YES) {
			if (!quiet) {
			    call vod_tinfo (Memc[old], nrows, ncols)
			    ip = strldx ('/', Memc[new])
			    call eprintf (" %5d  %4d  %s")
			        call pargi (nrows)
			        call pargi (ncols)
			        call pargstr (Memc[new+ip])
			    if (nres > 1) {
			        call eprintf ("  %s")
			            call pargstr (Memc[new+22])
			    } else
			        call eprintf ("\n")
			}

			if (count) {
			    # If ony printing a count, delete saved results.
		            if (access (Memc[old], 0, 0) == YES)
	    	                call delete (Memc[old])
			} else {

		            if (access (Memc[new], 0, 0) == NO) {
			        if (!streq (extn, "xml")) {
            			    stat = vot_convert (Memc[old], Memc[new],
					format)
	    	                    call delete (Memc[old])
			        } else 
	    	                    call frename (Memc[old], Memc[new])
			    }

	    	    	    if (samp) 			# broadcast the table
			        call vod_bcast_table (Memc[new])
			}

			nfound = nfound + 1
		    }
	    	}
		    
		if (DEBUG) {
		    call eprintf ("%d/%d: %s -> %s\n")
			call pargi (nres) ; call pargi (nobjs)
			call pargstr (Memc[old])
			call pargstr (Memc[new])
		}
	    }
	}

			
	if (count || streq (output, "STDOUT")) {
	    # If ony printing a count, delete saved results.
	    if (access (Memc[svc], 0, 0) == YES)
		call delete (Memc[svc])
	    if (access (Memc[obj], 0, 0) == YES)
		call delete (Memc[obj])
	}
	if (sfd != NULL)
	    call close (sfd) 
	call sfree (sp)

	return (nfound)
end


# VOD_TINFO -- Get table dimensions.

procedure vod_tinfo (tblname, nrows, ncols)

char	tblname[ARB]				#i table name
int	nrows, ncols				#o table dimensions

pointer	tp, tbtopn()
int	tbpsta()

begin
        iferr {
            tp = tbtopn (tblname, READ_ONLY, 0)
        } then {
            call eprintf ("Error: can't open '%s'\n")
                call pargstr (tblname)
	    return
	}
 
        nrows   = tbpsta (tp, TBL_NROWS)
        ncols   = tbpsta (tp, TBL_NCOLS)

	call tbtclo (tp)
end


# VOD_BCAST_TABLE -- Broadcast a table name for loading.

procedure vod_bcast_table (tblname)

char	tblname[ARB]				#i table name

pointer	sp, cwd, cmd, name
int	status

begin
	call smark (sp)
	call salloc (cwd, SZ_PATHNAME, TY_CHAR)
	call salloc (cmd, SZ_PATHNAME, TY_CHAR)
	call salloc (name, SZ_PATHNAME, TY_CHAR)

	# Get the current working dir
	call zfgcwd (Memc[cwd], SZ_PATHNAME, status)
       	call strupk (Memc[cwd], Memc[cwd], SZ_PATHNAME)

	# Broadcast the table
	call sprintf (Memc[cmd], SZ_PATHNAME, "samp loadVOTAble file://%s\n")
	    call pargstr (tblname)

	#  FIXME -- Need to be using the real SAMP interface here.
	call clcmd (Memc[cmd])

	call sfree (sp)
end


# VOD_GET_FORMAT -- Convert the format parameter to the flag and file extn.

procedure vod_get_format (format, fmt, extn)

char	format[ARB]				#i format parameter
char	fmt[ARB]				#o format flag
char	extn[ARB]				#o output extension

int	ofmt
int	strdic()

begin
	ofmt = strdic (format, fmt, SZ_FNAME, OUT_FMTS)
	switch (ofmt) {
	case FMT_ASCII:   
	    call strcpy ("-A", fmt, SZ_FNAME)
	    call strcpy (".asv", extn, SZ_FNAME)
	case FMT_CSV: 	  
	    call strcpy ("-C", fmt, SZ_FNAME)
	    call strcpy (".csv", extn, SZ_FNAME)
	case FMT_TSV: 	  
	    call strcpy ("-T", fmt, SZ_FNAME)
	    call strcpy (".tsv", extn, SZ_FNAME)
	case FMT_HTML: 	  
	    call strcpy ("-H", fmt, SZ_FNAME)
	    call strcpy (".html", extn, SZ_FNAME)
	case FMT_RAW: 	  
	    call strcpy ("-R", fmt, SZ_FNAME)
	    call strcpy (".xml", extn, SZ_FNAME)
	case FMT_FITS: 	  
	    call strcpy ("-F", fmt, SZ_FNAME)
	    call strcpy (".fits", extn, SZ_FNAME)
	default: 	  
	    call strcpy ("-R", fmt, SZ_FNAME)
	    call strcpy (".xml", extn, SZ_FNAME)
	}
end


# VOD_TBL_COPY -- Copy a single table to a new file.  If the file exists, append
# as a new extension.

procedure vod_tbl_copy (oldfile, newfile)

char	oldfile[ARB]	# i: current file name
char	newfile[ARB]	# i: new file name

int	phu_copied	# set by tbfpri and ignored
pointer	sp, oldname, newname

bool	use_fcopy	# true if we should copy the file with fcopy

bool	streq()
int	tbtacc()
errchk	tbfpri, tbtcpy

begin
	call smark (sp)
	call salloc (oldname, SZ_FNAME, TY_CHAR)
	call salloc (newname, SZ_FNAME, TY_CHAR)

	# Check to make sure the copy is legal

	use_fcopy = false
	if (streq (oldfile, newfile)) {
	    call eprintf ("Cannot copy table to itself:  %s\n")
	    call pargstr (oldfile)

	    if (tbtacc (oldfile) == YES)
		use_fcopy = true

	    if (use_fcopy) {
		call tbtext (oldfile, Memc[oldname], SZ_FNAME)
		call tbtext (newfile, Memc[newname], SZ_FNAME)

		iferr (call fcopy (Memc[oldname], Memc[newname])) {
		    call erract (EA_WARN)
		}
	    }

	} else {
	    # Table extensions are copied by the table
	    # library function tbtcpy
	    iferr {
		call tbfpri (oldfile, newfile, phu_copied)
		call tbtcpy (oldfile, newfile)
	    } then {
		call erract (EA_WARN)
	    }
	}

	call sfree (sp)
	return
end


#  VOD_IMG_QUERY -- Get the query params for the named image.

procedure vod_img_query (imname, ra, dec, sr, pos, size)

char	imname[ARB]				#i image name
double	ra, dec, sr				#o RA/DEC/SR params
char	pos[ARB]				#o POS string
char	size[ARB]				#o SIZE string

double  r[IM_MAXDIM], w[IM_MAXDIM], cd[2,2]
double	xrot, yrot, rot, scale

pointer	im, mw, ctw, co
int	stat
double	cx, cy, ra0, dec0

pointer	immap()
int	sk_decim(), mw_stati()
pointer	mw_sctran()

begin
	#  Open the image.
  	iferr {
            im = immap (imname, READ_ONLY, 0)
	} then {
            # Unable to decode image WCS
	    call eprintf ("Error: Cannot open image '%s'\n")
		call pargstr (imname)
	    return
	}


	#  Get the WCS.
  	iferr {
            stat = sk_decim (im, "world", mw, co)
            if (stat == ERR || mw == NULL) {
            	# Unable to decode image WCS
		call eprintf ("Error: No image WCS present.\n")
		return

            } else if (mw != NULL) {
                ctw = mw_sctran (mw, "logical", "world", 03B)
		cx  = IM_LEN(im,1) / 2
		cy  = IM_LEN(im,2) / 2
        	call mw_c2trand (ctw, cx,  cy,  ra0, dec0)

		# Get the CD matrix for scale and rotation
           	call vod_gfterm (mw, r, w, cd, mw_stati (mw, MW_NPHYSDIM))
        	scale = 3600. * sqrt ((cd[1,1]**2 + cd[2,1]**2 +
				       cd[1,2]**2 + cd[2,2]**2) / 2.)

        	xrot  = abs (atan2 ( cd[2,1], cd[1,1]))
        	yrot  = abs (atan2 (-cd[1,2], cd[2,2]))
        	rot   = (xrot + yrot) / 2.0		# NOT USED


		ra = ra0
		dec = dec0
		sr =  max (((scale * IM_LEN(im,1)) / 3600.),
	      		    (scale * IM_LEN(im,2)) / 3600.)

		call sprintf (pos, SZ_FNAME, "%.6g,%.6g")
		    call pargd (ra0)
		    call pargd (dec0)
		call sprintf (size, SZ_FNAME, "%.4g,%.4g")
		    call pargd ((scale * IM_LEN(im,1)) / 3600.)
		    call pargd ((scale * IM_LEN(im,2)) / 3600.)
	    }

	} then {
            # Unable to decode image WCS
	    call eprintf ("Error: Unable to decode WCS.\n")
	}

	call imunmap (im)
end


# VOD_GFTERM -- Compute the output FITS CRPIX, CRVAL, and CD arrays from the
# MWCS LTERM and WTERM. Note that the CD matrix terms are still transposed
# from the usual Fortran order.

procedure vod_gfterm (mw, crpix, crval, cd, ndim)

pointer mw              			#i the input mwcs pointer
double  crpix[ndim]     			#o the output FITS CRPIX array
double  crval[ndim]     			#o the output FITS CRVAL array
double  cd[ndim,ndim]   			#o the output FITS CD matrix
int     ndim            			#i the dimensionality of the wcs

pointer sp, r, wcd, ltv, ltm, iltm
int	i

errchk	mw_gwtermd, mw_gltermd

begin
	call smark (sp)
	call salloc (r, ndim, TY_DOUBLE)
	call salloc (wcd, ndim * ndim, TY_DOUBLE)
	call salloc (ltv, ndim, TY_DOUBLE)
	call salloc (ltm, ndim * ndim, TY_DOUBLE)
	call salloc (iltm, ndim * ndim, TY_DOUBLE)

	iferr {
	    call mw_gwtermd (mw, Memd[r], crval, Memd[wcd], ndim)
	    call mw_gltermd (mw, Memd[ltm], Memd[ltv], ndim)
	    call mwvmuld (Memd[ltm], Memd[r], crpix, ndim)
	    call aaddd (crpix, Memd[ltv], crpix, ndim)
	    call mwinvertd (Memd[ltm], Memd[iltm], ndim)
	    call mwmmuld (Memd[wcd], Memd[iltm], cd, ndim)

	} then {
	    # Set up a default value.
	    call aclrd (cd, ndim*ndim)
	    for (i=1; i <= ndim; i=i+1) {
	        crpix[i] = 1.0d0
	        crval[i] = 1.0d0
	        cd[i,i] = 1.0d0
	    }
	}

	call sfree (sp)
end
