include <pkg/gtools.h>
include <error.h>
include "rvpackage.h"
include "rvflags.h"
include "rvcont.h"
include "rvsample.h"

# RV_OPEN -- Allocate structure and initialize

pointer procedure rv_open (spool, device, interactive)

char	spool[SZ_FNAME]				#I Output spool filename
char	device[SZ_FNAME]			#I Graphics output device name
bool	interactive				#I Interactive operation?

pointer	rv
pointer	sp, ip

real	clgetr ()
int	clgeti(), btoi(), strdic()
bool	streq()

errchk	init_ptrs, init_files
errchk	filt_open, plot_open, cont_open, keyw_open

begin
	call smark (sp)
	call salloc (ip, SZ_FNAME, TY_CHAR)

	# Allocate space for the structure
	iferr (call calloc (rv, LEN_RVSTRUCT, TY_STRUCT))
	    call error (0, "Error allocating RV structure pointers.")

	# Initlialize pointers
	call init_ptrs (rv)

	# Open the file descriptors
	call init_files (rv, device, spool, interactive)

	# Now allocate the sub-structures, although not all may be used.
	iferr {
	    call filt_open (rv)			# open 'filterpars' struct
	    call plot_open (rv)			# open 'plotpars' struct
	    call cont_open (rv)			# open 'continpars' struct
	    call keyw_open (rv)			# open 'keywpars' struct
	} then
	    call error (0, "Error opening sub-structures.")

	# Get any package parameters.
	RV_ZTHRESH(rv) = clgetr ("z_threshold")
	RV_TOLERANCE(rv) = clgetr ("tolerance")
	RV_MAXITERS(rv) = clgeti ("maxiters")
	RV_LINECOLOR(rv) = clgeti ("line_color")
	RV_TXTCOLOR(rv) = clgeti ("text_color")
        call clgstr ("interp", Memc[ip], SZ_LINE)
        if (streq(Memc[ip],"") || streq(Memc[ip]," "))
            call error (0,"Rv.interp specified as empty string.")
        RV_INTERP(rv) = strdic (Memc[ip], Memc[ip], SZ_LINE, IN_FUNCTIONS)

	RV_INTERACTIVE(rv) = btoi (interactive)

	call sfree (sp)
	return (rv)				# return the pointer
end


# RV_CLOSE -- Free the RV structure pointers.

procedure rv_close (rv)

pointer	rv					#I RV struct pointer

include	"rvsinc.com"

begin
	# MFREE call is ignored if pointers are NULL
	call mfree (RV_OPIXX(rv), TY_REAL)	# Free the data pointers
	call mfree (RV_OPIXY(rv), TY_REAL)
	call mfree (RV_RPIXX(rv), TY_REAL)
	call mfree (RV_RPIXY(rv), TY_REAL)
	call mfree (RV_WKPIXX(rv), TY_REAL)
	call mfree (RV_WKPIXY(rv), TY_REAL)
	call mfree (RV_OCONTP(rv), TY_REAL)
	call mfree (RV_RCONTP(rv), TY_REAL)
	call mfree (RV_COEFFS(rv), TY_REAL)
	call mfree (RV_ECOEFFS(rv), TY_REAL)
	call mfree (RV_ANTISYM(rv), TY_REAL)
	call mfree (RV_TEMPVEL(rv), TY_REAL)
	call mfree (RV_DBL_SHIFT(rv), TY_REAL)

	call mfree (RV_APNUMKWD(rv),TY_CHAR)
	call mfree (RV_APPARAM(rv), TY_CHAR)
	call mfree (RV_CCFFILE(rv), TY_CHAR)
	call mfree (RV_IMAGE(rv), TY_CHAR)
	call mfree (RV_RIMAGE(rv), TY_CHAR)
	call mfree (RV_SPOOL(rv), TY_CHAR)
	call mfree (RV_DEVICE(rv), TY_CHAR)
	call mfree (RV_OBJNAME(rv), TY_CHAR)
	call mfree (RV_TEMPNAME(rv), TY_CHAR)
	call mfree (DBG_FNAME(rv), TY_CHAR)

	call mfree (RV_APLIST(rv), TY_INT)
	call mfree (RV_TCODE(rv), TY_INT)

	call mfree (splx, TY_REAL)		# free up the sinc pointers
	call mfree (sply, TY_REAL)
	call mfree (sx, TY_REAL)
	call mfree (sy, TY_REAL)

	call samp_close (RV_OSAMPLE(rv))	# Free sample structure ptrs
	call samp_close (RV_RSAMPLE(rv))

	if (RV_GT(rv) != NULL)
	    call gt_free (RV_GT(rv))
	if (RV_GP(rv) != NULL)
	    call gclose (RV_GP(rv))
	if (RV_MGP(rv) != NULL)
	    call gclose (RV_MGP(rv))
	if (RV_GRFD(rv) != NULL) 
	    call close (RV_GRFD(rv))
	if (RV_TXFD(rv) != NULL) 
	    call close (RV_TXFD(rv))
	if (RV_ICFIT(rv) != NULL)
	    call ic_closer (RV_ICFIT(rv))

	if (RV_CONT(rv) != NULL)
	    call cont_close (rv)
	if (RV_FILTP(rv) != NULL)
	    call filt_close (rv)
	if (RV_KEYW(rv) != NULL)
	    call keyw_close (rv)
	if (RV_PLOTP(rv) != NULL)
	    call plot_close (rv)
	if (RV_OBSPTR(rv) != NULL)
	    call obsclose (RV_OBSPTR(rv))

	if (DBG_FD(rv) != NULL) 
	    call close (DBG_FD(rv))

	call mfree (rv, TY_STRUCT)		# Dump the struct
end


# INIT_PTRS - Inilialize all of the RV struct pointers to NULL

procedure init_ptrs (rv)

pointer	rv					#I RV struct pointer

begin
	RV_NPTS(rv) = 0
	RV_RNPTS(rv) = 0
	RV_CCFNPTS(rv) = 0
	RV_ISHIFT(rv) = 0
	RV_IMNUM(rv) = 1
	RV_TEMPNUM(rv) = 1
	RV_BACKGROUND(rv) = 0.0
	RV_FWHM_Y(rv) = INDEF
	RV_OFORMAT(rv) = NULL
	RV_RFORMAT(rv) = NULL
	RV_OW0(rv) = 0.0
	RV_OWPC(rv) = 0.0
	RV_RW0(rv) = 0.0
	RV_RWPC(rv) = 0.0
	RV_SPMKEY(rv) = 'n'
	RV_SPMPLOT(rv) = NORM_PLOT
	RV_MODES(rv) = 1
	RV_PRINTZ(rv) = -1
	RV_STATLINE(rv) = 0
	RV_TEMPCODE(rv) = 'A'
	RV_FITDONE(rv) = NO
	RV_RESDONE(rv) = NO
	RV_WINDOW(rv) = 20
	RV_WINCENTER(rv) = INDEFI
	RV_WINL(rv) = INDEFI
	RV_WINR(rv) = INDEFI
	RV_Y1(rv) = INDEF
	RV_Y2(rv) = INDEF

	# Initialize the data vectors
	RV_OPIXX(rv) = NULL
	RV_OPIXY(rv) = NULL
	RV_RPIXX(rv) = NULL
	RV_RPIXY(rv) = NULL
	RV_WKPIXX(rv) = NULL
	RV_WKPIXY(rv) = NULL
	RV_OCONTP(rv) = NULL
	RV_RCONTP(rv) = NULL
	RV_ANTISYM(rv) = NULL
	RV_TCODE(rv) = NULL
	RV_ERRCOMMENTS(rv) = NULL
	RV_CCFFILE(rv) = NULL

	# Pointers for other packages 
	RV_GP(rv) = NULL
	RV_MGP(rv) = NULL
	RV_GT(rv) = NULL
	RV_NLFIT(rv) = NULL
	RV_ICFIT(rv) = NULL
	RV_MWCSP(rv) = NULL

	# Debug pointers
	DBG_DEBUG(rv) = NO
	DBG_FNAME(rv) = NULL
	DBG_FD(rv) = STDOUT
	DBG_OTHER(rv) = 0
	DBG_LEVEL(rv) = 4

	# Miscellaneous
	RV_APLIST(rv) = NULL
	RV_OBJECTS(rv) = NULL
	RV_TEMPLATES(rv)= NULL
	RV_CMD(rv) = NULL
	RV_APNUMKWD(rv) = NULL
	RV_APPARAM(rv) = NULL
	RV_ERRCODE(rv) = OK

	# Allocate space for the data info pointers
	iferr {
	    call calloc (RV_TEMPVEL(rv), MAXTEMPS, TY_REAL)

	    call calloc (RV_COEFFS(rv), 6, TY_REAL)
	    call calloc (RV_ECOEFFS(rv), 6, TY_REAL)
	    call calloc (RV_DBL_SHIFT(rv), DBL_LEN, TY_REAL)

	    call calloc (RV_IMAGE(rv), SZ_FNAME, TY_CHAR)
	    call calloc (RV_RIMAGE(rv), SZ_FNAME, TY_CHAR)
	    call calloc (RV_SPOOL(rv), SZ_FNAME, TY_CHAR)
	    call calloc (RV_DEVICE(rv), SZ_FNAME, TY_CHAR)
	    call calloc (RV_OBJNAME(rv), SZ_FNAME, TY_CHAR)
	    call calloc (RV_TEMPNAME(rv), SZ_FNAME, TY_CHAR)
	} then
	    call error (0, "Error allocating structure pointers.")

	IS_DBLSTAR(rv) = NO
end


# INIT_FILES - Initialize the log and metacode output files

procedure init_files (rv, device, spool, interactive)

pointer	rv					#I RV struct pointer
char	device[SZ_FNAME]			#I Graphics output device
char	spool[SZ_FNAME]				#I Root spool name
bool	interactive				#I Interactive flag

pointer	sp, log, meta, verb, pverb
bool	streq()
int	verbose, cod_verbose()
pointer	open()
errchk	open

begin
	call smark (sp)
	call salloc (log, SZ_FNAME, TY_CHAR)
	call salloc (meta, SZ_FNAME, TY_CHAR)
	call salloc (verb, SZ_FNAME, TY_CHAR)
	call salloc (pverb, SZ_FNAME, TY_CHAR)

	# Open the graphics pointer and file descriptors
	if (!streq("", device))
	    call strcpy (device, DEVICE(rv), SZ_FNAME)
	call strcpy (spool, SPOOL(rv), SZ_FNAME)
	if (streq("", spool) || streq(" ", spool)) {
	    RV_TXFD(rv) = NULL
	    RV_GRFD(rv) = NULL
	    RV_VBFD(rv) = NULL
	    if (!interactive)
	    	call rv_errmsg ("Warning: No spool file specified.")
	} else if (streq("STDOUT", spool)) {
	    RV_TXFD(rv) = STDOUT
	    RV_GRFD(rv) = NULL
	    RV_VBFD(rv) = NULL
	} else {
	    # Open the files
	    iferr {
	        call cons_file_names (spool, Memc[log], Memc[meta], 
	    	    Memc[verb], SZ_FNAME)
	        RV_TXFD(rv) = open (Memc[log], APPEND, TEXT_FILE) 
		call clgstr ("verbose", Memc[pverb], SZ_FNAME)
	        verbose = cod_verbose (Memc[pverb])
		if (verbose != OF_SHORT && 
		    verbose != OF_NOLOG && 
		    verbose != OF_TXTONLY && 
		    verbose != OF_STXTONLY) {
	                RV_VBFD(rv) = open (Memc[verb], APPEND, TEXT_FILE) 
	        } else
	            RV_VBFD(rv) = NULL
		if (verbose != OF_NOGKI && 
		    verbose != OF_TXTONLY &&
		    verbose != OF_STXTONLY) {
	                RV_GRFD(rv) = open (Memc[meta], APPEND, BINARY_FILE) 
		} else
	            RV_GRFD(rv) = NULL
	    } then {
		call sfree (sp)
		call error (0, "Error opening spool file.")
	    }
	}

	call sfree (sp)
end


# INIT_GP - Initialize the graphics and gtools descriptors

procedure init_gp (rv, interactive, device)

pointer	rv					# RV struct pointer
bool	interactive				# Interactive operation flag
char	device[SZ_FNAME]			# Output device

pointer	gopen(), gt_init()
int	open(), tmp_fd
bool	streq()
errchk	gopen, gt_init, open

begin
	if (interactive) {
	    iferr {
		if (streq("stdgraph",device)) {
		    RV_GP(rv) = gopen ("stdgraph", NEW_FILE, STDGRAPH)
		} else {
		    tmp_fd = open (device, APPEND, BINARY_FILE)
		    RV_GP(rv) = gopen (device, APPEND, tmp_fd)
		}
	        RV_GT(rv) = gt_init ()
		call gt_sets (RV_GT(rv), GTTYPE, "line")
	    } then
		call error (0, "Error opening `stdgraph'.")
	    if (RV_GRFD(rv) != NULL) {
	        iferr (RV_MGP(rv) = gopen ("stdvdm", APPEND, RV_GRFD(rv)))
		    call error (0, "Error opening `stdvdm'.")
	    } else
		RV_MGP(rv) = NULL
	} else if (RV_GRFD(rv) != NULL) {
	    RV_GP(rv) = NULL
	    iferr (RV_MGP(rv) = gopen ("stdvdm", APPEND, RV_GRFD(rv)))
		call error (0, "Error opening `stdvdm'.")
	} else if (RV_GRFD(rv) == NULL && !interactive) {
	    RV_GP(rv) = NULL
	    RV_MGP(rv) = NULL
	}
end
