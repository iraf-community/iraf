include <error.h>
include <ctype.h>
include "export.h"
include "exfcn.h"

define	DEBUG	false


# EX_PREPROCESS - Some of the output functions aren't really applied to
# each line in the image (which is how the expressions are evaluated) but
# just define some feature of the whole output image.  We'll strip out
# those functions here and set a flag so that the expression evaluation
# code doesn't have to see them.

procedure ex_preprocess (ex, expr)

pointer	ex				#i task struct pointer
char	expr[ARB]			#i input expression strings

char	expstr[SZ_EXPSTR]
int	ip, pp, last_ip, explen
char	func[SZ_FNAME]
bool	saw_output_func

int	strlen(), strdic(), nowhite()

errchk	ex_pp_setcmap, ex_pp_psdpi
errchk	ex_cnt_parens, ex_pp_psscale

begin
	# Strip out any whitespace chars.
	call aclrc (expstr, SZ_EXPSTR)
	ip = nowhite (expr, expstr, SZ_EXPSTR)

	# Do a quick syntax check.
	iferr (call ex_cnt_parens (expstr))
	    call erract (EA_FATAL)

	# Only some functions may be nested, loop until we're forced to break.
	# The functions have a precedence such that "special functions" 
	# may have as arguments "output functions".  Below that are "scaling
	# functions" and "builtin functions" that are evaluated for each image
	# line.  Functions w/in the same class may/may not call each other
	# where it makes sense, we check for that here.
	#
	# The precedence order is:
	#
	#	 CMAP, SETCMAP, PSDPI, PSSCALE
	#	     BAND, LINE, FLIPX, FLIPY
	#	 	ZSCALE, GRAY, BSCALE, GAMMA
	#	 	    builtin functions

	if (DEBUG) { call eprintf("preproc: str=`%s'\n");call pargstr(expstr) }

	saw_output_func = false
	for (ip = 1 ; expstr[ip] == '(' ; ip = ip + 1)
	    ;

	last_ip = 1
	explen = strlen (expstr)
	repeat {
	    # Get the function name.
	    pp = 1
	    call aclrc (func, SZ_FNAME)
	    while (expstr[ip] != '(' && expstr[ip] != EOS) {
		func[pp] = expstr[ip]
		ip = ip + 1
		pp = pp + 1
	    }
	    func[pp+1] = EOS 
	    if (expstr[ip] == EOS) {
		call strcpy (expstr[last_ip], expr, SZ_EXPSTR)
		return
	    }
	    if (DEBUG) { call eprintf("\tfunc=`%s'\n");call pargstr(func) }

	    # Update pointer into string past '('.
	    ip = ip + 1

	    switch (strdic (func, func, SZ_FNAME, OB_FUNCTIONS)) {

	    case CMAP:
		if (EX_NEXPR(ex) > 1)
		    call error (4, 
			"cmap() func allowed only in single expression")
		if (saw_output_func)
		    call error (5, 
			"Function cmap() may not be nested in output func.")
		EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_MKCMAP)

	    case SETCMAP:
		if (EX_NEXPR(ex) > 1)
		    call error (4, 
			"setcmap() func allowed only in single expression")
		if (saw_output_func)
		    call error (5, 
			"Function setcmap(0 may not be nested in output func.")
		EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_CMAP)
		iferr (call ex_pp_setcmap (ex, expstr[ip]))
	    	    call erract (EA_FATAL)
		last_ip = ip
		explen = strlen (expstr)
		next

	    case PSDPI:
		if (EX_NEXPR(ex) > 1)
		    call error (4, 
			"psdpi() func allowed only in single expression")
		if (saw_output_func)
		    call error (5, 
			"Function psdpi() may not be nested in output func.")
		iferr (call ex_pp_psdpi (ex, expstr[ip]))
	    	    call erract (EA_FATAL)
		last_ip = ip
		explen = strlen (expstr)
		next

	    case PSSCALE:
		if (EX_NEXPR(ex) > 1)
		    call error (4, 
			"psscale() func allowed only in single expression")
		if (saw_output_func)
		    call error (5, 
			"Function psscale() may not be nested in output func.")
		iferr (call ex_pp_psscale (ex, expstr[ip]))
	    	    call erract (EA_FATAL)
		last_ip = ip
		explen = strlen (expstr)
		next


	    case BAND:
		saw_output_func = true
		EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_BAND)
	    case LINE:
		saw_output_func = true
		EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_LINE)
	    case FLIPX:
		saw_output_func = true
		EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_FLIPX)
	    case FLIPY:
		saw_output_func = true
		EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_FLIPY)

	    default:
		# No special function seen so just punt.  
		break
	    }

	    last_ip = ip			# update string ptr
	    if (expstr[explen] != ')')
		call error (5, 
		    "Malformed expression, expecting ')' as last char")
	    expstr[explen] = EOS  	# remove trailing right paren
	}

	# Copy expression from current ip to begining of buffer.
	call strcpy (expstr[last_ip], expr, SZ_EXPSTR)

        if (DEBUG) { call eprintf("\tfixed exp =`%s'\n");call pargstr(expr) }
end


# EX_PP_SETCMAP - Process the SETCMAP special function.

procedure ex_pp_setcmap (ex, expstr)

pointer	ex				#i task struct pointer
char	expstr[ARB]			#i expression string

pointer	sp, cm, cmap
int	ip, lp				# string pointers
int	tp, i				# where to trim the string

int	ctor()
bool	streq()
include "cmaps.inc"

begin
	call smark (sp)
	call salloc (cm, SZ_FNAME, TY_CHAR)
	call aclrc (Memc[cm], SZ_FNAME)

	if (DEBUG) { call eprintf("\t\texp=`%s'\n");call pargstr(expstr)}

	# Skip ahead to a quote char single or double) indicating colormap
	# name, we also stop at another non-blank char incase they didn't
	# use quotes.  If we find a comma, back up one so it's handled below.
	ip = 1
        while (expstr[ip] != EOS  &&
               expstr[ip] != '"'  &&
               expstr[ip] != '\'') {
                   if (expstr[ip] == '@')
                       for (ip=ip+2; expstr[ip] != '"'; ip=ip+1)
                           ;
                   ip = ip + 1
        }
	tp = ip - 1

	if (expstr[ip+1] == '"' || (expstr[ip+1]==' ' && expstr[ip+2]=='"') ||
	    expstr[ip+1] == '\'' || (expstr[ip+1]==' ' && expstr[ip+2]=='\'')) {
	       # No colormap file specified, assume it's a greyscale.
	       call strcpy ("greyscale", CMAPFILE(ex), SZ_FNAME)
	       ip = ip + 1

	} else {
	    # Get colormap name and put it in the task struct.
	    ip = ip + 1
	    lp = 0
	    repeat {
	        Memc[cm+lp] = expstr[ip]
	        lp = lp + 1
	        ip = ip + 1
	    } until (expstr[ip] == EOS || expstr[ip] == '"' || 
		expstr[ip] == '\'')
	    call strcpy (Memc[cm], CMAPFILE(ex), SZ_FNAME)
	}

	# Allocate the colormap pointer and read the colormap.
	iferr (call calloc (EX_CMAP(ex), 3*CMAP_SIZE, TY_CHAR))
	    call error (0, "Error allocating colormap pointer.")
	call ex_read_cmap (ex, CMAPFILE(ex))

	# Get optional brightness and contrast values.
	ip = ip + 1
	if (expstr[ip] == ',') {
	    ip = ip + 1
	    if (ctor (expstr, ip, EX_BRIGHTNESS(ex)) == 0)
	        call error (5, "cannot interpret brightness value")
	    ip = ip + 1
	    if (ctor (expstr, ip, EX_CONTRAST(ex)) == 0)
	        call error (5, "cannot interpret contrast value")

            # Don't scale the overlay colors in colormap.
            if (streq(CMAPFILE(ex), "overlay")) {
                cmap = EX_CMAP(ex)
                call ex_scale_cmap (cmap, 200,
                    EX_BRIGHTNESS(ex), EX_CONTRAST(ex))

                # Patch up the static overlay colors.
                do i = 201, 255 {
                    Memc[cmap+(EX_RED*CMAP_SIZE)+i]   = overlay[i*3+1]
                    Memc[cmap+(EX_GREEN*CMAP_SIZE)+i] = overlay[i*3+2]
                    Memc[cmap+(EX_BLUE*CMAP_SIZE)+i]  = overlay[i*3+3]
                }
            } else {
                call ex_scale_cmap (EX_CMAP(ex), EX_NCOLORS(ex),
                    EX_BRIGHTNESS(ex), EX_CONTRAST(ex))
            }
	}

	# We should be at the end of the string now.
	if (expstr[ip] != ')')
	    call error (5, "Malformed expression, expecting ')' as last char")

	if (DEBUG) { 
	    call eprintf("\t\tcmfile=`%s' brightness=%g contrast=%g\n")
	        call pargstr(CMAPFILE(ex));call pargr(EX_BRIGHTNESS(ex))
		call pargr(EX_CONTRAST(ex))
	}

	# Now trim the expression string.
	expstr[tp] = EOS 
	call sfree (sp)
end


# EX_PP_PSDPI - Process the PSDPI special function.

procedure ex_pp_psdpi (ex, expstr)

pointer	ex				#i task struct pointer
char	expstr[ARB]			#i expression string

int	ip, tp
int	ctor(), strlen()

begin
	if (DEBUG) { call eprintf("\t\texp=`%s'\n");call pargstr(expstr)}

	# The last argument is required to be the dpi resolution so pull
	# it out.
	ip = strlen (expstr)
	while (expstr[ip] != ',') {
	    ip = ip - 1
	    if (expstr[ip] == ')' || IS_ALPHA(expstr[ip]))
		call error (6, "syntax error")
	}

	tp = ip
	ip = ip + 1
	if (ctor(expstr,ip,EX_PSDPI(ex)) == 0)
	    call error (5, "cannot interpret EPS dpi value")

	# Now trim the expression string.
	expstr[tp] = EOS 
end


# EX_PP_PSSCALE - Process the PSSCALE special function.

procedure ex_pp_psscale (ex, expstr)

pointer	ex				#i task struct pointer
char	expstr[ARB]			#i expression string

int	ip, tp
int	ctor(), strlen()

begin
	if (DEBUG) { call eprintf("\t\texp=`%s'\n");call pargstr(expstr)}

	# The last argument is required to be the dpi resolution so pull
	# it out.
	ip = strlen (expstr)
	while (expstr[ip] != ',') {
	    ip = ip - 1
	    if (expstr[ip] == ')' || IS_ALPHA(expstr[ip]))
		call error (6, "syntax error")
	}

	tp = ip
	ip = ip + 1
	if (ctor(expstr,ip,EX_PSSCALE(ex)) == 0)
	    call error (5, "cannot interpret EPS scale value")

	# Now trim the expression string.
	expstr[tp] = EOS 
end


# EX_CNT_PARENS - Count the number of parentheses in the expression string.

procedure ex_cnt_parens (expr)

char	expr[ARB]				#i outbands expression strinf

int	ip, plev

begin
	ip = 1
	plev = 0
	while (expr[ip] != EOS) {
	    if (expr[ip] == '(')   plev = plev + 1
	    if (expr[ip] == ')')   plev = plev - 1
	    ip = ip + 1
	}
	if (plev > 0)
	    call error (5, "Missing right paren in `outbands' expression.")
	if (plev < 0)
	    call error (5, "Missing left paren in `outbands' expression.")
end
