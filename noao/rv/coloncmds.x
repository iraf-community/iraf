include <ctype.h>
include <fset.h>
include <error.h>
include "rvpackage.h"
include "rvflags.h"
include "rvcont.h"
include "rvsample.h"

# COLON_CMDS  -  Utility file for common colon commands.  Usually, just 
# routines to get/set parameter values.  Task specific stuff is left in
# the appropriate colon command source file.


# CMD_ADD_COMMENT - Add a comment to the output logs.

procedure cmd_add_comment (rv)

pointer	rv					#I RV struct pointer

pointer	sp, buf
int 	i
char	c

begin
	if (RV_TXFD(rv) == NULL) {
	    call rv_errmsg ("No output log yet opened.")
	    return
	}

	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	# Now read the line and build up the buffer.
	call scanc (c)
	if (c == '\n' || c == EOS) {
	    call rv_errmsg ("Usage: ':comment <string>'")
	    call sfree (sp)
	    return
	} else {
	    i = 0
	    while (c != '\n' && c != EOS && i < SZ_LINE) {
		Memc[buf+i] = c
		i = i + 1
		call scanc (c)
	    }
	    Memc[buf+i] = '\0'
	    call fprintf (RV_TXFD(rv), "#   %s\n")
		call pargstr (Memc[buf])
	}

	call sfree (sp)
end


# CMD_APLIST - Set/Show the aperture list to process.

procedure cmd_aplist (rv, written)

pointer	rv					#I RV struct pointer
bool	written					#I Data write flag

pointer	sp, buf
int	stat, rv_apnum_range()
errchk	realloc

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call gargstr (Memc[buf], SZ_FNAME)
	if (Memc[buf] != EOS) {
	    call rv_do_save (rv, written)
	    stat = rv_apnum_range (rv, Memc[buf+1])
	} else {
	    call printf ("Aperture list = `%s'")
		call pargstr (APPARAM(rv))
	}

	call sfree (sp)
end


# CMD_APNUM - Get/Set the APNUM.

procedure cmd_apnum (rv, written)

pointer	rv					#I RV struct pointer
bool	written					#I Data write flag

int	i, ival, found
int	rv_getim(), nscan()

begin
	call gargi (ival)
	if (nscan() == 2) {
	    found = NO				# position the aplist pointer
	    do i = 1, NUMAPS(rv) {
		if (ival == APLIST(rv,i)) {
		    CURAPNUM(rv) = i
		    found = YES
		}
	    }
	    if (found == NO) {			# check if it's legal
		call rv_errmsg (
		   "Apnum not in current list. Reset list with `:apertures'")
		return
	    }
	    call rv_do_save (rv, written)
	    RV_APNUM(rv) = ival
	} else {
	    call printf ("APNUM = %d")
	        call pargi (RV_APNUM(rv))
	    return
	}

	# Get the new apertures.
	OBJCONT(rv) = NO
	IS_DBLSTAR(rv) = NO
	#SR_COUNT(RV_OSAMPLE(rv)) = ALL_SPECTRUM
	#SR_COUNT(RV_RSAMPLE(rv)) = ALL_SPECTRUM
	if (rv_getim(rv, IMAGE(rv), OBJECT_SPECTRUM, INDEF, INDEF, INDEFI) ==
	    ERR_READ)
	        return
	REFCONT(rv) = NO
	if (rv_getim(rv, RIMAGE(rv), REFER_SPECTRUM, INDEF, INDEF, INDEFI) == 
	    ERR_READ)
	        return

	RV_NEWXCOR(rv) = YES
end


# CMD_APODIZE - Get/Set the apodize percentage.

procedure cmd_apodize (rv)

pointer	rv					#I RV struct pointer

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    RV_APODIZE(rv) = rval
	    RV_NEWXCOR(rv) = YES
	} else {
	    call printf ("Apodize percentage = %g")
	        call pargr (RV_APODIZE(rv))
	}
end


# CMD_AUTODRAW - Set/Show the autodraw flag.

procedure cmd_autodraw (rv)

pointer	rv					#I RV struct pointer

bool	bval, itob()
int	nscan(), btoi()

begin
	call gargb (bval)
	if (nscan() == 2) {
	    RV_AUTODRAW(rv) = btoi (bval)
	} else {
	    call printf ("autodraw = `%b'")
	        call pargb (itob(RV_AUTODRAW(rv)))
	}
end


# CMD_AUTOWRITE - Set/Show the autowrite flag.

procedure cmd_autowrite (rv)

pointer	rv					#I RV struct pointer

bool	bval, itob()
int	nscan(), btoi()

begin
	call gargb (bval)
	if (nscan() == 2) {
	    RV_AUTOWRITE(rv) = btoi (bval)
	} else {
	    call printf ("autowrite = `%b'")
	        call pargb (itob(RV_AUTOWRITE(rv)))
	}
end


# CMD_BACKGROUND - Set/Show the fitting background.

procedure cmd_background (rv)

pointer	rv					#I RV struct pointer

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    call rv_erase_fit (rv, false)
	    RV_BACKGROUND(rv) = rval
	    RV_FITDONE(rv) = NO
	} else {
	    call printf ("Background = %g")
	        call pargr (RV_BACKGROUND(rv))
	}
end


# CMD_CONT - Do the continuum normalization.

procedure cmd_cont (rv)

pointer	rv					#I RV struct pointer

pointer	sp, cmd
int	stat, spc_cursor()

begin
	call smark (sp)
	call salloc (cmd, SZ_FNAME, TY_CHAR)

	call gargstr (Memc[cmd], SZ_FNAME)
	if (Memc[cmd] == EOS) {
	    # The default action (i.e. command is ":cont") is to do
	    # the continuum fitting from both the current bin spectrum
	    # and the current template spectrum.
	    call do_continuum (rv, OBJECT_SPECTRUM)
	    RV_GTYPE(rv) = NORM_PLOT
	    if (RV_CONTINUUM(rv) == TEMP_ONLY || RV_CONTINUUM(rv) == BOTH)
	        call do_continuum (rv, REFER_SPECTRUM)

	} else {
	    # Now parse the argument to find out what to do.
	    switch (Memc[cmd+1]) {
	    case 'o':				# do object only
	        call do_continuum (rv, OBJECT_SPECTRUM)
	    case 't':				# do template only
		call do_continuum (rv, REFER_SPECTRUM)
	    default:
		call rv_errmsg (
		  "Ambigous argument. Choose one of 'object|template'.")
	    }
	}
	if (RV_INTERACTIVE(rv) == YES)
	    stat = spc_cursor (rv)

	call sfree (sp)
end


# CMD_CONTINUUM - Set/Show which spectra get continuum subtracted.

procedure cmd_continuum (rv)

pointer	rv					#I RV struct pointer

pointer	sp, cont, bp
int	contin, cod_which()

begin
	call smark (sp)
	call salloc (bp, SZ_FNAME, TY_CHAR)
	call salloc (cont, SZ_FNAME, TY_CHAR)

	call gargstr (Memc[cont], SZ_FNAME)	# get a new file name
	if (Memc[cont] != EOS) {
	    contin = cod_which (Memc[cont+1])
	    if (contin == 0) {
		call rv_errmsg ("Unknown value.  Choose one of `%s'")
		    call pargstr ("|both|none|object|template|")
		call sfree (sp)
		return
	    }
	    RV_CONTINUUM(rv) = contin
	    if (RV_CONTINUUM(rv) == BOTH || RV_CONTINUUM(rv) == OBJ_ONLY)
	        call do_continuum (rv, OBJECT_SPECTRUM)
	    if (RV_CONTINUUM(rv) == BOTH || RV_CONTINUUM(rv) == TEMP_ONLY)
	        call do_continuum (rv, REFER_SPECTRUM)
	    RV_NEWXCOR(rv) = YES
	} else {
	    call nam_which (RV_CONTINUUM(rv), Memc[bp])
	    call printf ("continuum = `%s'")
		call pargstr (Memc[bp])
	}
	call sfree (sp)
end


# CMD_CORRECTION - Compute a velocity correction from a pixel shift.

procedure cmd_correction (rv)

pointer	rv					#I RV struct pointer

double	vobs, vcor, verr, rv_shift2vel()
real	rval, sigma
int	stat, nscan(), rv_rvcorrect()

begin
	call gargr (rval)
	sigma = 0.0
	if (nscan() == 2) {
	    if (RV_DCFLAG(rv) != -1) {
	        stat = rv_rvcorrect (rv, rval, sigma, vobs, vcor, verr)
	        call printf (
		    "Shift = %.4f ==>  vrel = %.4f vobs = %.4f vhelio = %.4f")
		        call pargr (rval)
			call pargd (rv_shift2vel(rv,rval))
		        call pargd (vobs)
		        call pargd (vcor)
	        call flush (STDOUT)
	    } else {
		call rv_errmsg ("No dispersion information for computation.")
	    }
	} else
	    call rv_errmsg ("Usage: ':correction <shift>'")
end


# CMD_DELTAV - Print the velocity dispersion to the screen.

procedure cmd_deltav (rv)

pointer	rv					#I RV struct pointer

begin
	call printf ("Velocity dispersion = %7.2f Km/sec/pixel")
	    call pargr (RV_DELTAV(rv))
end


# CMD_FILTER -  Set/Show the current FFT filter value.
 
procedure cmd_filter (rv)

pointer rv					#I RV struct pointer

pointer	sp, buf, bp
int	tmp, filt
int	rv_chk_filter(), cod_which()

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (bp, SZ_LINE, TY_CHAR)

	call gargstr (Memc[buf], SZ_FNAME)
	if (Memc[buf] != EOS) {
	    filt = cod_which (Memc[buf+1])
	    if (filt == 0) {
		call rv_errmsg ("Unknown value.  Choose one of `%s'")
		    call pargstr ("|both|none|object|template|")
		call sfree (sp)
		return
	    }
	    tmp = RV_FILTER(rv)
	    RV_FILTER(rv) = filt
	    if (filt == BOTH || filt == OBJ_ONLY) {
	        if (rv_chk_filter(rv,OBJECT_SPECTRUM) != OK) {
	    	        RV_FILTER(rv) = tmp
		        call rv_errmsg (
			    "Filter values not yet set or ambiguous.")
		        call sfree (sp)
		        return
	        }
	    } else if (filt == BOTH || filt == TEMP_ONLY) {
	        if (rv_chk_filter(rv,REFER_SPECTRUM) != OK) {
	    	        RV_FILTER(rv) = tmp
		        call rv_errmsg (
			    "Filter values not yet set or ambiguous.")
		        call sfree (sp)
		        return
	        }
	    }
	    RV_FILTER(rv) = filt
	    RV_NEWXCOR(rv) = YES
	} else {
	    call nam_which (RV_FILTER(rv), Memc[bp])
	    call printf ("filter = `%s'")
		call pargstr (Memc[bp])
	}

	call sfree (sp)
end


# CMD_FITFUNC -  Set/Show the current correlation fitting function.

procedure cmd_fitfunc (rv)

pointer	rv					#I RV struct pointer

pointer	sp, buf, bp
int	func, strdic()

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (bp, SZ_LINE, TY_CHAR)

	call gargstr (Memc[buf], SZ_FNAME)
	if (Memc[buf] != EOS) {
	    func = strdic(Memc[buf+1],Memc[buf+1], SZ_FNAME, RV_CFTYPES)
	    if (func == 0) {
		call rv_errmsg ("Unknown function.  Choose one of `%s'")
		    call pargstr ("|gaussian|lorentzian|parabola|center1d|")
		call sfree (sp)
		return
	    }
	    call rv_erase_fit (rv, false)
	    RV_FITFUNC(rv) = func
	    RV_FITDONE(rv) = NO
	    call rv_erase_fit (rv, true)
	} else {
	    call nam_fitfunc (rv, Memc[bp])
	    call printf ("Fitting Function = `%s'")
	       call pargstr (Memc[bp])
	}
	call sfree (sp)
end


# CMD_HEIGHT - Get/Set the fitting height.

procedure cmd_height (rv)

pointer	rv					#I RV struct pointer

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    call rv_erase_fit (rv, false)
	    RV_FITHGHT(rv) = rval
	    RV_FITDONE(rv) = NO
	} else {
	    call printf ("Height = %g")
	        call pargr (RV_FITHGHT(rv))
	}
end


# CMD_IMUPDATE - Set/Show the image header update flag.

procedure cmd_imupdate (rv)

pointer	rv					#I RV struct pointer

bool	bval, itob()
int	nscan(), btoi()

begin
	call gargb (bval)
	if (nscan() == 2) {
	    RV_IMUPDATE(rv) = btoi (bval)
	} else {
	    call printf ("imupdate = `%b'")
	        call pargb (itob(RV_IMUPDATE(rv)))
	}
end


# CMD_LINECOLOR - Set/Show the overlay vector line color.

procedure cmd_linecolor (rv)

pointer	rv					#I RV struct pointer

int	ival, nscan()

begin
	call gargi (ival)
	if (nscan() == 2) {
	    RV_LINECOLOR(rv) = ival
	    RV_NEWGRAPH(rv) = YES
	} else {
	    call printf ("Line color = %d")
	        call pargi (RV_LINECOLOR(rv))
	}
end


# CMD_MAXWIDTH - Get/Set the maximum fitting width.

procedure cmd_maxwidth (rv)

pointer	rv					#I RV struct pointer

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    if (rval > RV_CCFNPTS(rv)) {
		call rv_errmsg ("Maxwidth must be less than %d.")
		    call pargi (RV_CCFNPTS(rv))
	    } else {
		call rv_erase_fit (rv, false)
	        RV_MAXWIDTH(rv) = rval
		RV_FITDONE(rv) = NO
	    }
	} else {
	    call printf ("maxwidth = %g")
	        call pargr (RV_MAXWIDTH(rv))
	}
end


# CMD_MINWIDTH - Get/Set the minimum fitting width.

procedure cmd_minwidth (rv)

pointer	rv					#I RV struct pointer

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    if (rval < 3.) {
		call rv_errmsg ("Minwidth must be greater than 3.")
	    } else {
		call rv_erase_fit (rv, false)
	        RV_MINWIDTH(rv) = rval
		RV_FITDONE(rv) = NO
	    }
	} else {
	    call printf ("minwidth = %g")
	        call pargr (RV_MINWIDTH(rv))
	}
end


# CMD_NEXT - Get the next input spectrum.

int procedure cmd_next (rv, infile, rinfile, written, cmdstr)

pointer	rv					#I RV struct pointer
pointer	infile,	rinfile				#I File list pointers
bool	written					#I Have data been written?
char	cmdstr[SZ_FNAME]			#I Command string

pointer	sp, cmd
int	code
int	next_ap(), next_spec(), next_temp()

define	exit_			99

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	code = OK

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)

	call gargstr (Memc[cmd], SZ_FNAME)
	switch (Memc[cmd+1]) {
	case 'a':				# next aperture
	    call rv_do_save (rv, written)
	    code = next_ap (rv, written)
	case 'o':				# next object spectrum
	    call rv_do_save (rv, written)
	    code = next_spec (rv, infile, written)
	case 't':				# next template spectrum
	    call rv_do_save (rv, written)
	    code = next_temp (rv, rinfile, written)
	default:
	    call rv_errmsg ("Please specify 'aperture|object|template'.")
	}

exit_	call sfree (sp)
	return (code)
end


# CMD_OBJECTS - Set/Show the [new] object input list.

int procedure cmd_objects (rv, infile, written)

pointer	rv					#I RV struct pointer
pointer	infile					#I input list pointer
bool	written					#I data write flag

pointer	sp, buf
char 	imname[SZ_FNAME]
int	ip
pointer	imtopen()
int	get_spec(), imtrgetim(), rv_verify_aps()
errchk	imtopen

begin
	call smark (sp)
	call salloc (buf, SZ_FNAME, TY_CHAR)

	call gargstr (Memc[buf], SZ_FNAME)
	if (Memc[buf] != EOS) {
	    call rv_do_save (rv, written)
	    for (ip=1; IS_WHITE(Memc[buf+ip-1]); ip=ip+1)
	          ;
	    call imtclose (infile)
	    OBJCONT(rv) = NO			# update data flags
	    infile = imtopen (Memc[buf+ip-1])
	    RV_OBJECTS(rv) = infile
	    RV_IMNUM(rv) = 1
	    if (imtrgetim(infile, RV_IMNUM(rv), imname, SZ_FNAME) != EOF && 
	       infile != EOF) {
	        if (get_spec(rv, imname, OBJECT_SPECTRUM) == ERR_READ) {
		    call sfree (sp)
	    	    return (ERR_READ)
		}
		if (rv_verify_aps (rv, APPARAM(rv), APLIST(rv,1), 
		    NUMAPS(rv)) == ERR_READ)
		 	return (ERR_READ)
	    } else {
	        call rv_errmsg ("Error reading image from list.\n")
	        call sfree (sp)
	        return (ERR_READ)
	    }
	    RV_NEWXCOR(rv) = YES
	    RV_FITDONE(rv) = NO
	    written = false
	} else {
	    call printf ("Current object image name = `%s'")
	         call pargstr (IMAGE(rv))
	}

	call sfree (sp)
	return (OK)
end


# CMD_OUTPUT - Change/Show the output log file name. 

procedure cmd_output (rv)

pointer	rv					#I RV struct pointer

pointer	sp, fn
bool	streq()
pointer	gopen()
errchk  gopen

begin
	call smark (sp)
	call salloc (fn, SZ_FNAME, TY_CHAR)

	call gargstr (Memc[fn], SZ_FNAME)	# get new file name
	if (Memc[fn] == EOS) {
	    call printf ("Output file name = `%s'")
		call pargstr (SPOOL(rv))
	} else {
	    # Close existing log file - if any
	    if (RV_TXFD(rv) != NULL)
	        call close (RV_TXFD(rv))
	    if (RV_GRFD(rv) != NULL)
	        call close (RV_GRFD(rv))

	    # Open the graphics pointer and file descriptors.
	    if (streq("",Memc[fn+1]) || streq(" ",Memc[fn+1]) || 
		Memc[fn+1] == '"') {
	            RV_TXFD(rv) = NULL
	            RV_GRFD(rv) = NULL
	    } else if (streq("STDOUT",Memc[fn+1])) {
	        RV_TXFD(rv) = STDOUT
	        RV_GRFD(rv) = NULL
	    } else {
	        # Open the files
		if (!streq(Memc[fn+1],"\"\"")) {
		    call init_files (rv, DEVICE(rv), Memc[fn+1], true)
        	    RV_MGP(rv) = gopen ("stdvdm", APPEND, RV_GRFD(rv))
		}
	    }

	    if (streq("",Memc[fn+1]) || streq(" ",Memc[fn+1]) || 
		Memc[fn+1] == '"') {
	        call strcpy ("", SPOOL(rv), SZ_FNAME)
	    } else
	        call strcpy (Memc[fn+1], SPOOL(rv), SZ_FNAME)
	}

	call sfree (sp)
end


# CMD_OUT_TYPE - CCF output type.

procedure cmd_out_type (rv)

pointer	rv					#I RV struct pointer

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call gargstr (Memc[buf], SZ_FNAME)
	if (Memc[buf] != EOS) {
	    if (Memc[buf+1] == 'i') {
	        RV_CCFTYPE(rv) = OUTPUT_IMAGE
	    } else if (Memc[buf+1] == 't') {
	        RV_CCFTYPE(rv) = OUTPUT_TEXT
	    } else
		call rv_errmsg ("Choose one of 'image|text'.")
	} else {
	    call printf ("ccftype = `%s'")
	        if (RV_CCFTYPE(rv) == OUTPUT_IMAGE)
	            call pargstr ("image")
	        else
	            call pargstr ("text file")
	}

	call sfree (sp)
end


# CMD_PEAK - Is peak height a normalized correlation?

procedure cmd_peak (rv)

pointer	rv					#I RV struct pointer

bool	bval, itob()
int	nscan(), btoi()

begin
	call gargb (bval)
	if (nscan() == 2) {
	    call rv_erase_fit (rv, false)
	    RV_PEAK(rv) = btoi (bval)
	    RV_FITDONE(rv) = NO
	} else {
	    call printf ("peak = `%b'")
	        call pargb (itob(RV_PEAK(rv)))
	}
end


# CMD_PIXCORR - Do a pixel-only correlation?

procedure cmd_pixcorr (rv)

pointer	rv					#I RV struct pointer

bool	bval, itob()
int	stat, nscan(), btoi(), rv_getim()

begin
	call gargb (bval)
	if (nscan() == 2) {
	    if (btoi(bval) != RV_PIXCORR(rv)) {
	        RV_PIXCORR(rv) = btoi (bval)
	        RV_FITDONE(rv) = NO
	        RV_NEWXCOR(rv) = YES
		call printf ("Re-reading images....")
		call flush (STDOUT)
                stat = rv_getim (rv, IMAGE(rv), OBJECT_SPECTRUM, INDEF,
		    INDEF, INDEFI)
                stat = rv_getim (rv, RIMAGE(rv), REFER_SPECTRUM, INDEF,
		    INDEF, INDEFI)
		call printf ("\n")
		call flush (STDOUT)
            }
	} else {
	    call printf ("pixcorr = `%b'")
	        call pargb (itob(RV_PIXCORR(rv)))
	}
end


# CMD_PREVIOUS - Get the previous input spectrum.

int procedure cmd_previous (rv, infile, rinfile, written, cmdstr)

pointer	rv					#I RV struct pointer
pointer	infile,	rinfile				#I file list pointers
bool	written					#I have data been written?
char	cmdstr[SZ_FNAME]			#I command string

pointer	sp, cmd
int	code
int	prev_ap(), prev_spec(), prev_temp()

define	exit_			99

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	code = OK

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)

	call gargstr (Memc[cmd], SZ_FNAME)
	switch (Memc[cmd+1]) {
	case 'a':				# previous aperture
	    call rv_do_save (rv, written)
	    code = prev_ap (rv, written)
	case 'o':				# previous object spectrum
	    call rv_do_save (rv, written)
	    code = prev_spec (rv, infile, written)
	case 't':				# previous template spectrum
	    call rv_do_save (rv, written)
	    code = prev_temp (rv, rinfile, written)
	default:
	    call rv_errmsg ("Please specify 'aperture|object|template'.")
	}

exit_	call sfree (sp)
	return (code)
end


# CMD_PRINTZ - Toggle output of Z values

procedure cmd_printz (rv)

pointer	rv					#I RV struct pointer

bool	bval, itob()
int	nscan(), btoi()

begin
	call gargb (bval)
	if (nscan() == 2) {
	    RV_PRINTZ(rv) = btoi (bval)
	} else {
	    call printf ("Z output = `%b'")
	        call pargb (itob(RV_PRINTZ(rv)))
	}
end


# CMD_PRTDISP - Print the rebinned dispersion info for the user.

procedure cmd_prtdisp (rv)

pointer	rv						#I RV struct pointer

int	nscan()

begin
	if (nscan() > 1) {
	    call rv_errmsg ("Syntax: ':disp'.")
	} else {
	    if (RV_DCFLAG(rv) != -1) {
		call printf ("Object W0 = %.5f  Template W0 = %.5f  WPC = %g\n")
		    call pargr (RV_OW0(rv))
		    call pargr (RV_RW0(rv))
	            call pargr (RV_OWPC(rv))
	    } else {
	        call printf (
		  "No dispersion information present. (Pixel correlation only)")
	    }
	}
end


# CMD_REBIN - Set/Show the rebin parameter.

procedure cmd_rebin (rv)

pointer	rv					#I RV struct pointer

pointer	sp, rb
int	rebin, cod_rebin()

begin
	call smark (sp)
	call salloc (rb, SZ_FNAME, TY_CHAR)

	call gargstr (Memc[rb], SZ_FNAME)
	if (Memc[rb] != EOS) {
            rebin = cod_rebin (Memc[rb])
            if (rebin == ERR) {
                call rv_errmsg (
		   "`rebin' must be one of `smallest|largest|object|template'")
		call sfree (sp)
		return
	    }
	    RV_REBIN(rv) = rebin
	} else {
	    call nam_verbose (rv, Memc[rb])
	    call printf ("rebin = `%s'")
	        call pargstr (Memc[rb])
	}
end


# CMD_REFSPEC - Set/Show the [new] reference spectrum.

int procedure cmd_refspec (rv, rinfile, written)

pointer	rv					#I RV struct pointer
pointer	rinfile					#U image list pointer
bool	written					#I data write flag

pointer	sp, buf, tmp
pointer	imtopen()
int	ip
int	read_template_list(), rv_verify_aps()
errchk	imtopen

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call gargstr (Memc[buf], SZ_FNAME)
	if (Memc[buf] != EOS) {
	    call rv_do_save (rv, written)
	    for (ip=0; IS_WHITE(Memc[buf+ip]); ip=ip+1)	# skip white space
	          ;
	    tmp = imtopen (Memc[buf+ip])
	    if (read_template_list(rv,tmp) == ERR_READ) {
		call rv_errmsg ("Null list specified for templates.")
		call sfree (sp)
		return (ERR_READ)
	    } else {
	        call imtclose (rinfile)
	        rinfile = tmp
	        RV_TEMPLATES(rv) = tmp
		if (rv_verify_aps (rv, APPARAM(rv), APLIST(rv,1), 
		    NUMAPS(rv)) == ERR_READ)
		 	return (ERR_READ)
	    }
	    RV_NEWXCOR(rv) = YES
	    RV_FITDONE(rv) = NO
	    call rv_tempcodes (rv, RV_TXFD(rv))		# write out new codes
	    written = false

	} else {
	    call printf ("Current template image name = `%s'")
	         call pargstr (RIMAGE(rv))
	}

	call sfree (sp)
	return (OK)
end


# CMD_REGIONS -  Set/Show the current selected regions list.

int procedure cmd_regions (rv, ssp)

pointer	rv					#I RV struct pointer
pointer	ssp					#I Sample struct pointer

pointer	sp, buf
int	rv_load_sample()

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call aclrs (Memc[buf], SZ_FNAME)

	call gargstr (Memc[buf], SZ_FNAME)
	if (Memc[buf] != EOS) {
	    call rv_erase_regions (ssp, RV_GP(rv))
	    if (Memc[buf+1] == '*')
	        SR_COUNT(ssp) = ALL_SPECTRUM
	    else {
	        if (rv_load_sample(ssp, Memc[buf+1]) == ERR_CORREL) {
		    call sfree (sp)
	            return (ERR_CORREL)
		}
	    }
	    RV_NEWXCOR(rv) = YES

	    if (SR_COUNT(ssp) != ALL_SPECTRUM)
	        call rv_mark_regions (ssp, RV_GP(rv))
	    SR_MODIFY(ssp) = YES
	} else {
	    call rv_make_range_string (ssp, Memc[buf])
	    call printf ("Sample regions selected: `%s'")
	        call pargstr (Memc[buf])
	    call flush (STDOUT)
	}

	call sfree (sp)
	return (OK)
end


# CMD_RESULT - Page the logfile of results.

procedure cmd_result (rv)

pointer	rv					#I RV struct pointer

pointer	sp, cmd, buf, gp
int	open(), fstati()
errchk	open

begin
	gp = RV_GP(rv)
	if (gp == NULL)
	    return

	call smark (sp)
	call salloc (buf, SZ_FNAME, TY_CHAR)
	call salloc (cmd, SZ_FNAME, TY_CHAR)

    	# Page the logfile. Pointer is closed to be VMS compatible.
	call gargstr (Memc[cmd], SZ_FNAME)
	if (Memc[cmd] == EOS) {
    	    if (RV_TXFD(rv) != NULL) {
	        if (fstati(RV_TXFD(rv),F_FILESIZE) == 0) 
	    	    call rv_errmsg ("Nothing yet written to logfile.")
	        else {
	            call sprintf (Memc[buf], SZ_FNAME, "%s.txt\0")
	    	        call pargstr (SPOOL(rv))
	            call flush (RV_TXFD(rv))
	            call close (RV_TXFD(rv))
	            call gpagefile (gp, Memc[buf], "Log File of Results:")
	            RV_TXFD(rv) = open (Memc[buf], APPEND, TEXT_FILE)
	        }
    	    } else
	        call rv_errmsg ("No output file specified.")
	} else {
	    call sprintf (Memc[buf], SZ_FNAME, "%s.txt\0")
	        call pargstr (Memc[cmd])
	    call gpagefile (gp, Memc[buf], "Log File of Results:")
	}

	call sfree (sp)
end


# CMD_TEMPVEL - Set/show the known template velocity.

procedure cmd_tempvel (rv, tnum)

pointer	rv					#I RV struct pointer
int	tnum					#I Template number

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    TEMPVEL(rv,tnum) = rval
	    RV_NEWXCOR(rv) = YES
	} else {
	    call printf ("Template velocity = %g km/sec")
	        call pargr (TEMPVEL(rv,tnum))
	}
end


# CMD_TNUM - Get the specified template spectrum

int procedure cmd_tnum (rv, rinfile, written, cmdstr)

pointer rv                                      #I RV struct pointer
pointer rinfile                         	#I File list pointers
bool    written                                 #I Have data been written?
char    cmdstr[SZ_FNAME]                        #I Command string

pointer sp, cmd
int     i, tn, t1, t2, code
int	strlen(), get_spec(), imtrgetim()

define  exit_                   99

begin
        call smark (sp)
        call salloc (cmd, SZ_LINE, TY_CHAR)
        code = OK

        call sscan (cmdstr)
        call gargwrd (Memc[cmd], SZ_LINE)

	# Now get the requested template number.
        call gargstr (Memc[cmd], SZ_FNAME)
	for (i=0; IS_WHITE(Memc[cmd+i]); i=i+1)
	    ;
	if (IS_DIGIT(Memc[cmd+i])) {
	    call sscan (Memc[cmd+i])
	        call gargi (tn)
	    #RV_TEMPNUM(rv) = tn
	} else {
	    if (strlen(Memc[cmd+i]) == 1) {
	        tn = int (Memc[cmd+i])
	        if (IS_LOWER(tn)) tn = TO_UPPER(tn)
	        tn = tn - 'A' + 1
	    } else if (strlen(Memc[cmd+i]) == 2) {
		if (Memc[cmd+i] == ' ')
		    t1 = 0
		else {
		    t1 = int (Memc[cmd+i])
                    if (IS_LOWER(t1)) t1 = TO_UPPER(t1)
                    t1 = t1 - 'A' + 1
		}
		t2 = int (Memc[cmd+i])
                if (IS_LOWER(t2)) t2 = TO_UPPER(t2)
                t2 = t2 - 'A' + 1
		tn = t1 * 26 + t2
	    }
	}

	# Optimize.
	if (RV_TEMPNUM(rv) == tn) {
	    call nam_tempcode (tn, Memc[cmd])
	    call rv_errmsg ("Current template is already template `%s'.\n")
		call pargstr (Memc[cmd])
	    call sfree (sp)
	    return
	}

	# Check for the data write.
	call rv_do_save (rv, written)
 
	# Do the read on the template
	RV_TEMPNUM(rv) = tn
	if (imtrgetim(rinfile, RV_TEMPNUM(rv), RIMAGE(rv), SZ_FNAME) != EOF) {
	    if (get_spec (rv, RIMAGE(rv), REFER_SPECTRUM) == ERR_READ)
	        call error (0, "Error reading next template.")
	    call rv_imtitle (RIMAGE(rv), TEMPNAME(rv), SZ_FNAME)
	    written = false
	    RV_TEMPCODE(rv) = TEMPCODE(rv,RV_TEMPNUM(rv))
	    call amovkr (0.0, COEFF(rv,1), 4)
	    RV_FITDONE(rv) = NO
	    RV_NEWXCOR(rv) = YES
	    IS_DBLSTAR(rv) = NO
	} else
	    call rv_errmsg ("Error getting the requested template.")

exit_   call sfree (sp)
        return (code)
end


# CMD_TEXTCOLOR - Set/Show the text color.

procedure cmd_textcolor (rv)

pointer rv                                      #I RV struct pointer

int     ival, nscan()

begin
        call gargi (ival)
        if (nscan() == 2) {
            RV_TXTCOLOR(rv) = ival
        } else {
            call printf ("Text color = %d")
                call pargi (RV_TXTCOLOR(rv))
        }
end


# CMD_VERBOSE - Set/Show the verbose output flag.

procedure cmd_verbose (rv)

pointer	rv					#I RV struct pointer

pointer	sp, bp, op
int	verbose
int	cod_verbose()

begin
	call smark (sp)
	call salloc (bp, SZ_FNAME, TY_CHAR)
	call salloc (op, SZ_FNAME, TY_CHAR)

	call gargstr (Memc[bp], SZ_FNAME)
	if (Memc[bp] != EOS) {
            verbose = cod_verbose (Memc[bp])
            if (verbose == ERR) {
                call rv_errmsg (
		    "`verbose' must be one of `short|long|nogki|nolog|txtonly'")
		call sfree (sp)
		return
	    }
	    RV_VERBOSE(rv) = verbose
	} else {
	    call nam_verbose (rv, Memc[op])
	    call printf ("verbose = `%s'")
	        call pargstr (Memc[op])
	}

	call sfree (sp)
end


# CMD_VERSION - Development debug to print IRAF/RV version numbers.

procedure cmd_version ()

begin
	call printf ("RV Version: %s")
	    call pargstr (RV_VERSION)
end


# CMD_WEIGHTS - Get/Set the fitting weights.

procedure cmd_weights (rv)

pointer	rv					#I RV struct pointer

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    call rv_erase_fit (rv, false)
	    RV_WEIGHTS(rv) = rval
	    RV_FITDONE(rv) = NO
	} else {
	    call printf ("weights = %g")
	        call pargr (RV_WEIGHTS(rv))
	}
end


# CMD_WIDTH - Get/Set the fitting width.

procedure cmd_width (rv)

pointer	rv					#I RV struct pointer

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    if (!IS_INDEF(rval)) {
	        if (int(rval) > RV_CCFNPTS(rv)) {
		    call rv_errmsg ("Width is greater than npts in the ccf.")
		    return
	        }
	    }
	    call rv_erase_fit (rv, false)
	    RV_FITWIDTH(rv) = rval
	    RV_FITDONE(rv) = NO
	} else {
	    call printf ("width = %g")
	        call pargr (RV_FITWIDTH(rv))
	}
end


# CMD_WINCENTER - Get/Set the window center.

procedure cmd_wincenter (rv)

pointer	rv					#I RV struct pointer

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    RV_WINCENPAR(rv) = rval
	    call rv_batch_xcor (rv, RV_TEMPNUM(rv), RV_APNUM(rv), NO, YES, YES)
	} else {
	    if (RV_DCFLAG(rv) == -1 || RV_PIXCORR(rv) == YES) {
	        call printf ("wincenter = %d lags")
		    if (IS_INDEF(RV_WINPAR(rv)))
	                call pargi (INDEFI)
		    else
	                call pargi (int(RV_WINCENPAR(rv)))
	    } else {
	        call printf ("wincenter = %.2f Km/sec")
	            call pargr (RV_WINCENPAR(rv))
	    }
	}
end


# CMD_WINDOW - Set/show the current width of the peak window.

procedure cmd_window (rv)

pointer	rv						#I RV struct pointer

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    RV_WINPAR(rv) = rval
	    call rv_batch_xcor (rv, RV_TEMPNUM(rv), RV_APNUM(rv), NO, YES, YES)
	} else {
	    if (RV_DCFLAG(rv) == -1 || RV_PIXCORR(rv) == YES) {
	        call printf ("window = %d pixels")
		    if (IS_INDEF(RV_WINPAR(rv)))
	                call pargi (INDEFI)
		    else
	                call pargi (int(RV_WINPAR(rv)))
	    } else {
	        call printf ("window = %.2f Km/sec")
	            call pargr (RV_WINPAR(rv))
	    }
	}
end
 

# CMD_WRITE - Write results to logfile and/or header.

procedure cmd_write (rv, written)

pointer	rv					#I RV struct pointer
bool	written					#I data write flag

pointer	sp, fn, fname
pointer	gopen()
int	stat, scan()
bool	streq()
errchk	gopen

begin
	if (written && RV_UPDATE(rv) == NO)
	    return

	call smark (sp)
	call salloc (fn, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Make sure we have an output file.
        if (RV_TXFD(rv) == NULL) {
            call strcpy ("\0", Memc[fname], SZ_FNAME)
            while (Memc[fname] == '\0' && !streq(Memc[fname],"\"\"")) {
                call printf ("Root output filename: ")
                call flush (STDOUT)
                stat = scan()
                    call gargstr (Memc[fname], SZ_FNAME)
            }
            if (!streq(Memc[fname],"\"\"")) {
                call init_files (rv, DEVICE(rv), Memc[fname], true)
                RV_MGP(rv) = gopen ("stdvdm", APPEND, RV_GRFD(rv))
            }
        }
 
        if (!streq(Memc[fname],"\"\"")) {
	    call sprintf (Memc[fn], SZ_FNAME, "%s.txt")
	        call pargstr (SPOOL(rv))
	    call printf ("Writing current results to `%s'....")
	        call pargstr (Memc[fn])
	    call flush (STDOUT)

	    call rv_write (rv, RV_RECORD(rv))
	    call rv_eplot (rv, RV_MGP(rv))
	    #if (RV_VERBOSE(rv) == YES)
	        call rv_verbose_fit (rv, RV_VBFD(rv))
	    RV_RECORD(rv) = RV_RECORD(rv) + 1
	    written = true
	    RV_UPDATE(rv) = NO
	    call printf ("Done.\n")
	} else
	    call printf ("Results not saved.\n")

	call flush (STDOUT)
	call sfree (sp)
end


# CMD_YMAX - Set/show the top of the ccf plot window.

procedure cmd_ymax (rv)

pointer	rv						#I RV struct pointer

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    RV_Y2(rv) = rval
	    RV_NEWGRAPH(rv) = YES
	} else {
	    call printf ("Ymax = %f")
	        call pargr (RV_Y2(rv))
	}
end


# CMD_YMIN - Set/show the bottom of the ccf plot window.

procedure cmd_ymin (rv)

pointer	rv						#I RV struct pointer

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    RV_Y1(rv) = rval
	    RV_NEWGRAPH(rv) = YES
	} else {
	    call printf ("Ymin = %f")
	        call pargr (RV_Y1(rv))
	}
end
