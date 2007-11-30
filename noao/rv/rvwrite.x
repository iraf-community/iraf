include <imhdr.h>
include "rvpackage.h"
include "rvflags.h"
include "rvkeywords.h"
include "rvsample.h"
include "rvcont.h"

# RV_WRITE - Write results to logfile and/or header.

procedure rv_write (rv, record)

pointer	rv			#I RV struct pointer
int	record			#I Record number being written

pointer	sp, fd, tasknm, buf

begin
	fd = RV_TXFD(rv)
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (tasknm, SZ_FNAME, TY_CHAR)

	# Update the image header if set
	if (RV_IMUPDATE(rv) == YES) 
	    call rv_imupdate (rv)

	# write long explanatory header
	if (record <= 0) {
	    call rv_param (rv, fd, "fxcor")
	    call rv_tempcodes (rv, fd)
	    call rv_prdeltav (rv, fd)
	    call fprintf (fd, "# \n")
	    call rv_hdr (rv, fd)
	}

	# Update dispersion and the sample regions if they were modified.
	if (SR_MODIFY(RV_OSAMPLE(rv)) == YES) {
            call rv_make_range_string (RV_OSAMPLE(rv), Memc[buf])
            call rv_sparam (RV_TXFD(rv), "osample", Memc[buf], "", "")
	    SR_MODIFY(RV_OSAMPLE(rv)) = NO
	}
	if (SR_MODIFY(RV_RSAMPLE(rv)) == YES) {
            call rv_make_range_string (RV_RSAMPLE(rv), Memc[buf])
            call rv_sparam (RV_TXFD(rv), "rsample", Memc[buf], "", "")
	    SR_MODIFY(RV_RSAMPLE(rv)) = NO
	}

	if (RV_VERBOSE(rv) == OF_SHORT || RV_VERBOSE(rv) == OF_STXTONLY)
	    call rv_write_short (rv, fd)
	else
	    call rv_write_long (rv, fd)

	call sfree (sp)
end

# Define the parameter keyword strings in Km/s units.

define	RV_NSTR1S "#N OBJECT%13tIMAGE%24tREF%29tHJD%40tAP%44tSHIFT%53tFWHM%62tVHELIO%72tVERR\n"
define	RV_USTR1S "#U name%13timage%29tdays%44tpixel%53t    %62tkm/s%72tkm/s\n"
define	RV_WSTR1S "%-11.11s%13t%-10.10s%24t%.2s%29t%-10.5f%40t%-3d%44t%-7.3f%53t%-7.2f%62t%-9.4f%72t%-7.3f\n"

define  RV_NSTR1V "#N%4tOBJECT%18tIMAGE%28tREF%34tHJD%44tAP%50tCODES%60tSHIFT%68tHGHT%73tFWHM%81tTDR%88tVOBS%98tVREL%109tVHELIO%120tVERR\n"
define  RV_USTR1V "#U%4tname%18timage%34tdays%50tcfr/fun%60tpixel%73t    %88tkm/s%98tkm/s%109tkm/s%120tkm/s\n"
define  RV_WSTR1V "%-15.15s  %-10s %.2s  %-11.5f %-3d  %-7.7s   %-7.3f %-4.2f %-7.2f %-6.2f %-9.4f %-9.4f  %-9.4f  %-7.3f\n"

# Now define the parameter keyword strings in terms of redshift Z values.

define	RV_NSTR1SZ "#N OBJECT%13tIMAGE%24tREF%28tHJD%40tAP%44tSHIFT%53tFWHM%62tZHELIO%72tVERR\n"
define	RV_USTR1SZ "#U name%13timage%28tdays%44tpixel%53t    %62tz%72tkm/s\n"
define	RV_WSTR1SZ "%-11.11s%13t%-10.10s%24t%.2s%28t%-10.5f%40t%-3d%44t%-7.3f%53t%-7.2f%62t%-7.6f%72t%-7.3f\n"

define  RV_NSTR1VZ "#N%4tOBJECT%18tIMAGE%28tREF%34tHJD%44tAP%50tCODES%60tSHIFT%68tHGHT%73tFWHM%81tTDR%88tZOBS%98tZREL%109tZHELIO%120tVERR\n"
define  RV_USTR1VZ "#U%4tname%18timage%34tdays%50tcfr/fun%60tpixel%73t    %88tz%98tz%109tz%120tkm/s\n"
define  RV_WSTR1VZ "%-15.15s  %-10s %.2s  %-11.5f %-3d  %-7.7s   %-7.3f %-4.2f %-7.2f %-6.2f %-7.6f    %-7.6f     %-7.6f    %-7.3f\n"



# RV_WRITELN - Write out a line to the status line.

procedure rv_writeln (rv, fd)

pointer	rv				#I RV struct pointer
pointer	fd				#I File descriptor

begin
	if (fd == NULL)
	    return

	# Check error conditions
	if (RV_ERRCODE(rv) == ERR_FIT) {
	    call fprintf (fd, "Fit did not converge.\n")
	    return
	}

	if (RV_PRINTZ(rv) == ERR)  	# bad velocity computation
	    RV_PRINTZ(rv) = NO

	# Write the status line output
	if ((mod(RV_STATLINE(rv),2) == 0 || RV_NEWXCOR(rv) == YES) &&
	    RV_DCFLAG(rv) != -1) {
	    if (RV_PRINTZ(rv) == NO) {
                call fprintf (fd,
	           "HJD=%9.4f  FWHM=%.2f  Vr=%.3f  Vo=%.3f  Vh=%.3f +/- %.3f\n")
	    } else {
                call fprintf (fd,
	           "HJD=%9.4f  FWHM=%.2f  Zr=%.5f  Zo=%.5f  Zh=%.5f +/- %.3f\n")
	    }
	            if (RV_HJD(rv) > 0.0)
	                call pargd (mod(RV_HJD(rv),double(10000.0)))
		    else
	                call pargr (INDEF)
	            call pargr (RV_DISP(rv))
		    if (RV_PRINTZ(rv) == NO) {
	                call pargr (RV_VREL(rv))
	                call pargd (RV_VOBS(rv))
	                call pargd (RV_VCOR(rv))
		    } else {
			if (IS_INDEFD(RV_VREL(rv)))
	                    call pargd (INDEFD)
			else
	                    call pargd (RV_VREL(rv)/C)
			if (IS_INDEFD(RV_VOBS(rv)))
	                    call pargd (INDEFD)
			else
	                    call pargd (RV_VOBS(rv)/C)
			if (IS_INDEFD(RV_VCOR(rv)))
	                    call pargd (INDEFD)
			else
	                    call pargd (RV_VCOR(rv)/C)
		    }
	            call pargd (RV_ERROR(rv))
	} else {
            call fprintf (fd, 
		"Shift=%.5g +/- %.3g pixels  CCF Height=%.3g  FWHM = %.3g")
	            call pargr (RV_SHIFT(rv))
	            call pargr (RV_SIGMA(rv))
	            call pargr (RV_HEIGHT(rv))
	            call pargr (RV_FWHM(rv))
	}

	call flush (fd)
	RV_ERRCODE(rv) = OK
end


# RV_WRITE_SHORT - Write out a line to the logfile or the screen.

procedure rv_write_short (rv, fd)

pointer	rv				#I RV struct pointer
pointer	fd				#I File descriptor

int	i, nshifts
real	wpc
char	tc[3]

data	wpc /INDEF/

begin
	if (fd == NULL)
	    return
	if (RV_PRINTZ(rv) == ERR)  	# bad velocity computation
	    RV_PRINTZ(rv) = NO

	if (IS_INDEF(wpc))				# update dispersion
	    wpc = RV_OWPC(rv)
	else if (wpc != RV_OWPC(rv)) {
	    call rv_prdeltav (rv, fd)
	    wpc = RV_OWPC(rv)
	} else
	    wpc = RV_OWPC(rv)

	if (IS_DBLSTAR(rv) == YES) {
	    nshifts = DBL_NSHIFTS(rv)
	    do i = 1, nshifts {
	        call nam_tempcode (RV_TEMPNUM(rv), tc)
		if (RV_PRINTZ(rv) == NO)
                    call fprintf (fd, RV_WSTR1S)
		else
                    call fprintf (fd, RV_WSTR1SZ)
		        call pargstr (OBJNAME(rv))
		        call pargstr (IMAGE(rv))
		        #call pargi (RV_TEMPCODE(rv))
		        call pargstr (tc)
		        if (!IS_INDEF(DBL_VHELIO(rv,i)) && RV_HJD(rv) > 0)
	                    call pargd (mod(RV_HJD(rv),double(10000.0)))
		        else
		            call pargr (INDEF)
		        call pargi (RV_APNUM(rv))
		        call pargr (DBL_SHIFT(rv,i))
		        call pargr (DBL_FWHM(rv,i))
		        if (RV_DCFLAG(rv) != -1) {
			    if (RV_PRINTZ(rv) == NO)
		                call pargr (DBL_VHELIO(rv,i))
			    else {
				if (IS_INDEFD(DBL_VHELIO(rv,i)))
		                    call pargd (INDEFD)
				else
		                    call pargd (DBL_VHELIO(rv,i)/C)
			    }
		        } else
		            call pargr (INDEFR)
		        call pargr (DBL_VERR(rv,i))
	    }
	} else {
	    call nam_tempcode (RV_TEMPNUM(rv), tc)
	    if (RV_PRINTZ(rv) == NO)
                call fprintf (fd, RV_WSTR1S)
	    else
                call fprintf (fd, RV_WSTR1SZ)
	            call pargstr (OBJNAME(rv))
	            call pargstr (IMAGE(rv))
	            #call pargi (RV_TEMPCODE(rv))
	            call pargstr (tc)
	            if (!IS_INDEFD(RV_VCOR(rv)) && RV_ERRCODE(rv) == OK && 
		        RV_HJD(rv) > 0.)
	                    call pargd (mod(RV_HJD(rv),double(10000.0)))
	            else
	                call pargr (INDEF)
	            call pargi (RV_APNUM(rv))
		    if (RV_ERRCODE(rv) == OK) {
	                call pargr (RV_SHIFT(rv))
			if (!IS_INDEFR(RV_DISP(rv)))
	                    call pargr (RV_DISP(rv))
			else
	                    call pargr (RV_FWHM(rv))
		        if (RV_PRINTZ(rv) == NO)
	                    call pargd (RV_VCOR(rv))
		        else {
			    if (IS_INDEFD(RV_VCOR(rv)))
	                        call pargd (INDEFD)
			    else
	                        call pargd (RV_VCOR(rv)/C)
			}
	                call pargd (RV_ERROR(rv))
		    } else {
	                call pargr (INDEF)
	                call pargr (INDEF)
	                call pargr (INDEF)
	                call pargr (INDEF)
		    }
	}

	call flush (fd)
end


# RV_WRITE_LONG - Write the verbose output line

procedure rv_write_long (rv, fd)

pointer	rv					#I RV struct pointer
int	fd					#I File descriptor

pointer	sp, cp
int	i, nshifts
double	vrel, rv_shift2vel()
real	wpc
char	tc[3]

data	wpc /INDEF/

begin
	if (fd == NULL)
	    return
	if (RV_PRINTZ(rv) == ERR)  	# bad velocity computation
	    RV_PRINTZ(rv) = NO

	call smark (sp)
	call salloc (cp, SZ_FNAME, TY_CHAR)

	# First encode some stuff for output
	call rv_codes (rv, Memc[cp], SZ_FNAME)

	if (IS_INDEF(wpc))				# update dispersion
	    wpc = RV_OWPC(rv)
	else if (wpc != RV_OWPC(rv)) {
	    call rv_prdeltav (rv, fd)
	    wpc = RV_OWPC(rv)
	} else
	    wpc = RV_OWPC(rv)

	if (IS_DBLSTAR(rv) == YES) {
	    nshifts = DBL_NSHIFTS(rv)
	    do i = 1, nshifts {
		call nam_tempcode (RV_TEMPNUM(rv), tc)
	        if (RV_PRINTZ(rv) == NO)
                    call fprintf (fd, RV_WSTR1V)
	        else
                    call fprintf (fd, RV_WSTR1VZ)
	    	        call pargstr (OBJNAME(rv))
	    	        call pargstr (IMAGE(rv))
	    	        call pargstr (tc)
		        if (!IS_INDEF(DBL_VHELIO(rv,i)) && RV_HJD(rv) > 0.0)
	                    call pargd (mod(RV_HJD(rv),double(10000.0)))
		        else
	    	            call pargr (INDEF)
	    	        call pargi (RV_APNUM(rv))
	    	        call pargstr (Memc[cp])
	    	        call pargr (DBL_SHIFT(rv,i))
	    	        call pargr (DBL_HEIGHT(rv,i))
	    	        call pargr (DBL_FWHM(rv,i))
	    	        call pargr (DBL_R(rv,i))
		        if (RV_DCFLAG(rv) != -1) {
		            if (RV_PRINTZ(rv) == NO) {
	    	                call pargr (DBL_VOBS(rv,i))
	    	                call pargd (rv_shift2vel(rv,DBL_SHIFT(rv,i)))
	    	                call pargr (DBL_VHELIO(rv,i))
			    } else {
				if (IS_INDEFD(DBL_VOBS(rv,i)))
				    call pargd (INDEFD)
				else
	    	                    call pargd (DBL_VOBS(rv,i)/C)
				vrel = rv_shift2vel (rv,DBL_SHIFT(rv,i)) / C
				if (IS_INDEFD(vrel)) 
				    call pargd (INDEFD)
				else
	    	                    call pargd (vrel)
				if (IS_INDEFD(DBL_VHELIO(rv,i)))
				    call pargd (INDEFD)
				else
	    	                    call pargd (DBL_VHELIO(rv,i)/C)
			    }
		        } else {
	    	            call pargr (INDEFR)
	    	            call pargr (INDEFR)
	    	            call pargr (INDEFR)
		        }
	    	        call pargr (DBL_VERR(rv,i))
	    }
	} else {
	    call nam_tempcode (RV_TEMPNUM(rv), tc)
	    if (RV_PRINTZ(rv) == NO)
                call fprintf (fd, RV_WSTR1V)
	    else
                call fprintf (fd, RV_WSTR1VZ)
	            call pargstr (OBJNAME(rv))
	            call pargstr (IMAGE(rv))
	            #call pargi (RV_TEMPCODE(rv))
	            call pargstr (tc)
		    if (!IS_INDEFD(RV_VCOR(rv)) && RV_ERRCODE(rv) == OK &&
		        RV_HJD(rv) > 0.0)
	                    call pargd (mod(RV_HJD(rv),double(10000.0)))
		    else
	                call pargr (INDEF)
	            call pargi (RV_APNUM(rv))
	            call pargstr (Memc[cp])
		    if (RV_ERRCODE(rv) == OK) {
	                call pargr (RV_SHIFT(rv))
	                call pargr (RV_HEIGHT(rv))
			if (!IS_INDEFR(RV_DISP(rv)))
	                    call pargr (RV_DISP(rv))
	                else
	                    call pargr (RV_FWHM(rv))
	                call pargr (RV_R(rv))
		        if (RV_PRINTZ(rv) == NO) {
	                    call pargd (RV_VOBS(rv))
	                    call pargr (RV_VREL(rv))
	                    call pargd (RV_VCOR(rv))
		        } else {
			    if (IS_INDEFD(RV_VOBS(rv)))
				call pargd (INDEFD)
			    else
	                        call pargd (RV_VOBS(rv)/C)
			    if (IS_INDEFD(RV_VREL(rv)))
				call pargd (INDEFD)
			    else
	                        call pargd (RV_VREL(rv)/C)
			    if (IS_INDEFD(RV_VCOR(rv)))
				call pargd (INDEFD)
			    else
	                        call pargd (RV_VCOR(rv)/C)
		        }
	                call pargd (RV_ERROR(rv))
		    } else {
	                call pargr (INDEF)
	                call pargr (INDEF)
	                call pargr (INDEF)
	                call pargr (INDEF)
	                call pargd (INDEFD)
	                call pargr (INDEF)
	                call pargd (INDEFD)
	                call pargd (INDEFD)
		    }
	}

	call flush (fd)
	call sfree (sp)
end


# RV_SHORT_HDR - Write out a line to the logfile or the screen.

procedure rv_short_hdr (rv, fd)

pointer	rv				#I RV struct pointer
pointer	fd				#I File descriptor

begin
	if (fd == NULL)
	    return
	if (RV_PRINTZ(rv) == ERR)  	# bad velocity computation
	    RV_PRINTZ(rv) = NO

	if (RV_PRINTZ(rv) == NO) {
            call fprintf (fd, RV_NSTR1S)
            call fprintf (fd, RV_USTR1S)
	} else {
            call fprintf (fd, RV_NSTR1SZ)
            call fprintf (fd, RV_USTR1SZ)
	}
        call fprintf (fd, "# \n")

	call flush (fd)
end


# RV_HDR - Procedure to write the banner for the RVXCOR task.

procedure rv_hdr (rv, fd)

pointer	rv					#I RV struct pointer
int	fd					#I File descriptor

begin
	if (fd == NULL)
	    return
	if (RV_PRINTZ(rv) == ERR)  		# bad velocity computation
	    RV_PRINTZ(rv) = NO

	call flush (fd)
	if (RV_VERBOSE(rv) == OF_SHORT || RV_VERBOSE(rv) == OF_STXTONLY) {
	    call rv_short_hdr (rv, fd) 		# write short header
	    return
	}

	if (RV_PRINTZ(rv) == NO) {
            call fprintf (fd, RV_NSTR1V)
            call fprintf (fd, RV_USTR1V)
	} else {
            call fprintf (fd, RV_NSTR1VZ)
            call fprintf (fd, RV_USTR1VZ)
	}
	call fprintf (fd, "#\n")

	call flush (fd)
end


# RV_CODES - Encode certain parameters into a string for output

procedure rv_codes (rv, out, maxch)

pointer	rv					#I RV struct pointer
char 	out[maxch]				#O Output code string
int	maxch

pointer	sp, func

begin
	call smark (sp)
	call salloc (func, SZ_FNAME, TY_CHAR)
	call nam_fitfunc (rv, Memc[func])

	call sprintf (out, maxch, "%c%c%c/%-3.3s")
	    switch (RV_CONTINUUM(rv)) {
	    case OBJ_ONLY:
		call pargi ('O')
	    case TEMP_ONLY:
		call pargi ('T')
	    case BOTH:
		call pargi ('B')
	    case NONE:
		call pargi ('N')
	    }
	    switch (RV_FILTER(rv)) {
	    case OBJ_ONLY:
		call pargi ('O')
	    case TEMP_ONLY:
		call pargi ('T')
	    case BOTH:
		call pargi ('B')
	    case NONE:
		call pargi ('N')
	    }
	    switch (RV_REBIN(rv)) {
	    case RB_OBJ:
		call pargi ('O')
	    case RB_TEMP:
		call pargi ('T')
	    case RB_SMALL:
		call pargi ('S')
	    case RB_BIG:
		call pargi ('L')
	    }
	    call pargstr (Memc[func])

	call sfree (sp)
end


# RV_TEMPCODES - Output the template codes and information

procedure rv_tempcodes (rv, fd)

pointer	rv					#I RV struct pointer
int	fd					#i Output file descriptor

pointer sp, buf, im, title
int	i, imtrgetim(), strlen()
char	tc[3]

begin
	if (fd == NULL)
	    return

	call smark (sp)
	call salloc (buf, 4*SZ_LINE, TY_CHAR)
	call salloc (im, SZ_LINE, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)

	call fprintf (fd, "# \n")
	do i = 1, RV_NTEMPS(rv) {

	    if (imtrgetim(RV_TEMPLATES(rv), i, Memc[im], SZ_FNAME) != EOF)
                call rv_imtitle (Memc[im], Memc[title], SZ_FNAME)

	    # Truncate the leading path if needed.
	    if (strlen(Memc[im]) > 30)
		call rv_strip_path (Memc[im], 30)

	    call nam_tempcode (i, tc)
	    call sprintf (Memc[buf], SZ_LINE, 
	   "#T %s '%.2s' -- %s = '%.40s'%78t\*\n#%21t%s = '%.30s'%64t%s = %.2f")
		    call pargstr ("Template")
		    call pargstr (tc)
		    call pargstr ("Object")
		    call pargstr (Memc[title])
		    call pargstr ("Image ")
		    call pargstr (Memc[im])
		    call pargstr ("Vhelio")
		    call pargr (TEMPVEL(rv,i))
	    call fprintf (fd, "%s\n")
		call pargstr (Memc[buf])
	}
	call fprintf (fd, "# \n")

	call sfree (sp)
end


# RV_PRDELTAV - Output the velocity per pixel dispersion of the image.

procedure rv_prdeltav (rv, fd)

pointer	rv					#I RV struct pointer
int	fd					#I Output file descriptor

begin
	if (fd == NULL)
	    return

	call fprintf (fd, "#  Velocity Dispersion = %-.2f Km/sec/pixel     ")
	    call pargr (RV_DELTAV(rv))
	call fprintf (fd, "Rebinned WPC = %-.6g\n")
	    if (RV_DCFLAG(rv) != -1)
	        call pargr (RV_OWPC(rv))
	    else
	        call pargr (INDEFR)
	call flush (fd)
end


# RV_IMUPDATE - Update the image header with requested information

procedure rv_imupdate (rv)

pointer	rv					#I RV struct pointer

pointer	im, immap()
int 	imaccf()
errchk	immap, imaccf, imaddr

begin
	im = immap (IMAGE(rv), READ_WRITE, 0)
	if (IM_LEN(im,2) > 1 && IM_NDIM(im) > 1) {
	     call rv_err_comment (rv, 
		"WARNING: Cannot currently update a two-dimensional image.",
		"")

	} else {
	    # Write observed & corrected velocity to image header
	    iferr {
	        if (imaccf(im,KW_HJD(rv)) == NO)
	            call imaddd (im, KW_HJD(rv), RV_HJD(rv))
		else
		    call imputd (im, KW_HJD(rv), RV_HJD(rv))
	        if (imaccf(im,KW_MJD_OBS(rv)) == NO)
		    call imaddd (im, KW_MJD_OBS(rv), RV_MJD_OBS(rv))
		else
		    call imputd (im, KW_MJD_OBS(rv), RV_MJD_OBS(rv))
	        if (imaccf(im,KW_VOBS(rv)) == NO)
		    call imaddd (im, KW_VOBS(rv), RV_VOBS(rv))
		else
		    call imputd (im, KW_VOBS(rv), RV_VOBS(rv))
		if (imaccf(im,KW_VHELIO(rv)) == NO)
		    call imaddd (im, KW_VHELIO(rv), RV_VCOR(rv))
		else
		    call imputd (im, KW_VHELIO(rv), RV_VCOR(rv))
		} then
		    call rv_err_comment (rv, 
			" ERROR: Error updating image header.", "")
	}
	call imunmap (im)
end


# RV_STRIP_PATH -  Truncate a string to the rightmost number of characters
# specified.

procedure rv_strip_path (str, maxch)

char	str[ARB]				#I Input string
int	maxch					#I Maxchars

int	ip, strlen()

begin
	ip = max (1, strlen (str) - maxch + 1)
	call amovc (str[ip], str, maxch)
	str[31] = '\0'
end
