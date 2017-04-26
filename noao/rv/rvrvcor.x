include <imio.h>
include <error.h>
include <time.h>
include "rvpackage.h"
include "rvflags.h"
include "rvkeywords.h"

# RV_RVCORRECT - Given the shift compute the observed and corrected velocity

int procedure rv_rvcorrect (rv, shift, sigma, vobs, vcor, verror)

pointer	rv				#I RV struct pointer
real	shift				#I Computed pixel shift
real	sigma				#I Sigma of fit
double	vobs				#O Observed object velocity
double	vcor				#O Corrected object velocity
double	verror				#O Error of velocity

double	ra, dec, ep, ut
double	jd, rhjd, hjd, vrot, vbary, vorb, vsol
double	ref_rvobs, ref_rvcor, ref_rvknown
double	delta_vel, dshift, deltav, dwpc
pointer imo, imr
int	day, month, year, stat

double	rv_shift2vel()
pointer	immap()
int	rv_gposinfo()
errchk  immap, imgetr

begin
	if (DBG_DEBUG(rv) == YES)
	  call d_printf(DBG_FD(rv), "rv_rvcorrect:\n")

	# Initialize some things.
	dshift = double (shift)
	deltav = double (RV_DELTAV(rv))
	dwpc = double (RV_RWPC(rv))

	# Check for a legal operation.
	if (RV_DCFLAG(rv) == -1 || RV_PIXCORR(rv) == YES) {
            vobs = INDEFD
	    vcor = INDEFD
	    RV_VREL(rv) = INDEFR
            RV_HJD(rv) = INDEFD
	    RV_MJD_OBS(rv) = INDEFD
	    return (OK)
	}

	# Map the images.
	imo = immap (IMAGE(rv), READ_ONLY, 0)
	imr = immap (RIMAGE(rv), READ_ONLY, 0)

	# Get the velocity from the reference star image header.
	if (IS_INDEF(TEMPVEL(rv,RV_TEMPNUM(rv)))) {
	    ref_rvknown = 0.0d0
	    call rv_err_comment (rv, 
	        "WARNING: Using template velocity of 0 km/s.", "")
	} else
	    ref_rvknown = double (TEMPVEL(rv,RV_TEMPNUM(rv)))

	# Compute the heliocentric correction for the reference star
	stat = rv_gposinfo (rv, imr, NO, ra, dec, ep, ut, day, month, year) 
	if (stat != OK) {			# Error reading header
            if (RV_DCFLAG(rv) == -1) {
                RV_VREL(rv) = INDEFR
                RV_PRINTZ(rv) = NO
            } else {
                RV_VREL(rv) = real (rv_shift2vel(rv,shift))
                if (abs(RV_VREL(rv)/C) >= RV_ZTHRESH(rv))
                    RV_PRINTZ(rv) = YES
                else
                    RV_PRINTZ(rv) = NO
            }
            vobs = INDEFD
	    vcor = INDEFD
            RV_HJD(rv) = INDEFD
	    RV_MJD_OBS(rv) = INDEFD
	    call imunmap (imo);
	    call imunmap (imr);
	    return (ERR_RVCOR)
	}
	call rv_corr (rv, imr, ra, dec, ep, year, month, day, ut, jd, rhjd, 
	    vrot, vbary, vorb, vsol)
	ref_rvcor = vrot + vbary + vorb		# Computed Helio RV correction
	ref_rvobs = ref_rvknown - ref_rvcor	# Observed RV of standard

	if (DBG_DEBUG(rv) == YES) {
	  call d_printf(DBG_FD(rv), "\tref:m/d/y,ra,dec,ut=%d/%d/%d,%h,%h,%h\n")
		call pargi(month);  call pargi(day);  call pargi(year)
		call pargd(ra);     call pargd(dec);  call pargd(ut)
	  call d_printf(DBG_FD(rv), "\t    jd = %g   r_hjd = %g\n")
		call pargd (jd);	call pargd (rhjd)
	  call d_printf(DBG_FD(rv), "\tref: vrot,vbary,vorb=%.4g,%.4g,%.4g\n")
		call pargd (vrot);  call pargd (vbary);  call pargd (vorb)
	}

        # Compute the wavelength/velocity shift
	if (RV_DCFLAG(rv) == -1)
	    RV_VREL(rv) = INDEFR
	else
	    RV_VREL(rv) = real (rv_shift2vel(rv,shift))
        vobs = ((1 + ref_rvobs/C) * (10**(dwpc * dshift)) - 1) * C

	# Set the output print format
	if (RV_PRINTZ(rv) == -1) {
	    if (abs(RV_VREL(rv)/C) >= RV_ZTHRESH(rv) && !IS_INDEF(RV_VREL(rv)))
	        RV_PRINTZ(rv) = YES
	    else
		RV_PRINTZ(rv) = NO
	}

	# Now correct observed velocity
	if (rv_gposinfo(rv,imo,YES,ra,dec,ep,ut,day,month,year)==ERR_RVCOR) {
	    call imunmap (imo);
	    call imunmap (imr);
	    return (ERR_RVCOR)
	}
	call rv_corr (rv, imo, ra, dec, ep, year, month, day, ut, jd, hjd, 
	    vrot, vbary, vorb, vsol)

	# Apply the corrections (+ vsol for correction to LSR)
	vcor = vobs + (vrot + vbary + vorb)	

        # Error computations - Kludge until antisymmetric computation
        #verror = double (sigma * deltav)

	if (DBG_DEBUG(rv) == YES) {
	  call d_printf(DBG_FD(rv), "\tobj:m/d/y,ra,dec,ut=%d/%d/%d,%h,%h,%h\n")
		call pargi(month);  call pargi(day);  call pargi(year)
		call pargd(ra);     call pargd(dec);  call pargd(ut)
	  call d_printf(DBG_FD(rv), "\t    jd = %g   hjd = %g\n")
		call pargd (jd);	call pargd (hjd)
	  call d_printf(DBG_FD(rv), "\tobj: vrot,vbary,vorb=%.4f,%.4f,%.4f\n")
		call pargd (vrot);  call pargd (vbary);  call pargd (vorb)
	  call d_printf(DBG_FD(rv), "\tshift,w0,wpc=%.4g,%.6g,%.6g\n")
		call pargr(shift);  call pargr(RV_RW0(rv))
		call pargr(RV_RWPC(rv))
	  call d_printf(DBG_FD(rv), "\tow0,rw0,dv=%.6g,%.6g,%.6g\n")
		call pargr(RV_OW0(rv));    call pargr(RV_RW0(rv));
		call pargd(delta_vel)
	  call d_printf(DBG_FD(rv), 
		"\tvrel,ref_rvcor,ref_rvobs=%.4f,%.4f,%.4f\n")
		    call pargr (RV_VREL(rv));	call pargd (ref_rvcor)
		    call pargd (ref_rvobs)
	  call d_printf(DBG_FD(rv), "\tvobs,vcor,verror=%.4g,%.4f,%.4g\n")
		call pargd (vobs);  call pargd (vcor);  call pargd (verror)
	  call d_flush (DBG_FD(rv))
	}

	# Miscellaneous info cleanup
	RV_HJD(rv) = hjd			# Object HJD
	RV_MJD_OBS(rv) = jd - 2400000.5d0	# Object MJD-OBS

	call imunmap (imo)			# Free image pointers
	call imunmap (imr)
	return (OK)
end


# RV_GPOSINFO - Get positional and time info about the observation from image
# header

int procedure rv_gposinfo (rv, im, is_obj, ra, dec, ep, ut, day, month, year)

pointer	rv				#I RV struct pointer
pointer	im				#I Image pointer
int	is_obj				#I Is image object image?
double	ra, dec, ep			#O position info
double  ut				#O UT of observation
int	day, month, year		#O Date of observation

double	ut_start, ut_mid, int_time, time, imgetd()
int	code, flags, idx
char 	buf[SZ_LINE]

int	rv_parse_date(), rv_parse_timed(), imaccf(), stridx()
errchk 	imgetd()

begin
	code = OK
	time = 0
	if (rv_parse_date (rv, im, KW_DATE_OBS(rv), is_obj, day, month, year,
	    time, flags) == ERR_RVCOR) {
	        code = ERR_RVCOR
	}
	if (rv_parse_timed (rv, im, is_obj, KW_RA(rv), ra) == ERR_RVCOR)
	    code = ERR_RVCOR
	if (rv_parse_timed (rv, im, is_obj, KW_DEC(rv), dec) == ERR_RVCOR)
	    code = ERR_RVCOR

	iferr (ep = imgetd (im, KW_EPOCH(rv))) {
	    call rv_err_comment (rv, "ERROR: Missing EPOCH keyword.", "")
	    code = ERR_RVCOR
	}
	ut_start = time

	# If we have a UTMIDDLE keyword use that as the midpoint of the
	# observation.

	if (imaccf(im,KW_UTMID(rv)) == YES) {
            # If this is a DATE-OBS type of string, look for the time delimiter.
	    call aclrc (buf, SZ_LINE)
	    call imgstr (im, KW_UTMID(rv), buf, SZ_LINE)
            idx = stridx("T", buf)

	    if (idx > 0) {
		if (rv_parse_date (rv, im, KW_UTMID(rv), is_obj, 
		    day, month, year, time, flags) == ERR_RVCOR) {
	        	code = ERR_RVCOR
		}
		ut = time
	    } 

	    if (idx == 0 || code == ERR_RVCOR) {
	        iferr (ut = imgetd (im, KW_UTMID(rv))) {
		    # Try to recover
	            if (rv_parse_timed (rv, im, is_obj, KW_UT(rv), ut_start) 
		        == ERR_RVCOR) {
	                    code = ERR_RVCOR
		    }
	            iferr (int_time = imgetd (im, KW_EXPTIME(rv))) {
	                call rv_err_comment (rv, 
			    "ERROR: Missing exposure time keyword.", "")
	                code = ERR_RVCOR
	            }
	            ut = double (ut_start + (int_time/3600.0)/2.0)
	        }
		ut_mid = ut
	    }
	} else {

	    # No UTMIDDLE keyword, so compute it from the UT and EXPTIME.
	    # Time specified in the DATE-OBS keyword, if present, takes
	    # precedence over UT keyword value.

	    if (flags != TF_OLDFITS && !IS_INDEFD(time)) {
		# Use the DATE-OBS time value as the UT.
		ut_start = time

	    } else if (imaccf(im,KW_UT(rv)) == YES) {
	        if (rv_parse_timed (rv, im, is_obj, KW_UT(rv),
		    ut_start) == ERR_RVCOR) {
	                code = ERR_RVCOR
			ut_start = 0.0d0
		}
	    } else {
	        call rv_err_comment (rv, "ERROR: Missing time keyword.", "")
		ut_start = 0.0d0
	    }

	    iferr (int_time = imgetd (im, KW_EXPTIME(rv))) {
	        call rv_err_comment (rv, 
		    "ERROR: Missing exposure time keyword.", "")
	        code = ERR_RVCOR
		int_time = 0.0d0
	    }
	    ut = double (ut_start + (int_time/3600.0)/2.0)
	    ut_mid = ut
	}

	# Check for a rollover of the date.  This can happen e.g. when
	# UT observing starts before midnight but the midpoint of the
	# exposure is after midnight, the date and time are then separated
	# by 24 hr.  Likewise, if the midpoint is less that the start
	# time, we've rolled over midnight.
	if (ut > 24.0) {
	    call rv_incr_date (day, month, year)
	    ut = ut - 24
	}
	if (ut_mid < ut_start)
	    call rv_incr_date (day, month, year)

	return (code)
end


# RV_INCR_DATE - Increment the date by 1 day, taking into account month,
# year and leap-year rollovers.

procedure rv_incr_date (day, month, year)

int	day, month, year		#I Date of observation

int     days_per_month[12]      # days per month
data    days_per_month/31,28,31,30,31,30,31,31,30,31,30,31/

begin
        if (mod (year, 4) == 0) 	# set the days/month for leap years
            days_per_month[2] = 29

	day = day + 1			# increment day
	if (day > days_per_month[month]) {
	    day = 1
	    month = month + 1
	} 
	if (month > 12) {
	    month = 1
	    year = year + 1
	} 
end


# RV_SHIFT2VEL - Compute a velocity from the given pixel shift.  Application
# of the heliocentric corrections is handled above.

double procedure rv_shift2vel (rv, shift)

pointer	rv					#I RV struct pointer
real	shift					#I Pixel shift in ccf

double	lambda, delta_lambda
double	dxp, vel

double	dex()

begin
	if (RV_DCFLAG(rv) == 0) {
	    # Compute the wavelength/velocity shift. (Use central wavelength)
	    delta_lambda = double (shift * RV_RWPC(rv))
	    lambda = double (RV_RW0(rv) + RV_RWPC(rv) * real(RV_RNPTS(rv)-1)/2.)
	    vel = double (delta_lambda / lambda * SPEED_OF_LIGHT)
	} else if (RV_DCFLAG(rv) == 1) {
	    # Below is the correct _relativistic_ redshift equation!
	    dxp = dex (RV_RWPC(rv) * shift) - 1.0d0
	    vel = SPEED_OF_LIGHT * dxp
	} else if (RV_DCFLAG(rv) == -1)
	    vel = INDEFD

	return (vel)
end


# RV_VEL2SHIFT - Compute a shift from the given velocity shift.

real procedure rv_vel2shift (rv, vel)

pointer	rv					#I RV struct pointer
real	vel					#I Pixel shift in ccf

real	lambda, shift

begin
	if (RV_DCFLAG(rv) == 0) {
	    lambda = double (RV_RW0(rv) + RV_RWPC(rv) * real(RV_RNPTS(rv)-1)/2.)
	    shift = double ((vel/C)*lambda) / double (RV_RWPC(rv))

	} else if (RV_DCFLAG(rv) == 1) {
	    shift = log10 (vel / C + 1) / double (RV_RWPC(rv))

	} else if (RV_DCFLAG(rv) == -1)
	    shift = INDEFR

	return (shift)
end


# RV_CORRECT - Compute the radial velocity corrections.

procedure rv_corr (rv, im, ra, dec, ep, year, month, day, ut, jd, hjd, vrot, 
    vbary, vorb, vsol)

pointer	rv				#I RV struct pointer
pointer	im				#I Image descriptor
double	ra, dec, ep			#I Positional info
int	year, month, day		#I Date of obs info
double	ut				#I Time of info
double  jd				#I JD of observation
double	hjd				#I Heliocentric Julian Date
double	vrot				#O Correction for E rotation
double	vbary				#O Correction for E-M barycentre
double	vorb				#O Correction for E orbital movement
double	vsol				#O Correction to LSR

double 	epoch
double	ra_obs, dec_obs, t
double	lat, lon, alt
double  ast_julday()
bool	newobs, obshead
double 	obsgetd()
pointer	obspars()

begin
	call obsimopen (RV_OBSPTR(rv), im, 
            Memc[P2C(obspars(RV_OBSPTR(rv), "observatory"))], 
	    NO, newobs, obshead)
	if (newobs || obshead) {
	    RV_LATITUDE(rv) = real (obsgetd (RV_OBSPTR(rv), "latitude"))
	    RV_LONGITUDE(rv) = real (obsgetd (RV_OBSPTR(rv), "longitude"))
	    RV_ALTITUDE(rv) = real (obsgetd (RV_OBSPTR(rv), "altitude"))
	}
	lat  = double (RV_LATITUDE(rv))
	lon  = double (RV_LONGITUDE(rv))
	alt  = double (RV_ALTITUDE(rv))

	# Determine epoch of observation and precess coordinates.
	call ast_date_to_epoch (year, month, day, ut, epoch)
	call ast_precess (ra, dec, ep, ra_obs, dec_obs, epoch)
	call ast_hjd (ra_obs, dec_obs, epoch, t, hjd)

	# Determine velocity components.
	call ast_vbary (ra_obs, dec_obs, epoch, vbary)
	call ast_vrotate (ra_obs, dec_obs, epoch, lat, lon, alt, vrot)
	call ast_vorbit (ra_obs, dec_obs, epoch, vorb)

	jd = ast_julday (epoch)
	vsol = 0.0

	if (DBG_DEBUG(rv) == YES) {
	  call d_printf(DBG_FD(rv), "\tlat=%.5f long=%.5f alt=%.5f\n")
		call pargd(lat);  call pargd(lon);  call pargd(alt)
	}
end


# RV_PARSE_DATE - Parse a date string and return components

int procedure rv_parse_date (rv, im, param, is_obj, day, month, year, tm, flags)

pointer	rv				#I rv struct pointer
pointer	im		 		#I image pointer
char	param[SZ_LINE]			#I image parameter to get
int	is_obj				#I is image object image?
int	day, month, year		#O date components
double	tm				#O time string
int	flags				#O flags

char	date[SZ_FNAME]

int	dtm_decode()
errchk  imgstr()

begin
	iferr (call imgstr(im, param, date, SZ_LINE)) {
	    if (is_obj == YES) {
	        call rv_err_comment (rv, 
	    	    "ERROR: Error getting '%s' from object image header.",param)
	    } else {
	        call rv_err_comment (rv, 
	    	    "ERROR: Error getting '%s' from temp image header.",param)
	    }
	    call flush (STDERR)
	    return (ERR_RVCOR)
	}

	# Decode the keyword.  We ignore any time information since this
	# is obtained from other keywords.
        if (dtm_decode (date, year, month, day, tm, flags) == ERR) {
	    call rv_err_comment (rv, 
		"ERROR: Error parsing date keyword.", "")
	    return (ERR_RVCOR)
	}

	return (OK)
end


# RV_PARSE_TIMED - Utility to read a sexigimal field and return the answer
# as  decimal hours or degrees.  The fields are decoded as follows:
#
#      hh:mm:ss.ss  ->  hh + mm/60. + ss.ss / 3600.

int procedure rv_parse_timed (rv, im, is_obj, param, dval)

pointer	rv				#I RV struct pointer
pointer	im				#I Image pointer
int	is_obj				#I Is image object image?
char 	param[SZ_LINE]			#I Image parameter to read
double	dval				#O Output answer

pointer	sp, buf
int	res, flag, idx
int     yr, mon, day, hr, min
double  sec

int     dtm_decode_hms(), stridx()
errchk  imgstr

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	dval = INDEFD


        iferr (call imgstr(im, param, Memc[buf], SZ_FNAME)) {
	    if (is_obj == YES) { 
	        call rv_err_comment (rv,
	    	    "ERROR: Error getting '%s' from object image header.",param)
	    } else {
	        call rv_err_comment (rv,
	    	    "ERROR: Error getting '%s' from temp image header.", param)
	    }
	    call sfree (sp)
	    call flush (STDERR)
	    return (ERR_RVCOR)
	}


	# If this is a DATE-OBS type of string, look for the time delimiter.
	idx = stridx("T", Memc[buf])

	# Allow only sexigesimal or decimal values.
    	if (stridx(":", Memc[buf]) == 0 && stridx (".", Memc[buf]) == 0) {
	    if (is_obj == YES) { 
	        call rv_err_comment (rv,
	    	    "ERROR: Invalid time string '%s' from object header.",param)
	    } else {
	        call rv_err_comment (rv,
	    	    "ERROR: Invalid time string '%s' from temp header.", param)
	    }
	    call sfree (sp)
	    call flush (STDERR)
	    return (ERR_RVCOR)
	}


	if (idx > 0) {
	    # Decode the date-obs string for the time.
            res = dtm_decode_hms (Memc[buf], yr, mon, day, hr, min, sec, flag)
            dval = hr + double(min) / 60.0 + sec / 3600.0

        } else {
	    # Let gargd() decode the sexigesimal field.
	    call sscan (Memc[buf])
	        call gargd (dval)
        }


	call sfree (sp)
	return (OK)
end
