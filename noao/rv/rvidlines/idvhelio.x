include	<error.h>

# ID_VHELIO -- Compute helocentric velocity.

procedure id_vhelio (im, vhelio, hjd, fd)

pointer	im			#I IMIO pointer
double	vhelio			#O Heliocentric velocity correction
double	hjd			#O Heliocentric Julian Date
int	fd			#I Log file descriptor

bool	newobs, obshead
int	year, month, day, flags, dummy
double	ra, dec, ep, ut, lt
double	epoch, vrot, vbary, vorb
double	latitude, longitude, altitude
pointer	sp, str1, str2, tmp, obs, kp

int	dtm_decode()
double	imgetd(), obsgetd()
pointer	clopset()

int	err
data	err/0/

errchk	imgetd, imgstr, obsopen


begin
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	obs = NULL

	iferr {
	    # Get the observatory data.
	    call clgstr ("observatory", Memc[str1], SZ_LINE)
	    tmp = NULL
	    call obsimopen (tmp, im, Memc[str1], NO, newobs, obshead)
	    obs = tmp

	    latitude = obsgetd (obs, "latitude")
	    longitude = obsgetd (obs, "longitude")
	    altitude = obsgetd (obs, "altitude")

	    # Get the image header data.
	    kp = clopset ("keywpars")

	    call clgpset (kp, "date_obs", Memc[str1], SZ_LINE)
	    call imgstr (im, Memc[str1], Memc[str2], SZ_LINE)
	    if (dtm_decode (Memc[str2],year,month,day,ut,flags) == ERR)
		call error (1, "Error in date string")

	    call clgpset (kp, "ut", Memc[str1], SZ_LINE)
	    call imgstr (im, Memc[str1], Memc[str2], SZ_LINE)
	    if (dtm_decode (Memc[str2],dummy,dummy,dummy,ut,flags) == ERR) {
		iferr (ut = imgetd (im, Memc[str1]))
		    call error (1, "Error in UT keyword")
	    }

	    call clgpset (kp, "ra", Memc[str1], SZ_LINE)
	    ra = imgetd (im, Memc[str1])
	    call clgpset (kp, "dec", Memc[str1], SZ_LINE)
	    dec = imgetd (im, Memc[str1])
	    call clgpset (kp, "epoch", Memc[str1], SZ_LINE)
	    ep = imgetd (im, Memc[str1])
	    call clcpset (kp)

	    # Determine epoch of observation and precess coordinates.
	    call ast_date_to_epoch (year, month, day, ut, epoch)
	    call ast_precess (ra, dec, ep, ra, dec, epoch)

	    # Determine velocity components.
	    call ast_vorbit (ra, dec, epoch, vorb)
	    call ast_vbary (ra, dec, epoch, vbary)
	    call ast_vrotate (ra, dec, epoch, latitude, longitude,
		altitude, vrot)
	    call ast_hjd (ra, dec, epoch, lt, hjd)

	    vhelio = vrot + vbary + vorb

	    if (fd != NULL)
		call obslog (obs,
		    "RVIDLINES", "latitude longitude altitude", fd)

	} then {
	    vhelio = 0.
	    if (err == 0) {
		call eprintf ("Warning: Can't compute heliocentric velocity\n")
		call erract (EA_WARN)
		err = err + 1
	    }
	}

	iferr {		# This IFERR is to clear errcode.
	    if (obs != NULL)
		call obsclose (obs)
	} then
	    ;
	call sfree (sp)
end
