include "rvpackage.h"
include "rvcomdef.h"
include "rvkeywords.h"

.help keywpars
.nf ___________________________________________________________________________
KEYWPARS  - Support routines for the 'keywpars' named external pset.  

	This file include routines for opening/closing the keyword structure 
as well as command handling.  Command handling is limited to changing the 
parameter values or resetting them to the default values.  Routines included
here are as follows.

	 	        keyw_open (rv)
	               keyw_close (rv)
	            keyw_get_pars (rv)
		   keyw_parupdate (rv)
		     keyw_unlearn (rv)
		        keyw_show (rv)
		       keyw_colon (rv, cmdstr)
			   cmd_ra (rv)	
			  cmd_dec (rv)	
			   cmd_ut (rv)	
		        cmd_utmid (rv)	
		      cmd_exptime (rv)
			cmd_epoch (rv)
		     cmd_date_obs (rv)
			  cmd_hjd (rv)	
		      cmd_mjd_obs (rv)	
			 cmd_vobs (rv)	
			 cmd_vrel (rv)	
		       cmd_vhelio (rv)	
		         cmd_vlsr (rv)	
		         cmd_vsun (rv)	

	The 'cmd_' prefix indicates that the routine is called from a colon 
command to either print the current value or set the new value for that
field.  Other routines should be self-explanatory

.endhelp _____________________________________________________________________

# Default values for the RVKEYWORDS pset
define	DEF_RA		"RA"			# Right Ascension 
define	DEF_DEC		"DEC"			# Declination 
define	DEF_EXPTIME	"EXPTIME"		# Exposure time 
define	DEF_UT		"UT"			# UT of observation 
define	DEF_UTMID	"UTMIDDLE"		# UT middle of observation 
define	DEF_EPOCH	"EPOCH"			# Epoch of observation 
define	DEF_DATE_OBS	"DATE-OBS"		# Date of observation 
define	DEF_HJD		"HJD"			# Heliocentric Julian date
define	DEF_MJD_OBS	"MJD-OBS"		# Modified Julian Date
define	DEF_VOBS	"VOBS"			# Observed radial velocity 
define	DEF_VHELIO	"VHELIO"		# Heliocentric radial velocity 
define	DEF_VLSR	"VLSR"			# LSR radial velocity 
define	DEF_VSUN	"VSUN"			# Solar motion data 


# KEYW_OPEN - Open the Keyword translation substructure.  This is used to
# reduce the size of the already over-burdened main RV struct.  Since this is
# in reality a single pointer and not a structure, the pointer allocated is
# of type TY_CHAR.  Access to individual elements is controlled by the defined
# macros in the file "rv$rvkeywords.h".

procedure keyw_open (rv)

pointer	rv					#I RV struct pointer

pointer	keyw

begin
	iferr (call calloc (keyw, LEN_KEYWSTRUCT, TY_CHAR))
	    call error (0, "Error allocating sub-structure RV_KEYW.")

	RV_KEYW(rv) = keyw

	# Initlialize the values
	call keyw_get_pars (rv)
end


# KEYW_CLOSE - Close the keyword structure

procedure keyw_close (rv)

pointer	rv					#I RV struct pointer

begin
	call mfree (RV_KEYW(rv), TY_CHAR)
end


# KEYW_GET_PARS - Get the Keyword tranlation parameters from the RVKEYWORDS
# pset.

procedure keyw_get_pars (rv)

pointer	rv					#I RV struct pointer

pointer	fp, clopset()
errchk  clopset

begin
	fp = clopset ("keywpars")

	call clgpset (fp, "ra", KW_RA(rv), LEN_KEYWEL)   
	call clgpset (fp, "dec", KW_DEC(rv), LEN_KEYWEL)
	call clgpset (fp, "ut", KW_UT(rv), LEN_KEYWEL)
	call clgpset (fp, "utmiddle", KW_UTMID(rv), LEN_KEYWEL)
	call clgpset (fp, "exptime", KW_EXPTIME(rv), LEN_KEYWEL)
	call clgpset (fp, "epoch", KW_EPOCH(rv), LEN_KEYWEL)
	call clgpset (fp, "date_obs", KW_DATE_OBS(rv), LEN_KEYWEL)
	call clgpset (fp, "hjd", KW_HJD(rv), LEN_KEYWEL)
	call clgpset (fp, "mjd_obs", KW_MJD_OBS(rv), LEN_KEYWEL)
	call clgpset (fp, "vobs", KW_VOBS(rv), LEN_KEYWEL)
	call clgpset (fp, "vrel", KW_VREL(rv), LEN_KEYWEL)
	call clgpset (fp, "vhelio", KW_VHELIO(rv), LEN_KEYWEL)
	call clgpset (fp, "vlsr", KW_VLSR(rv), LEN_KEYWEL)
	call clgpset (fp, "vsun", KW_VSUN(rv), LEN_KEYWEL)

	call clcpset (fp)
end


# KEYW_PARUPDATE - Update the pset with the current values of the struct.

procedure keyw_parupdate (rv)

pointer	rv					#I RV struct pointer

pointer	fp, clopset()

begin
	# Update filter params
	fp = clopset ("keywpars")

	call clppset (fp, "ra", KW_RA(rv))   
	call clppset (fp, "dec", KW_DEC(rv))
	call clppset (fp, "ut", KW_UT(rv))
	call clppset (fp, "utmiddle", KW_UTMID(rv))
	call clppset (fp, "exptime", KW_EXPTIME(rv))
	call clppset (fp, "epoch", KW_EPOCH(rv))
	call clppset (fp, "date_obs", KW_DATE_OBS(rv))
	call clppset (fp, "hjd", KW_HJD(rv))
	call clppset (fp, "mjd_obs", KW_MJD_OBS(rv))
	call clppset (fp, "vobs", KW_VOBS(rv))
	call clppset (fp, "vrel", KW_VREL(rv))
	call clppset (fp, "vhelio", KW_VHELIO(rv))
	call clppset (fp, "vlsr", KW_VLSR(rv))
	call clppset (fp, "vsun", KW_VSUN(rv))

	call clcpset (fp)
end


# KEYW_UNLEARN - Unlearn the pset and replace with the default values.

procedure keyw_unlearn (rv)

pointer	rv					#I RV struct pointer

begin
	call strcpy (DEF_RA, KW_RA(rv), LEN_KEYWEL)   
	call strcpy (DEF_DEC, KW_DEC(rv), LEN_KEYWEL)
	call strcpy (DEF_UT, KW_UT(rv), LEN_KEYWEL)
	call strcpy (DEF_UTMID, KW_UTMID(rv), LEN_KEYWEL)
	call strcpy (DEF_EXPTIME, KW_EXPTIME(rv), LEN_KEYWEL)
	call strcpy (DEF_EPOCH, KW_EPOCH(rv), LEN_KEYWEL)
	call strcpy (DEF_DATE_OBS, KW_DATE_OBS(rv), LEN_KEYWEL)
	call strcpy (DEF_HJD, KW_HJD(rv), LEN_KEYWEL)
	call strcpy (DEF_MJD_OBS, KW_MJD_OBS(rv), LEN_KEYWEL)
	call strcpy (DEF_VOBS, KW_VOBS(rv), LEN_KEYWEL)
	call strcpy (DEF_VHELIO, KW_VHELIO(rv), LEN_KEYWEL)
	call strcpy (DEF_VLSR, KW_VLSR(rv), LEN_KEYWEL)
	call strcpy (DEF_VSUN, KW_VSUN(rv), LEN_KEYWEL)
end


# KEYW_SHOW - Show the current keyword translation parameters.

procedure keyw_show (rv, fd)

pointer	rv			#I RV struct pointer
pointer	fd			#I output file descriptor

begin
	if (fd == NULL)
	    return

	call fprintf (fd, "%6tRVKeywords PSET Values\n")
	call fprintf (fd, "%6t----------------------\n\n")

	# Print the keyword translation info
	call fprintf (fd, "RA keyword%30t= '%s'\n")
	    call pargstr (KW_RA(rv))
	call fprintf (fd, "DEC keyword%30t= '%s'\n")
	    call pargstr (KW_DEC(rv))
	call fprintf (fd, "UT keyword%30t= '%s'\n")
	    call pargstr (KW_UT(rv))
	call fprintf (fd, "UTMIDDLE keyword%30t= '%s'\n")
	    call pargstr (KW_UTMID(rv))
	call fprintf (fd, "OTIME keyword%30t= '%s'\n")
	    call pargstr (KW_EXPTIME(rv))
	call fprintf (fd, "EPOCH keyword%30t= '%s'\n")
	    call pargstr (KW_EPOCH(rv))
	call fprintf (fd, "DATE-OBS keyword%30t= '%s'\n")
	    call pargstr (KW_DATE_OBS(rv))
	call fprintf (fd, "VHJD keyword%30t= '%s'\n")
	    call pargstr (KW_HJD(rv))
	call fprintf (fd, "MJD_OBS keyword%30t= '%s'\n")
	    call pargstr (KW_MJD_OBS(rv))
	call fprintf (fd, "VOBS keyword%30t= '%s'\n")
	    call pargstr (KW_VOBS(rv))
	call fprintf (fd, "VHELIO keyword%30t= '%s'\n")
	    call pargstr (KW_VHELIO(rv))
	call fprintf (fd, "VLSR keyword%30t= '%s'\n")
	    call pargstr (KW_VLSR(rv))
	call fprintf (fd, "VSUN keyword%30t= '%s'\n")
	    call pargstr (KW_VSUN(rv))

	call fprintf (fd, "\n\n")
end



# KEYW_COLON -- Process the RVKEYWORDS task colon commands.

procedure keyw_colon (rv, cmdstr)

pointer	rv				#I pointer to the RV structure
char	cmdstr[SZ_LINE]			#I command string

pointer	sp, cmd, buf
int	strdic()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)

	# Unpack the keyword from the string and look it up in the
	# dictionary.  Switch on command and call the appropriate routines.

	switch (strdic(Memc[cmd], Memc[cmd], SZ_FNAME, KEY_KEYWORDS)) {
	case KEY_RA:	
	    # Right ascension keyword
	    call cmd_ra (rv)

	case KEY_DEC:	
	    # Declination keyword
	    call cmd_dec (rv)

	case KEY_UT:	
	    # Universal time of observation keyword
	    call cmd_ut (rv)

	case KEY_UTMID:	
	    # Universal time of observation keyword
	    call cmd_utmid (rv)

	case KEY_EXPTIME:
	    # Frame exposure time keyword
	    call cmd_exptime (rv)

	case KEY_EPOCH:
	    # Epoch of observation keyword
	    call cmd_epoch (rv)

	case KEY_DATE_OBS:
	    # Date of observation keyword
	    call cmd_date_obs (rv)

	case KEY_HJD:	
	    # Heliocentric Julian Date Keyword
	    call cmd_hjd (rv)

	case KEY_MJD_OBS:	
	    # Observed RV keyword
	    call cmd_mjd_obs (rv)

	case KEY_VOBS:	
	    # Observed RV keyword
	    call cmd_vobs (rv)

	case KEY_VREL:	
	    # Relative RV keyword
	    call cmd_vrel (rv)

	case KEY_VHELIO:	
	    # Heliocentric RV keyword
	    call cmd_vhelio (rv)

	case KEY_VLSR:	
	    # LSR RV keyword
	    call cmd_vlsr (rv)

	case KEY_VSUN:	
	    # Solar motion epoch for LSR
	    call cmd_vsun (rv)

	default:
	}

	call sfree (sp)
end


# CMD_RA - Set/Show the RA image header keyword.

procedure cmd_ra (rv)	

pointer	rv					#I RV struct pointer

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, LEN_KEYWEL, TY_CHAR)

	call gargstr (Memc[buf], LEN_KEYWEL)
	if (Memc[buf] != EOS) {
	    call strcpy (Memc[buf+1], KW_RA(rv), LEN_KEYWEL)
	} else {
	    call printf ("RA keyword = '%s'")
	    	call pargstr (KW_RA(rv))
	}

	call sfree (sp)
end


# CMD_DEC - Set/Show the DEC image header keyword.

procedure cmd_dec (rv)	

pointer	rv					#I RV struct pointer

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, LEN_KEYWEL, TY_CHAR)

	call gargstr (Memc[buf], LEN_KEYWEL)
	if (Memc[buf] != EOS) {
	    call strcpy (Memc[buf+1], KW_DEC(rv), LEN_KEYWEL)
	} else {
	    call printf ("DEC keyword = '%s'")
	    	call pargstr (KW_DEC(rv))
	}

	call sfree (sp)
end


# CMD_UT - Set/Show the UT image header keyword.

procedure cmd_ut (rv)	

pointer	rv					#I RV struct pointer

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, LEN_KEYWEL, TY_CHAR)

	call gargstr (Memc[buf], LEN_KEYWEL)
	if (Memc[buf] != EOS) {
	    call strcpy (Memc[buf+1], KW_UT(rv), LEN_KEYWEL)
	} else {
	    call printf ("UT keyword = '%s'")
	    	call pargstr (KW_UT(rv))
	}

	call sfree (sp)
end


# CMD_UTMID - Set/Show the UTMID image header keyword.

procedure cmd_utmid (rv)	

pointer	rv					#I RV struct pointer

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, LEN_KEYWEL, TY_CHAR)

	call gargstr (Memc[buf], LEN_KEYWEL)
	if (Memc[buf] != EOS) {
	    call strcpy (Memc[buf+1], KW_UTMID(rv), LEN_KEYWEL)
	} else {
	    call printf ("UTMIDDLE keyword = '%s'")
	    	call pargstr (KW_UTMID(rv))
	}

	call sfree (sp)
end


# CMD_EXPTIME - Set/Show the EXPTIME image header keyword.

procedure cmd_exptime (rv)

pointer	rv					#I RV struct pointer

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, LEN_KEYWEL, TY_CHAR)

	call gargstr (Memc[buf], LEN_KEYWEL)
	if (Memc[buf] != EOS) {
	    call strcpy (Memc[buf+1], KW_EXPTIME(rv), LEN_KEYWEL)
	} else {
	    call printf ("EXPTIME keyword = '%s'")
	    	call pargstr (KW_EXPTIME(rv))
	}

	call sfree (sp)
end


# CMD_EPOCH - Set/Show the EPOCH image header keyword.

procedure cmd_epoch (rv)

pointer	rv					#I RV struct pointer

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, LEN_KEYWEL, TY_CHAR)

	call gargstr (Memc[buf], LEN_KEYWEL)
	if (Memc[buf] != EOS) {
	    call strcpy (Memc[buf+1], KW_EPOCH(rv), LEN_KEYWEL)
	} else {
	    call printf ("EPOCH keyword = '%s'")
	    	call pargstr (KW_EPOCH(rv))
	}

	call sfree (sp)
end


# CMD_DATE_OBS - Set/Show the DATE-OBS image header keyword.

procedure cmd_date_obs (rv)

pointer	rv					#I RV struct pointer

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, LEN_KEYWEL, TY_CHAR)

	call gargstr (Memc[buf], LEN_KEYWEL)
	if (Memc[buf] != EOS) {
	    call strcpy (Memc[buf+1], KW_DATE_OBS(rv), LEN_KEYWEL)
	} else {
	    call printf ("DATE-OBS keyword = '%s'")
	    	call pargstr (KW_DATE_OBS(rv))
	}

	call sfree (sp)
end


# CMD_HJD - Set/Show the HJD image header keyword.

procedure cmd_hjd (rv)	

pointer	rv					#I RV struct pointer

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, LEN_KEYWEL, TY_CHAR)

	call gargstr (Memc[buf], LEN_KEYWEL)
	if (Memc[buf] != EOS) {
	    call strcpy (Memc[buf+1], KW_HJD(rv), LEN_KEYWEL)
	} else {
	    call printf ("HJD keyword = '%s'")
	    	call pargstr (KW_HJD(rv))
	}

	call sfree (sp)
end


# CMD_MJD_OBS - Set/Show the MJD-OBS image header keyword.

procedure cmd_mjd_obs (rv)	

pointer	rv					#I RV struct pointer

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, LEN_KEYWEL, TY_CHAR)

	call gargstr (Memc[buf], LEN_KEYWEL)
	if (Memc[buf] != EOS) {
	    call strcpy (Memc[buf+1], KW_MJD_OBS(rv), LEN_KEYWEL)
	} else {
	    call printf ("MJD_OBS keyword = '%s'")
	    	call pargstr (KW_MJD_OBS(rv))
	}

	call sfree (sp)
end


# CMD_VOBS - Set/Show the VOBS image header keyword.

procedure cmd_vobs (rv)	

pointer	rv					#I RV struct pointer

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, LEN_KEYWEL, TY_CHAR)

	call gargstr (Memc[buf], LEN_KEYWEL)
	if (Memc[buf] != EOS) {
	    call strcpy (Memc[buf+1], KW_VOBS(rv), LEN_KEYWEL)
	} else {
	    call printf ("VOBS keyword = '%s'")
	    	call pargstr (KW_VOBS(rv))
	}

	call sfree (sp)
end


# CMD_VREL - Set/Show the VREL image header keyword.

procedure cmd_vrel (rv)

pointer rv                                      #I RV struct pointer

pointer sp, buf

begin
        call smark (sp)
        call salloc (buf, LEN_KEYWEL, TY_CHAR)

        call gargstr (Memc[buf], LEN_KEYWEL)
        if (Memc[buf] != EOS) {
            call strcpy (Memc[buf+1], KW_VREL(rv), LEN_KEYWEL)
        } else {
            call printf ("VREL keyword = '%s'")
                call pargstr (KW_VREL(rv))
        }

        call sfree (sp)
end


# CMD_VHELIO - Set/Show the VHELIO image header keyword.

procedure cmd_vhelio (rv)	

pointer	rv					#I RV struct pointer

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, LEN_KEYWEL, TY_CHAR)

	call gargstr (Memc[buf], LEN_KEYWEL)
	if (Memc[buf] != EOS) {
	    call strcpy (Memc[buf+1], KW_VHELIO(rv), LEN_KEYWEL)
	} else {
	    call printf ("VHELIO keyword = '%s'")
	    	call pargstr (KW_VHELIO(rv))
	}

	call sfree (sp)
end


# CMD_VLSR - Set/Show the VLSR image header keyword.

procedure cmd_vlsr (rv)	

pointer	rv					#I RV struct pointer

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, LEN_KEYWEL, TY_CHAR)

	call gargstr (Memc[buf], LEN_KEYWEL)
	if (Memc[buf] != EOS) {
	    call strcpy (Memc[buf+1], KW_VLSR(rv), LEN_KEYWEL)
	} else {
	    call printf ("VLSR keyword = '%s'")
	    	call pargstr (KW_VLSR(rv))
	}

	call sfree (sp)
end


# CMD_VSUN - Set/Show the VSUN image header keyword.

procedure cmd_vsun (rv)	

pointer	rv					#I RV struct pointer

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, LEN_KEYWEL, TY_CHAR)

	call gargstr (Memc[buf], LEN_KEYWEL)
	if (Memc[buf] != EOS) {
	    call strcpy (Memc[buf+1], KW_VSUN(rv), LEN_KEYWEL)
	} else {
	    call printf ("Epoch of solar motion keyword = '%s'")
	    	call pargstr (KW_VSUN(rv))
	}

	call sfree (sp)
end
