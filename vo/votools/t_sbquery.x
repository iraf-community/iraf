#

#  SBQUERY -- Call the IMCCE SkyBoT (Sky Bodies Tracker) service to locate
#  minor planets within a radius at a given position and epoch.
#

include	<time.h>


#  Possible fields.
define  SB_FIELDS  "|num|name|class|ra|dec|vmag|poserr|cdist|dra|ddec|\
		    |dgeo|dhelio|px|py|pz|vx|vy|vz|jd0|"

define  SB_NFIELDS     20		# accounts for the newline

define	SB_KNUM 	"NUM"		# keywords
define	SB_KNAME 	"NAME"
define	SB_KCLASS 	"CLASS"
define	SB_KRA 		"RA"
define	SB_KDEC 	"DEC"
define	SB_KVMAG 	"VMAG"
define	SB_KPOSERR 	"POSERR"
define	SB_KCDIST 	"CDIST"
define	SB_KDRA 	"DRA"
define	SB_KDDEC 	"DDEC"
define	SB_KDGEO 	"DGEO"
define	SB_KDHELIO 	"DHELIO"
define	SB_KPX 		"PX"
define	SB_KPY 		"PY"
define	SB_KPZ 		"PZ"
define	SB_KVX 		"VX"
define	SB_KVY 		"VY"
define	SB_KVZ 		"VZ"
define	SB_KJD0 	"JD0"

define	SB_LNUM 	"number"	# attribute keywords
define	SB_LNAME 	"name"
define	SB_LCLASS 	"class"
define	SB_LRA 		"ra"
define	SB_LDEC 	"dec"
define	SB_LVMAG 	"vmag"
define	SB_LPOSERR 	"poserr"
define	SB_LCDIST 	"cdist"
define	SB_LDRA 	"dra"
define	SB_LDDEC 	"ddec"
define	SB_LDGEO 	"dgeo"
define	SB_LDHELIO 	"dhelio"
define	SB_LPX 		"px"
define	SB_LPY 		"py"
define	SB_LPZ 		"pz"
define	SB_LVX 		"vx"
define	SB_LVY 		"vy"
define	SB_LVZ 		"vz"
define	SB_LJD0 	"jd0"

define	SB_FNUM 	1		# field numbers
define	SB_FNAME 	2
define	SB_FCLASS 	3
define	SB_FRA 		4
define	SB_FDEC 	5
define	SB_FVMAG 	6
define	SB_FPOSERR 	7
define	SB_FCDIST 	8
define	SB_FDRA 	9
define	SB_FDDEC 	10
# NL
define	SB_FDGEO 	12
define	SB_FDHELIO 	13
define	SB_FPX 		14
define	SB_FPY 		15
define	SB_FPZ 		16
define	SB_FVX 		17
define	SB_FVY 		18
define	SB_FVZ 		19
define	SB_FJD0 	20

define  SB_FREAL       	"%-10.4f"
define  SB_FWSTRING     "%-12s"
define  SB_FSTRING     	"%-10s"
define  SB_FINTEGER    	"%10d"
define  SB_FSEX    	"%-012h"



procedure t_sbquery ()

pointer	sp, fields, fieldstr, epoch
double	ra, dec, sr, ut, jd_epoch
int	sb, nobjs, nfields, i, gmtco
int	tm[LEN_TMSTRUCT]
bool	fmt
long	otime, ntime

real	clgetr()
bool	clgetb(), streq()
int	vx_initVOClient(), vx_skybotnobjs(), vx_skybot()
int	sb_fields()
long	clktime()
double	ast_date_to_julday()

begin
	call smark (sp)
	call salloc (epoch, SZ_FNAME, TY_CHAR)
	call salloc (fields, SZ_LINE, TY_INT)
	call salloc (fieldstr, SZ_LINE, TY_CHAR)

	call aclrs (Memc[epoch], SZ_FNAME)
	call aclri (Memc[fields], SZ_LINE)
	call aclrs (Memc[fieldstr], SZ_LINE)


	ra = clgetr ("ra") 		# Get the parameters
	dec = clgetr ("dec")
	sr = clgetr ("sr")
	call clgstr ("epoch", Memc[epoch], SZ_LINE)
	if (streq (Memc[epoch], "now")) {
	    # Handle the special case of 'now', i.e. find out what time it is.
	    call zgmtco (gmtco)
	    call brktime (clktime(0)+gmtco, tm)
	    ut =  real (TM_HOUR(tm)) + 
		 (real(TM_MIN(tm))/60.) + 
		 (real(TM_SEC(tm))/3600.)
	    jd_epoch = ast_date_to_julday (TM_YEAR(tm), TM_MONTH(tm),
		TM_MDAY(tm), ut)

	} else {
	    # Convert the user-supplied epoch.
	    call sscan (Memc[epoch])
		call gargd (jd_epoch)
	}
	call clgstr ("fields", Memc[fieldstr], SZ_LINE)
	fmt = clgetb ("fmt")

	if (vx_initVOClient("") == ERR) {		# Initialize
	    call clputi ("status", ERR)
	    return
	}

	if (fmt) {
	    call printf ("#\n# Search Terms: ")
	    call printf ("Position=(%H,%h) sr=%.1f epoch=%.4f\n")
	        call pargd (ra)
	        call pargd (dec)
	        call pargd (sr)
	        call pargd (jd_epoch)
	    call printf ("#\n")
	    call flush (STDOUT)
	}

	# Call the service
	otime = clktime (0)
	sb  = vx_skybot (ra, dec, sr, sr, jd_epoch)
	ntime = clktime (otime)

	if (sb != NULL) {
	    nobjs = vx_skybotnobjs (sb)
	    if (fmt) {
	        call printf ("# Found %d objects in %d seconds\n#\n")
	            call pargi (nobjs)
	            call pargl (ntime)
	    }
	} else {
	    call eprintf ("Error in calling SkyBoT service\n")
	    call sfree (sp)
	    call clputi ("status", ERR)
	    call vx_closeVOClient (0)
	    return
	}

        # Get the selected fields.
        nfields = sb_fields (Memc[fieldstr], Memi[fields], SB_NFIELDS)
        if (nfields <= 0) {
	    call vx_closeVOClient (0)
            call sfree (sp)
            return
        }

	# Finally, print the table of results.
	if (fmt)
	    call sb_pheader (Memi[fields], nfields)
	for (i=0; i < nobjs; i=i+1)
	    call sb_print (sb, i, Memi[fields], nfields)

	call vx_closeVOClient (0)

	call clputi ("status", OK)
end


# SB_FIELDS -- Procedure to decode the fields string into a list of the
# fields to be printed.

int procedure sb_fields (fieldstr, fields, max_nfields)

char    fieldstr[ARB]           #I string containing the list of fields
int     fields[ARB]             #O fields array
int     max_nfields             #I maximum number of fields

int     nfields, flist, field
pointer sp, fname
int     fntopenb(), fntgfnb(), strdic()

begin
        nfields = 0

        call smark (sp)
        call salloc (fname, SZ_FNAME, TY_CHAR)

        flist = fntopenb (fieldstr, NO)
        while (fntgfnb (flist, Memc[fname], SZ_FNAME) != EOF &&
            (nfields < max_nfields)) {
            field = strdic (Memc[fname], Memc[fname], SZ_FNAME, SB_FIELDS)
            if (field == 0)
                next
            nfields = nfields + 1
            fields[nfields] = field
        }
        call fntclsb (flist)

        call sfree (sp)

        return (nfields)
end


# SB_PHEADER -- Print the banner fields.

procedure sb_pheader (fields, nfields)

int     fields[ARB]             # fields to be printed
int     nfields                 # number of fields

int     i

begin
        call printf ("#")
        do i = 1, nfields {

            switch (fields[i]) {
            case SB_FNUM: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KNUM)
	    case SB_FNAME: 	
            	call printf (SB_FWSTRING)
		    call pargstr (SB_KNAME)
	    case SB_FCLASS: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KCLASS)
	    case SB_FRA: 	
            	call printf (SB_FWSTRING)
		    call pargstr (SB_KRA)
	    case SB_FDEC: 	
            	call printf (SB_FWSTRING)
		    call pargstr (SB_KDEC)
	    case SB_FVMAG: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KVMAG)
	    case SB_FPOSERR: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KPOSERR)
	    case SB_FCDIST: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KCDIST)
	    case SB_FDRA: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KDRA)
	    case SB_FDDEC: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KDDEC)
	    case SB_FDGEO: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KDGEO)
	    case SB_FDHELIO: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KDHELIO)
	    case SB_FPX: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KPX)
	    case SB_FPY: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KPY)
	    case SB_FPZ: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KPZ)
	    case SB_FVX: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KVX)
	    case SB_FVY: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KVY)
	    case SB_FVZ: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KVZ)
	    case SB_FJD0: 	
            	call printf (SB_FSTRING)
		    call pargstr (SB_KJD0)
            }
        }
        call printf ("\n")
        call flush (STDOUT)
end


# SB_PRINT -- Print the banner fields.

procedure sb_print (sb, objnum, fields, nfields)

int	sb			# skybot pointer
int	objnum			# skybot result number
int     fields[ARB]             # fields to be printed
int     nfields                 # number of fields

int     i, len
char	buf[SZ_LINE]
double	dval

int	vx_skybotstr()
double	vx_skybotdbl()

begin
        do i = 1, nfields {
	    call aclrs (buf, SZ_LINE)

            switch (fields[i]) {
            case SB_FNUM: 	
	        len = vx_skybotstr (sb, SB_LNUM, objnum, buf, SZ_FNAME)
                call printf (SB_FSTRING)
		    call pargstr (buf)
	    case SB_FNAME: 	
	        len = vx_skybotstr (sb, SB_LNAME, objnum, buf, SZ_FNAME)
                call printf (SB_FWSTRING)
		    call pargstr (buf)
	    case SB_FCLASS: 	
	        len = vx_skybotstr (sb, SB_LCLASS, objnum, buf, SZ_FNAME)
                call printf (SB_FSTRING)
		    call pargstr (buf)

	    case SB_FRA: 	
	        dval = vx_skybotdbl (sb, SB_LRA, objnum)
                call printf (SB_FSEX)
		    call pargd (dval)
	    case SB_FDEC: 	
	        dval = vx_skybotdbl (sb, SB_LDEC, objnum)
                call printf (SB_FSEX)
		    call pargd (dval)
	    case SB_FVMAG: 	
	        dval = vx_skybotdbl (sb, SB_LVMAG, objnum)
                call printf (SB_FREAL)
		    call pargd (dval)
	    case SB_FPOSERR: 	
	        dval = vx_skybotdbl (sb, SB_LPOSERR, objnum)
                call printf (SB_FREAL)
		    call pargd (dval)
	    case SB_FCDIST: 	
	        dval = vx_skybotdbl (sb, SB_LCDIST, objnum)
                call printf (SB_FREAL)
		    call pargd (dval)
	    case SB_FDRA: 	
	        dval = vx_skybotdbl (sb, SB_LDRA, objnum)
                call printf (SB_FREAL)
		    call pargd (dval)
	    case SB_FDDEC: 	
	        dval = vx_skybotdbl (sb, SB_LDDEC, objnum)
                call printf (SB_FREAL)
		    call pargd (dval)
	    case SB_FDGEO: 	
	        dval = vx_skybotdbl (sb, SB_LDGEO, objnum)
                call printf (SB_FREAL)
		    call pargd (dval)
	    case SB_FDHELIO: 	
	        dval = vx_skybotdbl (sb, SB_LDHELIO, objnum)
                call printf (SB_FREAL)
		    call pargd (dval)
	    case SB_FPX: 	
	        dval = vx_skybotdbl (sb, SB_LPX, objnum)
                call printf (SB_FREAL)
		    call pargd (dval)
	    case SB_FPY: 	
	        dval = vx_skybotdbl (sb, SB_LPY, objnum)
                call printf (SB_FREAL)
		    call pargd (dval)
	    case SB_FPZ: 	
	        dval = vx_skybotdbl (sb, SB_LPZ, objnum)
                call printf (SB_FREAL)
		    call pargd (dval)
	    case SB_FVX: 	
	        dval = vx_skybotdbl (sb, SB_LVX, objnum)
                call printf (SB_FREAL)
		    call pargd (dval)
	    case SB_FVY: 	
	        dval = vx_skybotdbl (sb, SB_LVY, objnum)
                call printf (SB_FREAL)
		    call pargd (dval)
	    case SB_FVZ: 	
	        dval = vx_skybotdbl (sb, SB_LVZ, objnum)
                call printf (SB_FREAL)
		    call pargd (dval)
	    case SB_FJD0: 	
	        dval = vx_skybotdbl (sb, SB_LJD0, objnum)
                call printf (SB_FREAL)
		    call pargd (dval)
            }
        }
        call printf ("\n")
        call flush (STDOUT)
end
