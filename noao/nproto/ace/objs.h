# This file defines the object parameters.

# The following are the parameter ids which are the offsets into the object
# data structure.  Note that the first group of parameters are those
# determined during detection for potential objects.  The second group
# are parameters added after an object has been accepted.

define	ID_ROW		 0 # i "" ""		"Catalog row"
define  ID_NUM           1 # i "" ""		"Object number"
define  ID_PNUM          2 # i "" ""		"Parent number"
define	ID_XAP		 3 # r pixels %.2f	"X aperture coordinate"
define	ID_YAP		 4 # r pixels %.2f	"Y aperture coordinate"
define	ID_FLUX		 5 # r counts ""	"Isophotal flux (I - sky)"
define	ID_NPIX		 6 # i pixels ""	"Number of pixels"
define	ID_NDETECT	 7 # i pixels ""	"Number of detected pixels"
define	ID_ISIGAVG	 8 # r sigma ""		"Average (I - sky) / sig"
define	ID_ISIGMAX	 9 # r sigma ""		"Maximum (I - sky) / sig"
define	ID_ISIGAVG2	10 # r sigma ""		"*Ref average (I - sky) / sig"
define	ID_FLAGS	11 # 8 "" ""		"Flags"

define	ID_SKY		12 # r counts ""	"Mean sky"
define	ID_SIG		13 # r counts ""	"Sky sigma"
define	ID_PEAK		14 # r counts ""	"Peak pixel value above sky"
define	ID_APFLUX	15 # r counts ""	"Aperture fluxes"
define	ID_FRACFLUX	16 # r counts ""	"Apportioned flux"
define	ID_FRAC		17 # r "" ""		"Apportioned fraction"
define	ID_XMIN		18 # i pixels ""	"Minimum X"
define	ID_XMAX		19 # i pixels ""	"Maxium X"
define	ID_YMIN		20 # i pixels ""	"Minimum Y"
define	ID_YMAX		21 # i pixels ""	"Maxium Y"
define	ID_WX		22 # d pixels %.2f	"X world coordinate"
define	ID_WY		24 # d pixels %.2f	"Y world coordinate"
define	ID_X1		26 # r pixels %.2f	"X centroid"
define	ID_Y1		27 # r pixels %.2f	"Y centroid"
define	ID_X2		28 # r pixels ""	"X 2nd moment"
define	ID_Y2		29 # r pixels ""	"Y 2nd moment"
define	ID_XY		30 # r pixels ""	"X 2nd cross moment"

define	ID_FLUXVAR	31 # r counts ""	"*Variance in the flux"
define	ID_XVAR		32 # r pixels ""	"*Variance in X centroid"
define	ID_YVAR		33 # r pixels ""	"*Variance in Y centroid"
define	ID_XYCOV	34 # r pixels ""	"*Covariance of X and Y"

# The following are derived quantities which have ids above 1000.

define	ID_A		1001 # r pixels ""	"Semimajor axis"
define	ID_B		1002 # r pixels ""	"Semiminor axis"
define	ID_THETA	1003 # r degrees ""	"Position angle"
define	ID_ELONG	1004 # r "" ""		"Elongation = A/B"
define	ID_ELLIP	1005 # r "" ""		"Ellipticity = 1 - B/A"
define	ID_R		1006 # r pixels "" 	"Second moment radius"
define	ID_CXX		1007 # r pixels ""	"Second moment ellipse"
define	ID_CYY		1008 # r pixels ""	"Second moment ellipse"
define	ID_CXY		1009 # r pixels ""	"Second moment ellipse"

define	ID_FLUXERR	1011 # r counts ""	"Error in flux"
define	ID_XERR		1012 # r pixels ""	"Error in X centroid"
define	ID_YERR		1013 # r pixels ""	"Error in Y centroid"
define	ID_AERR		1014 # r "" ""		"Error in A"
define	ID_BERR		1015 # r "" ""		"Error in B"
define	ID_THETAERR	1016 # r degrees ""	"Error in THETA"
define	ID_CXXERR	1017 # r pixels ""	"Error in CXX"
define	ID_CYYERR	1018 # r pixels ""	"Error in CYY"
define	ID_CXYERR	1019 # r pixels ""	"Error in CXY"


# Reference to elements of the object data structure may be made with
# the generic OBJ[IRDC] macros or with the individual structure macros.

define	OBJI		Memi[$1+$2]		# Reference integer parameter
define	OBJR		Memr[P2R($1+$2)]	# Reference real parameter
define	OBJD		Memd[P2D($1+$2)]	# Reference double parameter
define	OBJC		Memc[P2C($1+$2)]	# Reference char parameter

define	OBJ_DETLEN	12			# Length for candidate objects
define	OBJ_LEN		35			# Length for accepted objects

# Detection pass parameters.
define	OBJ_ROW		OBJI($1,ID_ROW)		# Catalog row
define	OBJ_NUM		OBJI($1,ID_NUM)		# Object number
define	OBJ_PNUM	OBJI($1,ID_PNUM)	# Parent object number
define	OBJ_XAP		OBJR($1,ID_XAP)		# X aperture coordinate
define	OBJ_YAP		OBJR($1,ID_YAP)		# Y aperture coordinate
define	OBJ_NPIX	OBJI($1,ID_NPIX)	# Number of pixels
define	OBJ_NDETECT	OBJI($1,ID_NDETECT)	# Number of detected pixels
define	OBJ_ISIGAVG	OBJR($1,ID_ISIGAVG)	# Average (I - sky) / sig
define	OBJ_ISIGMAX	OBJR($1,ID_ISIGMAX)	# Maximum (I - sky) / sig
define	OBJ_ISIGAVG2	OBJR($1,ID_ISIGAVG2)	# Ref average (I - sky) / sig
define	OBJ_FLAGS	OBJI($1,ID_FLAGS)	# Flags

define	OBJ_SKY		OBJR($1,ID_SKY)		# Mean sky
define	OBJ_SIG		OBJR($1,ID_SIG)		# Sky sigma
define	OBJ_PEAK	OBJR($1,ID_PEAK)	# Peak pixel value above sky
define	OBJ_FLUX	OBJR($1,ID_FLUX)	# Isophotal flux (I - sky)
define	OBJ_APFLUX	OBJI($1,ID_APFLUX)	# Array of aperture fluxes (ptr)
define	OBJ_FRACFLUX	OBJR($1,ID_FRACFLUX)	# Apportioned flux
define	OBJ_FRAC	OBJR($1,ID_FRAC)	# Approtioned fraction
define	OBJ_XMIN	OBJI($1,ID_XMIN)	# Minimum X
define	OBJ_XMAX	OBJI($1,ID_XMAX)	# Maxium X
define	OBJ_YMIN	OBJI($1,ID_YMIN)	# Minimum Y
define	OBJ_YMAX	OBJI($1,ID_YMAX)	# Maxium Y
define	OBJ_WX		OBJD($1,ID_WX)		# X world coordinate
define	OBJ_WY		OBJD($1,ID_WY)		# Y world coordinate
define	OBJ_X1		OBJR($1,ID_X1)		# X centroid
define	OBJ_Y1		OBJR($1,ID_Y1)		# Y centroid
define	OBJ_X2		OBJR($1,ID_X2)		# X centroid
define	OBJ_Y2		OBJR($1,ID_Y2)		# Y centroid
define	OBJ_XY		OBJR($1,ID_XY)		# X centroid

define	OBJ_FLUXVAR	OBJR($1,ID_FLUXVAR)	# Variance in flux
define	OBJ_XVAR	OBJR($1,ID_XVAR)	# Variance in X centroid
define	OBJ_YVAR	OBJR($1,ID_YVAR)	# Variance in Y centroid
define	OBJ_XYCOV	OBJR($1,ID_XYCOV)	# Covariance of X and Y centroid




# Object flags.
define	OBJ_EVAL	001B		# Object was evaluated
define	OBJ_GROW	002B		# Object was grown
define	OBJ_SPLIT	004B		# Object was split
define	OBJ_SINGLE	010B		# Object was not split
define	OBJ_DARK	020B		# Object was below sky

define	FLAGSET		(andi(OBJ_FLAGS($1),$2)!=0)
define	FLAGNOTSET	(andi(OBJ_FLAGS($1),$2)==0)
define	SETFLAG		OBJ_FLAGS($1)=ori(OBJ_FLAGS($1),$2)
define	UNSETFLAG	OBJ_FLAGS($1)=andi(OBJ_FLAGS($1),noti($2))

define	DARK		(andi(OBJ_FLAGS($1),OBJ_DARK)!=0)
define	EVAL		(andi(OBJ_FLAGS($1),OBJ_EVAL)!=0)
define	SPLIT		(andi(OBJ_FLAGS($1),OBJ_SPLIT)!=0)
define	NOTSPLIT	(andi(OBJ_FLAGS($1),OBJ_SPLIT)==0)
define	SINGLE		(andi(OBJ_FLAGS($1),OBJ_SINGLE)!=0)
define	NOTSINGLE	(andi(OBJ_FLAGS($1),OBJ_SINGLE)==0)
define	GROWN		(andi(OBJ_FLAGS($1),OBJ_GROW)!=0)
define	NOTGROWN	(andi(OBJ_FLAGS($1),OBJ_GROW)==0)

define	SZ_FLAGS	5		# Size of flag string
