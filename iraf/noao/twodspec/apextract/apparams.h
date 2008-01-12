# PP_TABLE -- This table assigns pset pointers for each parameter.

define	PP_PP_LENTABLE	61

# APDEFAULT

define	PP_APIDTABLE		Memi[P2I($1)]
define	PP_LOWER		Memi[P2I($1+1)]
define	PP_UPPER		Memi[P2I($1+2)]
define	PP_B_FUNCTION		Memi[P2I($1+3)]
define	PP_B_ORDER		Memi[P2I($1+4)]
define	PP_B_SAMPLE		Memi[P2I($1+5)]
define	PP_B_NAVERAGE		Memi[P2I($1+6)]
define	PP_B_NITERATE		Memi[P2I($1+7)]
define	PP_B_LOW_REJECT		Memi[P2I($1+8)]
define	PP_B_HIGH_REJECT	Memi[P2I($1+9)]
define	PP_B_GROW		Memi[P2I($1+10)]

#APFIND

define	PP_NFIND		Memi[P2I($1+11)]
define	PP_MINSEP		Memi[P2I($1+12)]
define	PP_MAXSEP		Memi[P2I($1+13)]
define	PP_ORDER		Memi[P2I($1+14)]

# APRECENTER

define	PP_APERTURES		Memi[P2I($1+15)]
define	PP_NPEAKS		Memi[P2I($1+16)]
define	PP_SHIFT		Memi[P2I($1+17)]

# APRESIZE

define	PP_LLIMIT		Memi[P2I($1+18)]
define	PP_ULIMIT		Memi[P2I($1+19)]
define	PP_YLEVEL		Memi[P2I($1+20)]
define	PP_PEAK			Memi[P2I($1+21)]
define	PP_BKG			Memi[P2I($1+22)]

# APEDIT

define	PP_WIDTH		Memi[P2I($1+23)]
define	PP_RADIUS		Memi[P2I($1+24)]
define	PP_THRESHOLD		Memi[P2I($1+25)]
define	PP_E_OUTPUT		Memi[P2I($1+26)]
define	PP_E_SKY		Memi[P2I($1+27)]
define	PP_E_PROFILES		Memi[P2I($1+28)]

# APTRACE

define	PP_FITTRACE		Memi[P2I($1+29)]
define	PP_T_NSUM		Memi[P2I($1+30)]
define	PP_STEP			Memi[P2I($1+31)]
define	PP_T_FUNCTION		Memi[P2I($1+32)]
define	PP_T_ORDER		Memi[P2I($1+33)]
define	PP_T_SAMPLE		Memi[P2I($1+34)]
define	PP_T_NAVERAGE		Memi[P2I($1+35)]
define	PP_T_NITERATE		Memi[P2I($1+36)]
define	PP_T_LOW_REJECT		Memi[P2I($1+37)]
define	PP_T_HIGH_REJECT	Memi[P2I($1+38)]
define	PP_T_GROW		Memi[P2I($1+39)]

# APSUM or APSTRIP

define	PP_SKYEXTRACT		Memi[P2I($1+40)]
define	PP_BACKGROUND		Memi[P2I($1+41)]
define	PP_CLEAN		Memi[P2I($1+42)]
define	PP_WEIGHTS		Memi[P2I($1+43)]
define	PP_FIT(pp)		Memi[P2I($1+61)]
define	PP_NAVERAGE		Memi[P2I($1+44)]
define	PP_INTERPOLATOR		Memi[P2I($1+45)]
define	PP_NCLEAN		Memi[P2I($1+46)]
define	PP_LSIGMA		Memi[P2I($1+47)]
define	PP_USIGMA		Memi[P2I($1+48)]
define	PP_V0			Memi[P2I($1+49)]
define	PP_V1			Memi[P2I($1+50)]

# APNORMALIZE

define	PP_N_THRESHOLD		Memi[P2I($1+51)]
define	PP_N_FUNCTION		Memi[P2I($1+52)]
define	PP_N_ORDER		Memi[P2I($1+53)]
define	PP_N_SAMPLE		Memi[P2I($1+54)]
define	PP_N_NAVERAGE		Memi[P2I($1+55)]
define	PP_N_NITERATE		Memi[P2I($1+56)]
define	PP_N_LOW_REJECT		Memi[P2I($1+57)]
define	PP_N_HIGH_REJECT	Memi[P2I($1+58)]
define	PP_N_GROW		Memi[P2I($1+59)]

# APSCATTER

define	PP_BUFFER		Memi[P2I($1+60)]
