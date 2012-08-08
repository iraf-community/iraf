# PP_TABLE -- This table assigns pset pointers for each parameter.

define	PP_PP_LENTABLE	61

# APDEFAULT

define	PP_APIDTABLE		Memi[$1]
define	PP_LOWER		Memi[$1+1]
define	PP_UPPER		Memi[$1+2]
define	PP_B_FUNCTION		Memi[$1+3]
define	PP_B_ORDER		Memi[$1+4]
define	PP_B_SAMPLE		Memi[$1+5]
define	PP_B_NAVERAGE		Memi[$1+6]
define	PP_B_NITERATE		Memi[$1+7]
define	PP_B_LOW_REJECT		Memi[$1+8]
define	PP_B_HIGH_REJECT	Memi[$1+9]
define	PP_B_GROW		Memi[$1+10]

#APFIND

define	PP_NFIND		Memi[$1+11]
define	PP_MINSEP		Memi[$1+12]
define	PP_MAXSEP		Memi[$1+13]
define	PP_ORDER		Memi[$1+14]

# APRECENTER

define	PP_APERTURES		Memi[$1+15]
define	PP_NPEAKS		Memi[$1+16]
define	PP_SHIFT		Memi[$1+17]

# APRESIZE

define	PP_LLIMIT		Memi[$1+18]
define	PP_ULIMIT		Memi[$1+19]
define	PP_YLEVEL		Memi[$1+20]
define	PP_PEAK			Memi[$1+21]
define	PP_BKG			Memi[$1+22]

# APEDIT

define	PP_WIDTH		Memi[$1+23]
define	PP_RADIUS		Memi[$1+24]
define	PP_THRESHOLD		Memi[$1+25]
define	PP_E_OUTPUT		Memi[$1+26]
define	PP_E_SKY		Memi[$1+27]
define	PP_E_PROFILES		Memi[$1+28]

# APTRACE

define	PP_FITTRACE		Memi[$1+29]
define	PP_T_NSUM		Memi[$1+30]
define	PP_STEP			Memi[$1+31]
define	PP_T_FUNCTION		Memi[$1+32]
define	PP_T_ORDER		Memi[$1+33]
define	PP_T_SAMPLE		Memi[$1+34]
define	PP_T_NAVERAGE		Memi[$1+35]
define	PP_T_NITERATE		Memi[$1+36]
define	PP_T_LOW_REJECT		Memi[$1+37]
define	PP_T_HIGH_REJECT	Memi[$1+38]
define	PP_T_GROW		Memi[$1+39]

# APSUM or APSTRIP

define	PP_SKYEXTRACT		Memi[$1+40]
define	PP_BACKGROUND		Memi[$1+41]
define	PP_CLEAN		Memi[$1+42]
define	PP_WEIGHTS		Memi[$1+43]
define	PP_FIT(pp)		Memi[$1+61]
define	PP_NAVERAGE		Memi[$1+44]
define	PP_INTERPOLATOR		Memi[$1+45]
define	PP_NCLEAN		Memi[$1+46]
define	PP_LSIGMA		Memi[$1+47]
define	PP_USIGMA		Memi[$1+48]
define	PP_V0			Memi[$1+49]
define	PP_V1			Memi[$1+50]

# APNORMALIZE

define	PP_N_THRESHOLD		Memi[$1+51]
define	PP_N_FUNCTION		Memi[$1+52]
define	PP_N_ORDER		Memi[$1+53]
define	PP_N_SAMPLE		Memi[$1+54]
define	PP_N_NAVERAGE		Memi[$1+55]
define	PP_N_NITERATE		Memi[$1+56]
define	PP_N_LOW_REJECT		Memi[$1+57]
define	PP_N_HIGH_REJECT	Memi[$1+58]
define	PP_N_GROW		Memi[$1+59]

# APSCATTER

define	PP_BUFFER		Memi[$1+60]
