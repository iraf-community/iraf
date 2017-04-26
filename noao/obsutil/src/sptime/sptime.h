# Definitions for SPTIME.

# Spectral distribution types.
define	SPECTYPES	"|blackbody|flambda_power|fnu_power|"
define	SPEC_TAB	0
define	SPEC_BB		1
define	SPEC_FL		2
define	SPEC_FN		3

# Flux units.
define	FUNITS		"|AB|F_lambda|F_nu|U|B|V|R|I|J|H|Ks|K|L|L'|M|"
define	AB		1
define	FLAMBDA		2
define	FNU		3
define	UMAG		4
define	BMAG		5
define	VMAG		6
define	RMAG		7
define	IMAG		8
define	JMAG		9
define	HMAG		10
define	KSMAG		11
define	KMAG		12
define	LMAG		13
define	LPMAG		14
define	MMAG		15

# Sky subtraction options.
define	SKYSUB		"|none|longslit|multiap|shuffle|"
define	SKY_NONE	1
define	SKY_LONGSLIT	2
define	SKY_MULTIAP	3
define	SKY_SHUFFLE	4

# Aperture types.
define	APTYPES		"|circular|rectangular|"
define	CIRCULAR	1
define	RECTANGULAR	2

# Output types.
define	OUTTYPES	"|counts|snr|object|rate|atmosphere|telescope|adc|\
			 |aperture|fiber|filter|filter2|collimator|disperser|\
			 |xdisperser|corrector|camera|detector|spectrograph|\
			 |emissivity|thruput|sensfunc|correction|"
define	OUT_COUNTS	1
define	OUT_SNR		2
define	OUT_OBJ		3
define	OUT_RATE	4
define	OUT_ATM		5
define	OUT_TEL		6
define	OUT_ADC		7
define	OUT_AP		9
define	OUT_FIB		10
define	OUT_FILT	11
define	OUT_FILT2	12
define	OUT_COL		13
define	OUT_DISP	14
define	OUT_XDISP	16
define	OUT_COR		17
define	OUT_CAM		18
define	OUT_DET		19
define	OUT_SPEC	20
define	OUT_EMIS	22
define	OUT_THRU	23
define	OUT_SENS	24
define	OUT_CORRECT	25

# Grating types.
define	DISPTYPES	"|grating|grism|generic"
define	GRATING		1
define	GRISM		2
define	GENERIC		3

# Macros.
define	MINEXP		0.01		# Minimum exposure time.
define	MAXNEXP		10000		# Maximum number of exposures.
define	MAXITER		50
define	H		6.626E-27
define	C		2.99792456E18

# Data structure.
define	ST_SZSTRING	99			# Length of strings
define	ST_LEN		154			# Structure length

define	ST_TAB		Memi[$1]		# Tables pointer
define	ST_SEARCH	Memi[$1+1]		# Search list
define	ST_SPEC		Memi[$1+2]		# Spectrum type
define	ST_PARAM	Memr[P2R($1+3)]		# Spectrum parameter
define	ST_THERMAL	Memr[P2R($1+4)]		# Thermal background temperature
define	ST_AV		Memr[P2R($1+5)]		# A(V)
define	ST_RV		Memr[P2R($1+6)]		# A(V)/E(B-V)
define	ST_TIME		Memr[P2R($1+7)]		# Exposure time
define	ST_NEXP		Memi[$1+8]		# Number of exposures
define	ST_MINEXP	Memr[P2R($1+9)]		# Minimum time per integration
define	ST_MAXEXP	Memr[P2R($1+10)]	# Maximum time per integration
define	ST_AIRMASS	Memr[P2R($1+11)]	# Airmass
define	ST_SEEING	Memr[P2R($1+12)]	# Seeing (FWHM in arcsec)
define	ST_PHASE	Memr[P2R($1+13)]	# Moon phase
define	ST_REFW		Memr[P2R($1+14)]	# Reference wavelength
define	ST_REFF		Memr[P2R($1+15)]	# Reference flux
define	ST_REFFL	Memr[P2R($1+16)]	# Reference flambda
define	ST_CW		Memr[P2R($1+17)]	# Central wavelength
define	ST_ORDER	Memi[$1+17+$2]		# Grating orders (2)
define	ST_APSIZE	Memr[P2R($1+19+$2)]	# Aperture sizes (2) 
define	ST_BIN		Memi[$1+21+$2]		# Binning (2)
define	ST_GAIN		Memr[P2R($1+24)]	# Detector gain
define	ST_RDNOISE	Memr[P2R($1+25)]	# Detector read noise
define	ST_DARK		Memr[P2R($1+26)]	# Detector dark counts
define	ST_INOUTA	Memr[P2R($1+26+$2)]	# Incident-diffracted angle(deg)
define	ST_SKYSUB	Memi[$1+29]		# Sky subtraction type
define	ST_NSKYAPS	Memi[$1+30]		# Number of sky apertures

define	ST_DISPTYPE	Memi[$1+30+$2]		# Disperser type
define	ST_GR		Memi[$1+32+$2]		# Grating pointer (2)
define	ST_DISP		Memr[P2R($1+34+$2)]	# Dispersion (2)
define	ST_SCALE	Memr[P2R($1+36+$2)]	# Scale at detector (2)
define	ST_RES		Memr[P2R($1+38+$2)]	# Resolution at detector (2)
define	ST_AREA		Memr[P2R($1+41)]	# Effective collecting area
define	ST_TELSCALE	Memr[P2R($1+42)]	# Telescope scale at aperture
define	ST_NOBJPIX	Memi[$1+43]		# Number of object pixels
define	ST_NSKYPIX	Memi[$1+44]		# Number of sky pixels
define	ST_APLIM	Memi[$1+45]		# Aperture limited?
define	ST_APTYPE	Memi[$1+46]		# Aperture type
define	ST_PIXSIZE	Memr[P2R($1+47)]	# Pixel weighted disp. size
define	ST_FILTBLK	Memi[$1+48]		# Block filter flag
define	ST_DUNANG	Memi[$1+49]		# Angstrom units pointer
define	ST_DUN		Memi[$1+50]		# User units pointer
define	ST_SUBPIXELS	Memi[$1+51]		# Number of subpixels
define	ST_COLFL	Memr[P2R($1+52)]	# Collimator focal length
define	ST_CAMFL	Memr[P2R($1+53)]	# Collimator focal length
define	ST_DUNITS	Memc[P2C($1+54)]	# Dispersion units
define	ST_FUNITS	Memc[P2C($1+104)]	# Flux units
