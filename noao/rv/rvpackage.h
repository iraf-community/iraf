# Header file for the Radial Velocity/Cross Correlation Package

define	RV_VERSION	"RV Level-0 Release V1.3:  1/6/92"

define	LEN_RVSTRUCT	    200		# Length of main data structure
define	MAXTEMPS	    702		# Max templates allowed
define	DBL_LEN	     	     50		# Deblended struct len
define	SZ_APNUM	     80		# Size of the APNUM field
define	SZ_APLIST	    256		# Size of APNUM keyword range

# Image data pointers (current working set)
define	RV_OPIXX	Memi[$1]	# Object x-axis (ptr)
define	RV_OPIXY	Memi[$1+1]	# Object y-axis (ptr)
define	RV_RPIXX	Memi[$1+2]	# Template x-axis (ptr)
define	RV_RPIXY        Memi[$1+3]	# Template y-axis (ptr)
define	RV_WKPIXX	Memi[$1+4]	# Working array x-axis (ptr)
define	RV_WKPIXY	Memi[$1+5]	# Working array y-axis (ptr)

# Task Parameters
define	RV_APODIZE	Memr[P2R($1+10)] # Endmask percentage
define	RV_AUTOWRITE	Memi[$1+11]	 # Auto record results?
define	RV_AUTODRAW	Memi[$1+12]	 # Auto redraw fit results?
define	RV_CONTINUUM	Memi[$1+13]	 # Continuum subtract spectra?
define	RV_FILTER	Memi[$1+14]	 # Fourier filter data?
define	RV_INTERACTIVE	Memi[$1+15]	 # Interactive flag?
define  RV_PIXCORR	Memi[$1+16]	 # Do a pixel-only correlation?
define	RV_INTERP	Memi[$1+17]	 # Rebinning interpolator

# Peak Fitting Misc.
define	RV_BACKGROUND	Memr[P2R($1+20)] # baseline for FWHM computation
define	RV_FITDONE	Memi[$1+21]	 # Has a fit been done?
define	RV_FITFUNC	Memi[$1+22]	 # Correlation fitting func
define	RV_FITHGHT	Memr[P2R($1+23)] # Height of peak to begin fit
define	RV_FITWIDTH	Memr[P2R($1+24)] # Width of fitting region
define	RV_ISHIFT	Memi[$1+25]	 # Initial shift of ccf
define	RV_ISTART	Memi[$1+26]	 # Start element of ccf fit
define	RV_IEND		Memi[$1+27]	 # Ending element of ccf fit
define	RV_MINWIDTH	Memr[P2R($1+28)] # Min Width of fitting region
define	RV_MAXWIDTH	Memr[P2R($1+29)] # Max Width of fitting region
define	RV_MAXITERS	Memi[$1+30]	 # Max number of iterations
define	RV_PEAK		Memi[$1+31]	 # Is fitheight relative to peak height?
define	RV_TOLERANCE	Memr[P2R($1+32)] # Fitting tolerance
define	RV_WEIGHTS	Memr[P2R($1+33)] # Weighting power
define	RV_WINPAR	Memr[P2R($1+34)] # Size of plot window
define	RV_WINCENPAR	Memr[P2R($1+35)] # Center of plot window
define	RV_WINDOW	Memi[$1+36]	 # Size of plot window (array index)
define	RV_WINCENTER	Memi[$1+37]	 # Center of plot window (array index)
define	RV_WINL		Memi[$1+38]	 # Left edge of window (array index)
define	RV_WINR		Memi[$1+39]	 # Right edge of window (array index)

# Miscellaneous values
define  RV_APNUM	Memi[$1+40]	 # Aperture number
define	RV_CCFNPTS	Memi[$1+41]	 # No. points in CCF
define  RV_CURAPNUM	Memi[$1+42]	 # Current aperture
define  RV_DI1		Memi[$1+43]	 # Deblend continuum start index
define  RV_DSCALE	Memr[P2R($1+44)] # Deblend continuum scale
define  RV_DSLOPE	Memr[P2R($1+45)] # Deblend continuum slope
define  RV_DX1		Memr[P2R($1+46)] # Start of deblend region
define  RV_DY1		Memr[P2R($1+47)] # End of deblend region
define  RV_DX2		Memr[P2R($1+48)] # Start of deblend region
define  RV_DY2		Memr[P2R($1+49)] # End of deblend region
define  RV_FILL		Memi[$1+50]	 # Sample region filling type
define  RV_FFTNPTS	Memi[$1+51]	 # Npts in FFT of spectrum
define	RV_IMNUM	Memi[$1+52]	 # Image no. in input list
define	RV_IMUPDATE	Memi[$1+53]	 # Update image headers?
define	RV_IS_DOUBLE	Memi[$1+54]	 # Update image headers?
define	RV_MODES	Memi[$1+55]	 # Command mode structure
define	RV_NOBJS	Memi[$1+56]	 # Number of object spectra
define	RV_NTEMPS	Memi[$1+57]	 # Number of template spectra
define  RV_NFITP	Memi[$1+58]	 # Number of peak points fit
define	RV_NPTS		Memi[$1+59]	 # No. points in object
define	RV_NSHIFTS	Memi[$1+60]	 # No. of shift in deblend
define	RV_NUMAPS	Memi[$1+61]	 # No. of apertures in image
define  RV_OAPNUM	Memi[$1+62]	 # Object aperture number
define	RV_REBIN	Memi[$1+63]	 # Which spectrum to rebin
define	RV_RNPTS	Memi[$1+64]	 # No. points in refrence
define  RV_RAPNUM	Memi[$1+65]	 # Template aperture number
define  RV_TEMPNUM	Memi[$1+66]	 # Template image number
define  RV_UPDATE	Memi[$1+67]	 # Update since write flag
define  RV_VERBOSE	Memi[$1+68]	 # Verbose output format types
define	RV_ZTHRESH	Memr[P2R($1+69)] # Output redshift threshold
 
# Observatory values
define	RV_OBSPTR	Memi[$1+70]	 # Observation Location (ptr)
define	RV_ALTITUDE	Memr[P2R($1+71)] # Altitude of observation
define	RV_LATITUDE	Memr[P2R($1+72)] # Latitude of observation
define	RV_LONGITUDE	Memr[P2R($1+73)] # Logitude of observation

# Output Miscellaneous values
define	RV_NEWGRAPH	Memi[$1+75]	 # GTOOLS newgraph flag
define  RV_RECORD	Memi[$1+76]	 # Output record being written
define	RV_TXFD		Memi[$1+77]  	 # Text file FD
define	RV_GRFD		Memi[$1+78]	 # Metacode file FD
define	RV_VBFD		Memi[$1+79]	 # Verbose logfile FD
define	RV_CCFFILE	Memi[$1+80]	 # Output ccf File
define	RV_CCFTYPE	Memi[$1+81]	 # Output ccf Type (image|text)
define	RV_STATLINE	Memi[$1+82]	 # Status line output flag
define	RV_TEMPCODE	Memi[$1+83]	 # Template code on output
define	RV_TCODE	Memi[$1+84]	 # Template code array ptr
define	RV_PRINTZ	Memi[$1+85]	 # Output z values instead of velocities

# Plotting Miscellaneous values
define	RV_DTYPE	Memi[$1+90]	 # Data type
define	RV_GTYPE	Memi[$1+91]	 # Graph type
define	RV_RESDONE	Memi[$1+92]	 # Residuals plotted?
define	RV_SPMKEY	Memi[$1+93]	 # Spec-mode plot switch
define	RV_SPMPLOT	Memi[$1+94]	 # Spec-mode plot switch
define	RV_WHERE	Memi[$1+95]	 # Where is data plotted on split screen
define  RV_X1           Memr[P2R($1+96)] # Starting plot scale
define  RV_X2           Memr[P2R($1+97)] # Ending plot scale
define  RV_Y1           Memr[P2R($1+98)] # Bottom plot scale (ccf plot)
define  RV_Y2           Memr[P2R($1+99)] # Top plot scale (ccf plot)

# Dispersion Info and Misc.
define	RV_APPARAM	Memi[$1+100]	  # APNUM parameter string (ptr)
define	RV_APLIST	Memi[$1+101]	  # Aperture ranges list
define	RV_CMD		Memi[$1+102]	  # Current cursor keystroke command
define	RV_DCBIAS	Memr[P2R($1+103)] # DC BIAS of the object spectrum
define	RV_DCFLAG	Memi[$1+104]	  # Is data in log-lambda space?
define	RV_DELTAV	Memr[P2R($1+105)] # Velocity per pixel
define	RV_DO_CORRECT	Memi[$1+106]	  # Do the heliocentric correction?
define	RV_OFORMAT	Memi[$1+107]	  # Data format (1D, echelle, multispec)
define	RV_RFORMAT	Memi[$1+108]	  # Data format (1D, echelle, multispec)
define	RV_FWHM_Y	Memr[P2R($1+109)] # Correlation coeff for FWHM calc.
define	RV_GLOB_W1	Memr[P2R($1+110)] # Global w1
define	RV_GLOB_W2	Memr[P2R($1+111)] # Global w2
define	RV_NEWXCOR	Memi[$1+112]	  # Do a new correlation?
define	RV_OW0		Memr[P2R($1+113)] # Object W0
define	RV_OW2		Memr[P2R($1+114)] # Object endpoint of dispersion
define	RV_OWPC		Memr[P2R($1+115)] # Object WPC
define	RV_RW0		Memr[P2R($1+116)] # Reference W0
define	RV_RW2		Memr[P2R($1+117)] # Template endpoint of dispersion
define	RV_RWPC		Memr[P2R($1+118)] # Reference WPC
define	RV_DO_REBIN	Memi[$1+119]	  # Rebin the data?

# The answers
define	RV_VOBS		Memd[P2D($1+120)]  # Observed velocity (vel)
define	RV_VCOR		Memd[P2D($1+122)]  # Corrected velocity (vel)
define	RV_ERROR	Memd[P2D($1+124)]  # Obs. Velocity error (vel)
define	RV_HJD		Memd[P2D($1+126)]  # Heliocentric JD of obs (days)
define	RV_MJD_OBS	Memd[P2D($1+128)]  # Heliocentric JD of obs (days)
define	RV_VREL		Memr[P2R($1+131)]  # Relative vel. from pix shift
define	RV_R		Memr[P2R($1+132)]  # Tonry&Davis 'R' parameter (vel)
define	RV_SHIFT	Memr[P2R($1+133)]  # Computed shift value (pix)
define	RV_SIGMA	Memr[P2R($1+134)]  # Error of fit (pix)
define  RV_FWHM		Memr[P2R($1+135)]  # FWHM of ccf peak
define  RV_HEIGHT	Memr[P2R($1+136)]  # Height of ccf peak (fft only)
define  RV_DISP		Memr[P2R($1+137)]  # Dispersion
define  RV_ERRCODE	Memi[$1+138]       # Error code for comment
define  RV_DBL_SHIFT	Memi[$1+139]       # Deblended velocity struct ptr

# Pointers for other packages 
define	RV_GP		Memi[$1+140]	   # GIO pointer
define  RV_MGP		Memi[$1+141]	   # Metacode GIO pointer
define  RV_GT		Memi[$1+142]	   # GTOOLS pointer
define  RV_NLFIT	Memi[$1+143]	   # NLFIT pointer
define	RV_ICFIT	Memi[$1+144]	   # ICFIT pointer
define	RV_COEFFS	Memi[$1+145]	   # Coefficients pointer
define	RV_ECOEFFS	Memi[$1+146]	   # Error coefficients pointer
define	RV_CONT		Memi[$1+147]	   # Continuum params pointer
define	RV_FILTP	Memi[$1+148]	   # Filter params pointer
define	RV_KEYW		Memi[$1+149]	   # Keyword table pointer
define	RV_PLOTP	Memi[$1+150]	   # Plotpars params pointer
define	RV_MWCSP	Memi[$1+151]	   # MWCS structure pointer

# Sample correlation regions structure pointers
define	RV_OSAMPLE	Memi[$1+155]	   # Obj sample struct (ptr)
define	RV_RSAMPLE	Memi[$1+156]	   # Ref sample struct (ptr)

# Working array pointers.  Keep things in memory and reallocate space
# as needed.  All indexing automatically done by macros below.
define	RV_OBJECTS    	Memi[$1+160]	   # Object list ptr
define  RV_TEMPLATES   	Memi[$1+161]	   # Template list ptr
define	RV_OBJCONT  	Memi[$1+162]	   # Object normalized flag
define	RV_REFCONT  	Memi[$1+163]	   # Reference normalized flag
define	RV_OCONTP  	Memi[$1+164]	   # Object normalized data ptr
define	RV_RCONTP  	Memi[$1+165]	   # Reference normalized data ptr
define	RV_ANTISYM	Memi[$1+166]	   # CCF Antisymmetric noise ptr
define	RV_ERRCOMMENTS	Memi[$1+167]	   # Error comments ptr
define	RV_TEMPVEL	Memi[$1+168]	   # All template velocities
define	RV_APNUMKWD	Memi[$1+169]	   # APNUM keyword strings (ptr)

# File names and stuff.
define	RV_IMAGE	Memi[$1+170]	   # Object image name 
define	RV_RIMAGE	Memi[$1+171]	   # Ref image name 
define  RV_SPOOL	Memi[$1+172]	   # Root spool name 
define	RV_DEVICE	Memi[$1+173]	   # Output device name 
define	RV_OBJNAME	Memi[$1+174]	   # Object Name
define	RV_TEMPNAME	Memi[$1+175]	   # Template Name

# Output color values.
define	RV_TXTCOLOR	Memi[$1+176]	   # Text color
define	RV_LINECOLOR	Memi[$1+177]	   # Overplot line colors

# Package Debugging info. (To be deleted in installed software.)
define	DBG_DEBUG	Memi[$1+180]	   # Debug flag
define	DBG_FNAME	Memi[$1+181]	   # Debug filename (ptr)
define	DBG_FD		Memi[$1+182]	   # Debug file descriptor
define	DBG_LEVEL	Memi[$1+183]	   # Level of debugging info
define 	DBG_OTHER	Memi[$1+184]	   # Compare algorithms?
define 	DBG_KEYSTROKE	Memi[$1+185]	   # Intial keystroke command
define 	DBG_QUICK	Memi[$1+186]	   # Speed up graphics?


###################  End of structure definitions ##############################


# Useful Macro definitions.  All indexing is done in the macros themselves
# and pointers are assumed to be allocated at process startup.

# Current working data
define	OBJPIXX		Memr[RV_OPIXX($1)+$2-1]   # Pixel data 
define	OBJPIXY		Memr[RV_OPIXY($1)+$2-1]   # Pixel data 
define	REFPIXX		Memr[RV_RPIXX($1)+$2-1]   # Comparison data
define	REFPIXY		Memr[RV_RPIXY($1)+$2-1]   # Comparison data
define	WRKPIXX		Memr[RV_WKPIXX($1)+$2-1]  # Working space
define	WRKPIXY		Memr[RV_WKPIXY($1)+$2-1]  # Working space

# File names and stuff.
define	IMAGE		Memc[RV_IMAGE($1)]	  # Object image name 
define	RIMAGE		Memc[RV_RIMAGE($1)]	  # Ref image name 
define  SPOOL		Memc[RV_SPOOL($1)]	  # Root spool name 
define	DEVICE		Memc[RV_DEVICE($1)]	  # Output device name 
define	OBJNAME		Memc[RV_OBJNAME($1)]	  # Object Name
define	TEMPNAME	Memc[RV_TEMPNAME($1)]	  # Template Name

# Misc arrays.
define	ANTISYM		Memr[RV_ANTISYM($1)+$2-1] # Antisymmetric noise array
define	ERRCOMMENTS	Memc[RV_ERRCOMMENTS($1)]  # Error comment strings

# Deblending data struct.  We are insured there is enough space and offsets
# are correct assuming only 4 Gaussians are fit.  Length of the struct is 50.
define	DBL_SHIFT	Memr[RV_DBL_SHIFT($1)+$2-1]	# Self-explanatory
define	DBL_VOBS	Memr[RV_DBL_SHIFT($1)+5+$2-1]
define	DBL_VHELIO	Memr[RV_DBL_SHIFT($1)+10+$2-1]
define	DBL_VERR	Memr[RV_DBL_SHIFT($1)+15+$2-1]
define	DBL_R		Memr[RV_DBL_SHIFT($1)+20+$2-1]
define	DBL_FWHM	Memr[RV_DBL_SHIFT($1)+25+$2-1]
define	DBL_HEIGHT	Memr[RV_DBL_SHIFT($1)+30+$2-1]
define	DBL_COEFFS	Memr[RV_DBL_SHIFT($1)+35+$2-1]
define	DBL_NFITP	RV_NFITP($1)		  # Npts fit in deblend
define	DBL_I1		RV_DI1($1)		  # Index of start
define	DBL_X1		RV_DX1($1)		  # Left WCS of fit
define	DBL_Y1		RV_DY1($1)		  # Bottom WCS of fit
define	DBL_X2		RV_DX2($1)		  # Right WCS of fit
define	DBL_Y2		RV_DY2($1)		  # Top WCS of fit
define	DBL_SCALE	RV_DSCALE($1)		  # Amplitude scale factor
define	DBL_SLOPE	RV_DSLOPE($1)		  # Slope of continuum
define	DBL_NSHIFTS	RV_NSHIFTS($1)		  # No. of components
define	IS_DBLSTAR	RV_IS_DOUBLE($1)	  # Deblend fit flag 

# Aperture Dispersion information
define	APLIST		Memi[RV_APLIST($1)+$2-1]  # List of apertures to be used
define	APNUM		Memc[RV_APNUMKWD($1)+(($2-1)*SZ_APNUM)]
define	APPARAM		Memc[RV_APPARAM($1)]	  # APNUM parameter string
define	CURAPNUM	RV_CURAPNUM($1)	  	  # Current number in list
define	NUMAPS		RV_NUMAPS($1)	  	  # Number of apertures in image
define  OAPNUM		RV_OAPNUM($1)		  # Object aperture number
define  RAPNUM		RV_RAPNUM($1)		  # Reference aperture number

# Flags and miscellaneous 
define	TEMPVEL		Memr[RV_TEMPVEL($1)+$2-1] # Template velocity array
define	COEFF		Memr[RV_COEFFS($1)+$2-1]  # Coefficients array
define	ECOEFF		Memr[RV_ECOEFFS($1)+$2-1] # Coefficients array
define	OCONT_DATA	Memr[RV_OCONTP($1)+$2-1]  # Obj contin norm data
define	RCONT_DATA	Memr[RV_RCONTP($1)+$2-1]  # Ref contin norm data
define	DEBUG_FNAME	Memc[DBG_FNAME($1)]	  # Debugging info file name
define	TEMPCODE	Memi[RV_TCODE($1)+$2-1]	  # Output template code
define	OBJCONT		RV_OBJCONT($1) 		  # Have objects been normalized
define	REFCONT		RV_REFCONT($1) 		  # Have temps been normalized
