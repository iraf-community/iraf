# Selection method keywords and codes.

define	SELECT	"|match|nearest|preceding|following|interp|average|"
define	MATCH		1	# Match input and reference lists
define	NEAREST		2	# Nearest reference
define	PRECEDING	3	# Preceding reference
define	FOLLOWING	4	# Following reference
define	INTERP		5	# Interpolate between nearest references
define	AVERAGE		6	# Average first two reference spectra

# Reference list types.

define	LIST		1	# References are an image list
define	TABLE		2	# Referenece are a table

# Maximum number of aperture ranges.
define	NRANGES		100

# Message codes (see procedure refprint)

define	NO_SPEC		1	# Spectrum not found (immap failed)
define	NO_REF		2	# Reference spectrum not found (immap failed)
define	NOT_REFSPEC	3	# Not a reference spectrum
define	NO_REFSPEC	4	# No reference spectrum found
define	DEF_REFSPEC	5	# Reference spectra already defined
define	OVR_REFSPEC	6	# Override reference spectra
define	BAD_AP		7	# Bad aperture
define	BAD_REFAP	8	# Bad reference aperture
define	REF_GROUP	9	# Group
define	REF_AP		10	# Aperture
