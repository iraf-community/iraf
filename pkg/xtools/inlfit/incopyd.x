include	<pkg/inlfit.h>
include	"inlfitdef.h"

# IN_COPY -- Copy INLFIT parameter structure, into another. The destination
# structure is allocated if the pointer is NULL.

procedure in_copyd (insrc, indst)

pointer	insrc			# source INLFIT pointer
pointer	indst			# destination INLFIT pointer

int	in_geti()
double	in_getd()
pointer	in_getp()

begin
#	# Debug.
#	call eprintf (
#	    "in_copy: insrc=%d, indst=%d\n")
#	    call pargi (insrc)
#	    call pargi (indst)

	# Allocate destination.
	if (indst == NULL) {

	    # Allocate structure memory.
	    call malloc (indst, LEN_INLSTRUCT, TY_STRUCT)

	    # Allocate memory for parameter values, changes, and list.
	    call malloc (IN_PARAM  (indst), in_geti (insrc, INLNPARAMS),
			 TY_DOUBLE)
	    call malloc (IN_DPARAM (indst), in_geti (insrc, INLNPARAMS),
			 TY_DOUBLE)
	    call malloc (IN_PLIST  (indst), in_geti (insrc, INLNPARAMS),
			 TY_INT)

	    # Allocate space for strings. All strings are limited
	    # to SZ_LINE or SZ_FNAME.
	    call malloc (IN_LABELS     (indst), SZ_LINE,  TY_CHAR)
	    call malloc (IN_UNITS      (indst), SZ_LINE,  TY_CHAR)
	    call malloc (IN_FLABELS    (indst), SZ_LINE,  TY_CHAR)
	    call malloc (IN_FUNITS     (indst), SZ_LINE,  TY_CHAR)
	    call malloc (IN_PLABELS    (indst), SZ_LINE,  TY_CHAR)
	    call malloc (IN_PUNITS     (indst), SZ_LINE,  TY_CHAR)
	    call malloc (IN_VLABELS    (indst), SZ_LINE,  TY_CHAR)
	    call malloc (IN_VUNITS     (indst), SZ_LINE,  TY_CHAR)
	    call malloc (IN_USERLABELS (indst), SZ_LINE,  TY_CHAR)
	    call malloc (IN_USERUNITS  (indst), SZ_LINE,  TY_CHAR)
	    call malloc (IN_HELP       (indst), SZ_FNAME, TY_CHAR)
	    call malloc (IN_PROMPT     (indst), SZ_FNAME, TY_CHAR)

	    # Allocate space for floating point and graph substructures.
	    call malloc (IN_SFLOAT (indst), LEN_INLFLOAT, TY_DOUBLE)
	    call malloc (IN_SGAXES (indst), INLNGKEYS * LEN_INLGRAPH, TY_INT)
	}

	# Copy integer parameters.
	call in_puti (indst, INLFUNCTION,   in_geti (insrc, INLFUNCTION))
	call in_puti (indst, INLDERIVATIVE, in_geti (insrc, INLDERIVATIVE))
	call in_puti (indst, INLNPARAMS,    in_geti (insrc, INLNPARAMS))
	call in_puti (indst, INLNFPARAMS,   in_geti (insrc, INLNFPARAMS))

	# Copy parameter values, changes, and list.
	call amovd  (Memd[in_getp (insrc, INLPARAM)],
		      Memd[in_getp (indst, INLPARAM)],
		      in_geti (insrc, INLNPARAMS))
	call amovd  (Memd[in_getp (insrc, INLDPARAM)],
		      Memd[in_getp (indst, INLDPARAM)],
		      in_geti (insrc, INLNPARAMS))
	call amovi   (Memi[in_getp (insrc, INLPLIST)],
		      Memi[in_getp (indst, INLPLIST)],
		      in_geti (insrc, INLNPARAMS))

	# Copy defaults.
	call in_putd (indst, INLTOLERANCE, in_getd (insrc, INLTOLERANCE))
	call in_puti  (indst, INLMAXITER,   in_geti  (insrc, INLMAXITER))
	call in_puti  (indst, INLNREJECT,   in_geti  (insrc, INLNREJECT))
	call in_putd (indst, INLLOW,       in_getd (insrc, INLLOW))
	call in_putd (indst, INLHIGH,      in_getd (insrc, INLHIGH))
	call in_putd (indst, INLGROW,      in_getd (insrc, INLGROW))

	# Copy character strings.
	call in_pstr (indst, INLLABELS,     Memc[IN_LABELS     (insrc)])
	call in_pstr (indst, INLUNITS,      Memc[IN_UNITS      (insrc)])
	call in_pstr (indst, INLFLABELS,    Memc[IN_FLABELS    (insrc)])
	call in_pstr (indst, INLFUNITS,     Memc[IN_FUNITS     (insrc)])
	call in_pstr (indst, INLPLABELS,    Memc[IN_PLABELS    (insrc)])
	call in_pstr (indst, INLPUNITS,     Memc[IN_PUNITS     (insrc)])
	call in_pstr (indst, INLVLABELS,    Memc[IN_VLABELS    (insrc)])
	call in_pstr (indst, INLVUNITS,     Memc[IN_VUNITS     (insrc)])
	call in_pstr (indst, INLUSERLABELS, Memc[IN_USERLABELS (insrc)])
	call in_pstr (indst, INLUSERUNITS,  Memc[IN_USERUNITS  (insrc)])
	call in_pstr (indst, INLHELP,       Memc[IN_HELP       (insrc)])
	call in_pstr (indst, INLPROMPT,     Memc[IN_PROMPT     (insrc)])

	# Copy user defined functions.
	call in_puti (indst, INLUAXES,  in_geti (insrc, INLUAXES))
	call in_puti (indst, INLUCOLON, in_geti (insrc, INLUCOLON))
	call in_puti (indst, INLUFIT,   in_geti (insrc, INLUFIT))

	# Copy graph key, and axes.
	call in_puti (indst, INLGKEY, in_geti (insrc, INLGKEY))
	call amovi (IN_SGAXES (insrc), IN_SGAXES (indst),
	    INLNGKEYS * LEN_INLGRAPH)

	# Copy flags and counters.
	call in_puti (indst, INLOVERPLOT, in_geti (insrc, INLOVERPLOT))
	call in_puti (indst, INLPLOTFIT,  in_geti (insrc, INLPLOTFIT))
	call in_puti (indst, INLNREJPTS,  in_geti (insrc, INLNREJPTS))

	# Initialize number of points and variables.
	call in_puti (indst, INLNVARS, 0)
	call in_puti (indst, INLNPTS,  0)

	# Reallocate rejected point list and limit values.
	call in_bfinit (indst, in_geti (insrc, INLNPTS),
	    in_geti (insrc, INLNVARS))

	# Copy rejected point list and limit values.
	call amovi  (MEMP[in_getp (insrc, INLREJPTS)],
	    MEMP[in_getp (indst, INLREJPTS)], in_geti (indst, INLNPTS))
	call amovd (Memd[in_getp (insrc, INLXMIN)],
	    Memd[in_getp (indst, INLXMIN)], in_geti (indst, INLNVARS))
	call amovd (Memd[in_getp (insrc, INLXMAX)],
	    Memd[in_getp (indst, INLXMAX)], in_geti (indst, INLNVARS))
end
