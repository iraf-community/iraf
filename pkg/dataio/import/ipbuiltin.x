include "import.h"


# Define the builtin format names.  We also define the aliases in case the
# user specifies one of these instead, the 'sensed' format name is the
# proper name.

define  IP_BUILTINS     "|gif|giff\
			 |sunras|ras\
			 |xwd|x11|"

define  IP_GIF          1                       # CompuServe GIF format
define  IP_GIFF         2                       # CompuServe GIF format
define  IP_SUNRAS       3                       # Sun Rasterfile
define  IP_RAS          4                       # Sun Rasterfile
define  IP_XWD          5                       # X11 Window Dump
define  IP_X11          6                       # X11 Window Dump



# IP_PRBUILTIN -- Process a 'builtin' format.

procedure ip_prbuiltin (ip, fname)

pointer	ip					#i task struct pointer
char    fname[ARB]                              #i file name


begin
	# Branch off to the particular format.
	switch (IP_FCODE(ip)) {
	case IP_GIF, IP_GIFF:
	    call ip_gif (ip, fname, NO, NO)
	case IP_SUNRAS, IP_RAS:
	    call ip_ras (ip, fname, NO, NO)
	case IP_XWD, IP_X11:
	    call ip_xwd (ip, fname, NO, NO)
	default:
	   return
	}
end


# IP_BLTIN_INFO -- Process a 'builtin' format file information request.  These
# are done separately because in a builtin we can print information such as
# colormap information, compression schemes, etc.

procedure ip_bltin_info (ip, fname, verbose)

pointer	ip					#i task struct pointer
char    fname[ARB]                              #i file name
int	verbose					#i verbosity flag

begin
	# Branch off to the particular format.
	switch (IP_FCODE(ip)) {
	case IP_GIF, IP_GIFF:
	    call ip_gif (ip, fname, YES, verbose)
	case IP_SUNRAS, IP_RAS:
	    call ip_ras (ip, fname, YES, verbose)
	case IP_XWD, IP_X11:
	    call ip_xwd (ip, fname, YES, verbose)
	default:
	   return
	}
end


# IP_IS_BUILTIN -- See if this is a 'builtin' format.

int procedure ip_is_builtin (format)

char	format[ARB]				#i format to check

int	btoi(), strdic()

begin
	return (btoi(strdic(format,format,SZ_FNAME,IP_BUILTINS) != 0))
end


# IP_FCODE -- Get the format code for a builtin format.

int procedure ip_fcode (format)

char	format[ARB]				#i format to check
int	strdic()

begin
	return (strdic (format, format, SZ_FNAME, IP_BUILTINS))
end
