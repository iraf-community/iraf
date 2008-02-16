# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GMPRINTF -- Formatted write a string value to a UI (user interface)
# parameter.
#
# NOTE - I don't think this code works yet.  Don't use it, use gmsg.

procedure gmprintf (gp, object, format)

pointer	gp			#I graphics descriptor
char	object[ARB]		#I object name
char	format[ARB]		#I print format

size_t	sz_val
pointer	sp, fmt

begin
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (fmt, sz_val, TY_CHAR)

	call sprintf (Memc[fmt], SZ_LINE, "\031%s %s\035\037")
	    call pargstr (object)
	    call pargstr (format)

	call flush (STDOUT)
	call printf (Memc[fmt])
	call sfree (sp)
end
