# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"help.h"

# DO_FILE_TEMPLATE -- Called with a template matching a list of files containing
# help blocks.  All help blocks therein are processed and output.

procedure do_file_template (template, ctrl)

char	template[ARB]			# filename matching template
pointer	ctrl				# Help control structure

char	fname[SZ_FNAME]
int	option
pointer	list
pointer	fntopn()
int	fntgfn()

begin
	list = fntopn (template)
	option = H_OPTION(ctrl)

	while (fntgfn (list, fname, SZ_FNAME) != EOF)
	    call pr_helpblock (fname, "", "", NULL, ctrl)

	call fntcls (list)
end
