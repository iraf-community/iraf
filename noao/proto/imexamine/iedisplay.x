# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>

# IE_DISPLAY -- Display an image.  For the sake of convenience in this
# prototype program we do this by calling a task via the cl.  This is an
# interface violation which we try to mitigate by using a CL parameter to
# hide the knowledge of how to format the command (as well as make it easy
# for the user to control how images are displayed).

procedure ie_display (ie, image, frame)

pointer	ie			#I imexamine descriptor
char	image[ARB]		#I image to be displayed
int	frame			#I frame in which to display image

int	nchars
pointer	sp, d_cmd, d_args, d_template, im
int	gstrcpy(), strmac(), ie_getnframes()
pointer	immap()

begin
	call smark (sp)
	call salloc (d_cmd, SZ_LINE, TY_CHAR)
	call salloc (d_args, SZ_LINE, TY_CHAR)
	call salloc (d_template, SZ_LINE, TY_CHAR)

	# Verify that the named image or image section exists.
	iferr (im = immap (image, READ_ONLY, 0)) {
	    call erract (EA_WARN)
	    call sfree (sp)
	    return
	} else
	    call imunmap (im)

	# Get the display command template.
	call clgstr ("display", Memc[d_template], SZ_LINE)

	# Construct the macro argument list, a sequence of EOS delimited
	# strings terminated by a double EOS.

	call aclrc (Memc[d_args], SZ_LINE)
	nchars = gstrcpy (image, Memc[d_args], SZ_LINE) + 1
	call sprintf (Memc[d_args+nchars], SZ_LINE-nchars, "%d")
	    call pargi (frame)

	# Expand the command template to form the CL command.
	nchars = strmac (Memc[d_template], Memc[d_args], Memc[d_cmd], SZ_LINE)

	# Send the command off to the CL and wait for completion.
	call clcmdw (Memc[d_cmd])
	nchars = ie_getnframes (ie)

	call sfree (sp)
end
