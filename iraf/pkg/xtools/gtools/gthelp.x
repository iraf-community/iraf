# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GT_HELP -- Page graphics help from a file.
# This routine should not be called anymore.

procedure gt_help (file)

char	file[ARB]			# File to be paged

begin
	call pagefile (file, "")
end
