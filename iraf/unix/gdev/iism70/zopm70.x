# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ZOPM70 -- Open the IIS for binary file i/o.  The device will be automatically
# allocated if necessary.

procedure zopm70 (device, mode, chan)

char	device[ARB]		# packed UNIX device name
int	mode			# access mode
int	chan			# receives device channel

begin
	call zopnbf (device, mode, chan)
end
