# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# QPF_UPDHDR -- Update the image header.  This is a no-op for QPF since the
# datafiles can only be accessed READ_ONLY via IMIO.

procedure qpf_updhdr (im, status)

pointer	im			#I image descriptor
int	status			#O output status

begin
	status = OK
end
