# FREESCREEN -- Free a window's buffer
#
# B.Simon	26-Sep-90	Original

procedure freescreen (buffer)

pointer	buffer		# i: Buffer allocated by wgetscr
#--

begin
	if (buffer != NULL)
	    call mfree (buffer, TY_CHAR)
end
