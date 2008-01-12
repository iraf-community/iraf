include defs

# ENDCOD -- Code thats gets executed when the END statement is encountered,
# terminating a procedure.

subroutine endcod (endstr)

character endstr(1)
include COMMON_BLOCKS
string	sepro "call zzepro"
string	sret "return"

	if (esp != 0)
	    call synerr ("Unmatched 'iferr' or 'then' keyword.")
	esp = 0					# error stack pointer
	body = NO
	ername = NO
	if (errtbl != NULL)
	    call rmtabl (errtbl)
	errtbl = NULL
	memflg = NO				# reinit mem decl flag

	if (retlab != NULL)
	    call outnum (retlab)
	call outtab
	call outstr (sepro)
	call outdon
	call outtab
	call outstr (sret)
	call outdon

	col = 6
	call outtab
	call outstr (endstr)
	call outdon
end
