define	TXT_MAXFD	64		# Maximum FD for stropen.


# XT_TXTOPEN -- Open a READ_ONLY text file which is possibly compiled into a
# procedure.
# 
# This is used to allow text files to be incorported in binaries but still use
# FIO.  The text file must be compiled into a program which is linked with
# into the binary (see txtcompile).  A file name of the form proc:nnnn, where
# nnnn is a number returned by locpr, calls the procedure which is expected to
# allocate a string buffer.  In this case the string buffer is opened with
# stropen.  Any other file name is opened as a READ_ONLY TEXT_FILE with
# normal FIO.

int procedure xt_txtopen (fname)

char	fname[ARB]		#I File name or proc:nnnn reference
int	fd			#R Null to open and non-null to close

int	ip, procptr, strncmp(), ctoi(), open(), stropen()
pointer	strbuf
errchk	zcall1, open, stropen

int	firsttime
data	firsttime/YES/

pointer	buf[TXT_MAXFD]
common	/xttxtn_com/ buf

begin
	# Make sure array of string buffer pointers is initialized.
	if (firsttime==YES) {
	    call aclri (buf, TXT_MAXFD)
	    firsttime = NO
	}

	# Determine type of open to use.
	if (strncmp (fname, "proc:", 5) == 0) {
	    ip = 1
	    if (ctoi (fname[6], ip, procptr) == 0)
		call error (1, "xt_txtopen: bad file specification")
	    call zcall1 (procptr, strbuf)
	    fd = stropen (Memc[strbuf], ARB, READ_ONLY)
	    if (fd > TXT_MAXFD) {
	        call close (fd)
		call mfree (strbuf, TY_CHAR)
		call error (1, "xt_txtopen: Too many file descriptors")
	    }
	    buf[fd] = strbuf
	} else
	    fd = open (fname, READ_ONLY, TEXT_FILE)

	return (fd)
end


# XT_TXTCLOSE -- Close procedure.

procedure xt_txtclose (fd)

int	fd			#O Null to open and non-null to close

pointer	buf[TXT_MAXFD]
common	/xttxtn_com/ buf

begin
	# Close file descriptor.
	call close (fd); fd = NULL
	if (fd <= TXT_MAXFD)
	    call mfree (buf[fd], TY_CHAR)
end
