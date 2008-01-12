# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

define	SZ_OBUF		8
define	CPU		1
define	CLK		2


# SYS_MTIME -- Mark the time, i.e., save the current clock and cpu times in
# the save buffer.

procedure sys_mtime (save_time)

long	save_time[2]			# mark time buffer

begin
	call zgtime (save_time[CLK], save_time[CPU])
end


# SYS_PTIME -- Print the cpu and clock time consumed since the last call to
# SYS_MTIME.

procedure sys_ptime (fd, opstr, save_time)

int	fd				# output file
char	opstr[ARB]			# optional operand name string
long	save_time[2]			# mark time buffer

int	op, junk
char	obuf[SZ_OBUF]
long	new_clk, new_cpu
int	d_clk, d_cpu, msec, sec, percent
int	itoc()

begin
	call zgtime (new_clk, new_cpu)
	d_clk = (new_clk - save_time[CLK]) 	# clk seconds
	d_cpu = (new_cpu - save_time[CPU])	# cpu msec

	call putline (fd, "Time ")
	if (opstr[1] != EOS) {
	    call putci (fd, '(')
	    call putline (fd, opstr)
	    call putline (fd, ") ")
	}

	# Output the cpu time in seconds.
	op = itoc (d_cpu / 1000, obuf, SZ_OBUF) + 1
	obuf[op] = '.'; op = op + 1
	msec = mod (d_cpu, 1000)
	if (msec < 100) {
	    obuf[op] = '0'; op = op + 1
	}
	if (msec < 10) {
	    obuf[op] = '0'; op = op + 1
	}
	if (msec > 0)
	    op = op + itoc (msec, obuf[op], SZ_OBUF-op+1)
	call putline (fd, obuf)

	# Output the clock time in minutes and seconds.
	call putci (fd, ' ')
	op = itoc (d_clk / 60, obuf, SZ_OBUF) + 1
	obuf[op] = ':'; op = op + 1
	sec = mod (d_clk, 60)
	obuf[op] = TO_DIGIT(sec/10); op = op + 1
	obuf[op] = TO_DIGIT(mod(sec,10)); op = op + 1
	obuf[op] = EOS
	call putline (fd, obuf)

	# Output the percent cpu utilization.
	call putci (fd, ' ')
	if (d_clk < 1)
	    call strcpy ("99", obuf, SZ_OBUF)
	else {
	    percent = min (99, d_cpu / d_clk / 10)
	    junk = itoc (percent, obuf, SZ_OBUF)
	}

	call putline (fd, obuf)
	call putline (fd, "%\n")
end
