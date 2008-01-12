# GIOTR.H -- Global definitions for the GIOTR graphics i/o workstation
# transformation and i/o program unit.  Note: requires <gio.h>.

define	DEF_MAXLENFRAMEBUF	128000
define	DEF_LENFRAMEBUF		8192
define	INC_LENFRAMEBUF		4096
define	DEF_LENSCRATCHBUF	256
define	INC_LENSCRATCHBUF	256
define	MAX_PSEUDOFILES		10
define	SZ_TRDEVNAME		229
define	SZ_KERNFNAME		259
define	LEN_GTRCOM		28	# see "gtr.com"
define	KSHIFT			10000	# encode pr ("etc$prpsio.x") such that
					#
					#    ((pr*KSHIFT)+stream) > LAST_FD
					#
					# see also <gio.h>

define	LEN_TRSTRUCT	(564+187)

define	TR_PID		Memi[P2I($1)]		# process id of kernel
define	TR_IN		Memi[P2I($1+1)]		# input from process
define	TR_OUT		Memi[P2I($1+2)]		# output to process
define	TR_TTY		Memi[P2I($1+3)]		# graphcap descriptor
define	TR_SPOOLDATA	Memi[P2I($1+4)]		# spool metacode instructions
define	TR_FRAMEBUF	Memi[P2I($1+5)]		# pointer to frame buffer
define	TR_LENFRAMEBUF	Memi[P2I($1+6)]		# length of the frame buffer
define	TR_MAXLENFRAMEBUF Memi[P2I($1+7)]		# max length of the frame buffer
define	TR_IP		Memi[P2I($1+8)]		# input pointer into frame buf
define	TR_OP		Memi[P2I($1+9)]		# output pointer into frame buf
define	TR_LASTOP	Memi[P2I($1+10)]		# last OP (for undo)
define	TR_SCRATCHBUF	Memi[P2I($1+11)]		# for annotating plots
define	TR_LENSCRATCHBUF Memi[P2I($1+12)]		# length of the scratch buffer
define	TR_OPSB		Memi[P2I($1+13)]		# output pointer, scratch buf
define	TR_NOPEN	Memi[P2I($1+14)]		# number of opens
define	TR_REDIR	Memi[P2I($1+15)]		# redirection information
define	TR_WCS		Memi[P2I($1+16)]		# WCS selected, 0 if none
define	TR_PAGE		Memi[P2I($1+17)]		# clear screen for text
define	TR_WAITPAGE	Memi[P2I($1+18)]		# grc_waitpage flag
define	TR_WSOPEN	Memi[P2I($1+19)]		# workstation open count
define	TR_SKIPOPEN	Memi[P2I($1+20)]		# skip wsopen in metacode
define	TR_WSACTIVE	Memi[P2I($1+21)]		# workstation activated?
define	TR_WSACTSAVE	Memi[P2I($1+22)]		# save old wsactive state
define	TR_INTERACTIVE	Memi[P2I($1+23)]		# the user graphics terminal?
			# (open)
define	TR_TXAP		($1+30)			# text drawing attributes
define	TR_PLAP		($1+40)			# text drawing attributes
define	TR_DEVNAME	Memc[P2C($1+44)]	# device name
define	TR_KERNFNAME	Memc[P2C($1+274)]	# name of kernel file (or "cl")
define	TR_GTRCOM	Memi[P2I($1+534)]		# storage for the gtr common
define	TR_WCSPTR	(($1)+564+($2)*LEN_WCS)	# WCS storage (0=not used)
