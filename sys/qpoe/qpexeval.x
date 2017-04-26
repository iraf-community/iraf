# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "qpex.h"

define	RTOL	(EPSILONR * 10.0)	# (only useful for normalized numbers)
define	DTOL	(EPSILOND * 10.0)

# QPEX_EVALUATE -- Evaluate the compiled event-attribute expression for the
# given seqeuence of event structs.  Expression evaluation for each event
# terminates as soon as an attribute test fails.  If all attribute tests
# succeed (i.e., the full expression is evaluated or the evaluate function
# runs to completion) then the event pointer is put on the output list O_EV,
# indicating that the event satisfies the given selection expression.
# The function value is the number of events which passed the filter.
# The time required to evaluate an expression depends upon the complexity of
# the expression to be evaluated, and the fraction of events which fail the
# test (a fail is determined quicker than a pass - attribute tests likely to
# fail should appear first in the expression).

int procedure qpex_evaluate (ex, i_ev, o_ev, nev)

pointer ex			#I QPEX descriptor (expression)
pointer i_ev[nev]		#I array of pointers to event structs
pointer	o_ev[nev]		#O receives the pointers of the passed events
int	nev			#I number of input events

int	i0			# integer data register
real	r0			# real data register
double	d0			# double data register
bool	pass			# expression value
int	npass			# number of events which pass expr

real	rbin
bool	pv_save[MAX_LEVELS]
pointer ip_save[MAX_LEVELS]
pointer lt, ev, ev_i, ev_r, ev_d, ip
int	level, bin, i, j, v

define	lut_ 91
define	ret_ 92
define	ev_s ev

begin
	npass = 0

	do j = 1, nev {
	    pass = false
	    ev = i_ev[j]

	    # Get event struct pointers of various types.
	    ev_d = (ev - 1) * SZ_SHORT / SZ_DOUBLE + 1
	    ev_i = (ev - 1) * SZ_SHORT / SZ_INT + 1
	    ev_r = ev_i

	    # Execute each compiled instruction in sequence until the value
	    # of the compiled attribute-value expression is known.  The call
	    # stack level is used to keep track of subroutine calls
	    # (subroutines are used to evaluate the indeterminate cells of
	    # compressed lookup tables).

	    # Notes on expression evaluation.
	    # ---------------------------------
	    # An expression consists of 1 or more expression terms, all of
	    # which must pass the event for the event to pass the filter.
	    #
	    # An expression term consists of a range list giving a list of
	    # acceptable values or ranges of values.
	    #
	    # The compiled expression consists of a sequence of instruction
	    # blocks, one for each expression term.  If the event fails to
	    # pass any expression term (instruction block) then the event
	    # fails and we are done.  Instruction blocks are of three types:
	    #
	    #	1)	Multiple instructions consisting of a load register,
	    #		any number of register tests, then a XIFF or XIFT
	    #		test at the end of the block.  PASS is set to false
	    # 		at the beginning of the block and can be set to true
	    #		by any register test to pass the event to the next
	    #		expression term.
	    #
	    #   2)	In simple cases the above can all be expressed as a
	    #		single test-and-exit-if-false instruction.  These are
	    #		the "X" instructions below.
	    #
	    #   3)	The lookup table (LUTX) instruction.  LUTX is like
	    #		case 2) except that it may compile as a sequence of
	    #		many instructions, using subprograms to evaluate the
	    #		value of LUT bins.  LUTs may nest.  When lookup table
	    #		evaluation is complete the instruction branches
	    #		forward to a closing XIFF which is used to test the
	    #		value of PASS returned by the executed LUT-bin
	    #		subprograms.
	    #
	    # The blocks of instructions corresponding to successive expression
	    # terms are executed until the PASS instruction is encountered.
	    # Execution of PASS terminates evaluation and passes the event.

	    ip = EX_START(ex)
	    level = 0

	    do i = 1, MAX_INSTRUCTIONS {
		pragma switch_no_range_check
		switch (OPCODE(ip)) {
		case NOP:				# null operation
		    ;
		case GOTO:				# go-to prog offset
		    ip = IARG1(ip)
		    next
		case XIFT: 				# exit if true
		    if (pass) {
			pass = false
			goto ret_
		    }
		case XIFF:				# exit if false
		    if (!pass)
			goto ret_
		case PASS:
		    pass = true
		    break
		case RET:				# return from subprog
ret_		    if (level > 0) {
			pass = (pv_save[level] || pass)
			ip = ip_save[level]
			level = level - 1
			next
		    } else
			break

		case LDSI: 				 # load registers
		    i0 = Mems[ev_s+IARG1(ip)]
		    pass = false
		case LDII:
		    i0 = Memi[ev_i+IARG1(ip)]
		    pass = false
		case LDRR:
		    r0 = Memr[ev_r+IARG1(ip)]
		    pass = false
		case LDRD:
		    d0 = Memr[ev_r+IARG1(ip)]
		    pass = false
		case LDDD:
		    d0 = Memd[ev_d+IARG1(ip)]
		    pass = false

		case BTTI: 				 # register tests
		    pass = pass || (and (i0, IARG1(ip)) != 0)
		case EQLI:
		    pass = pass || (i0 == IARG1(ip))
		case EQLR:
		    pass = pass || (abs(r0 - RARG1(ip)) < RTOL)
		case EQLD:
		    pass = pass || (abs(d0 - DARG1(ip)) < DTOL)
		case LEQI:
		    pass = pass || (i0 <= IARG1(ip))
		case LEQR:
		    pass = pass || (r0 <= RARG1(ip))
		case LEQD:
		    pass = pass || (d0 <= DARG1(ip))
		case GEQI:
		    pass = pass || (i0 >= IARG1(ip))
		case GEQR:
		    pass = pass || (r0 >= RARG1(ip))
		case GEQD:
		    pass = pass || (d0 >= DARG1(ip))
		case RNGI:
		    pass = pass || (i0 >= IARG1(ip) && i0 <= IARG2(ip))
		case RNGR:
		    pass = pass || (r0 >= RARG1(ip) && r0 <= RARG2(ip))
		case RNGD:
		    pass = pass || (d0 >= DARG1(ip) && d0 <= DARG2(ip))

		case BTTXS:				# load, test, and
		    i0 = Mems[ev_s+IARG1(ip)]		# exit if false
		    pass = (and (i0, IARG2(ip)) != 0)
		    if (!pass)
			goto ret_
		case BTTXI:
		    pass = (and (Memi[ev_i+IARG1(ip)], IARG2(ip)) != 0)
		    if (!pass)
			goto ret_

		case NEQXS:
		    pass = (Mems[ev_s+IARG1(ip)] != IARG2(ip))
		    if (!pass)
			goto ret_
		case NEQXI:
		    pass = (Memi[ev_i+IARG1(ip)] != IARG2(ip))
		    if (!pass)
			goto ret_
		case NEQXR:
		    pass = (abs(Memr[ev_r+IARG1(ip)] - RARG2(ip)) > RTOL)
		    if (!pass)
			goto ret_
		case NEQXD:
		    pass = (abs(Memd[ev_d+IARG1(ip)] - DARG2(ip)) > DTOL)
		    if (!pass)
			goto ret_

		case EQLXS:
		    pass = (Mems[ev_s+IARG1(ip)] == IARG2(ip))
		    if (!pass)
			goto ret_
		case EQLXI:
		    pass = (Memi[ev_i+IARG1(ip)] == IARG2(ip))
		    if (!pass)
			goto ret_
		case EQLXR:
		    pass = (abs(Memr[ev_r+IARG1(ip)] - RARG2(ip)) <= RTOL)
		    if (!pass)
			goto ret_
		case EQLXD:
		    pass = (abs(Memd[ev_d+IARG1(ip)] - DARG2(ip)) <= DTOL)
		    if (!pass)
			goto ret_

		case LEQXS:
		    pass = (Mems[ev_s+IARG1(ip)] <= IARG2(ip))
		    if (!pass)
			goto ret_
		case LEQXI:
		    pass = (Memi[ev_i+IARG1(ip)] <= IARG2(ip))
		    if (!pass)
			goto ret_
		case LEQXR:
		    pass = (Memr[ev_r+IARG1(ip)] <= RARG2(ip))
		    if (!pass)
			goto ret_
		case LEQXD:
		    pass = (Memd[ev_d+IARG1(ip)] <= DARG2(ip))
		    if (!pass)
			goto ret_

		case GEQXS:
		    pass = (Mems[ev_s+IARG1(ip)] >= IARG2(ip))
		    if (!pass)
			goto ret_
		case GEQXI:
		    pass = (Memi[ev_i+IARG1(ip)] >= IARG2(ip))
		    if (!pass)
			goto ret_
		case GEQXR:
		    pass = (Memr[ev_r+IARG1(ip)] >= RARG2(ip))
		    if (!pass)
			goto ret_
		case GEQXD:
		    pass = (Memd[ev_d+IARG1(ip)] >= DARG2(ip))
		    if (!pass)
			goto ret_

		case RNGXS:
		    i0 = Mems[ev_s+IARG1(ip)]
		    pass = (i0 >= IARG2(ip) && i0 <= IARG3(ip))
		    if (!pass)
			goto ret_
		case RNGXI:
		    i0 = Memi[ev_i+IARG1(ip)]
		    pass = (i0 >= IARG2(ip) && i0 <= IARG3(ip))
		    if (!pass)
			goto ret_
		case RNGXR:
		    r0 = Memr[ev_r+IARG1(ip)]
		    pass = (r0 >= RARG2(ip) && r0 <= RARG3(ip))
		    if (!pass)
			goto ret_
		case RNGXD:
		    d0 = Memd[ev_d+IARG1(ip)]
		    pass = (d0 >= DARG2(ip) && d0 <= DARG3(ip))
		    if (!pass)
			goto ret_

		case LUTXS:				# lookup tables
		    i0 = Mems[ev_s+IARG1(ip)]
		    lt = IARG2(ip)
		    rbin = (i0 - int(LT_I0(lt))) * LT_IS(lt)
		    goto lut_
		case LUTXI:
		    i0 = Memi[ev_i+IARG1(ip)]
		    lt = IARG2(ip)
		    rbin = (i0 - int(LT_I0(lt))) * LT_IS(lt)
		    goto lut_
		case LUTXR:
		    r0 = Memr[ev_r+IARG1(ip)]
		    lt = IARG2(ip)
		    rbin = (r0 - LT_R0(lt)) * LT_RS(lt)
		    goto lut_
		case LUTXD:
		    d0 = Memd[ev_d+IARG1(ip)]
		    lt = IARG2(ip)
		    rbin = (d0 - LT_D0(lt)) * LT_DS(lt)
lut_
		    # Common code for any lookup table.
		    if (rbin <= 0)
			v = LT_LEFT(lt)
		    else {
			bin = int(rbin) + 1
			if (bin > LT_NBINS(lt))
			    v = LT_RIGHT(lt)
			else
			    v = LT_LUT(lt,bin)
		    }

		    # Table value may be 0, 1, or indeterminate, i.e., the
		    # offset of a subprogram to be called to evaluate the
		    # subrangelist for that bin.

		    if (v == 0) {
			# Table value is zero, !pass, all done.
			pass = false
			goto ret_

		    } else if (v > 1) {
			# Table value is indeterminate and depends on the
			# data value.  Call subroutine to evaluate subrange.
			# At level=0 where we are starting to evaluate an
			# independent expression term we must initialize pass
			# to false before entering the subprogram instruction
			# sequence.

			if (level == 0)
			    pass = false

			level = level + 1
			pv_save[level] = pass

			if (IARG3(ip) != NULL)
			    ip_save[level] = IARG3(ip)
			else
			    ip_save[level] = ip + LEN_INSTRUCTION

			pass = false
			ip = EX_PB(ex) + v
			next

		    } else if (v == 1) {
			# Table value is one, value passes this test.
			pass = true
		    }

		    # Go to the jump address if set.  The jump is needed
		    # to skip over any subprograms that may have been compiled
		    # after the LUTX.

		    if (IARG3(ip) != NULL) {
			ip = IARG3(ip)
			next
		    }
		}

		# Advance to the next instruction.
		ip = ip + LEN_INSTRUCTION
	    }

	    # Output event pointer if event passed the filter.
	    if (pass) {
		npass = npass + 1
		o_ev[npass] = ev
	    }
	}

	return (npass)
end
