# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"stdgraph.h"

.help stg_encode
.nf _________________________________________________________________________
STG_ENCODE -- Table driven binary encoder/decoder.  The encoder (which can
also decode) processes a format string, also referred to as a program, to
either encode an output string or decode an input string.  Internally the
encoder operates in two modes, copy mode and execute mode.  In copy mode
all format characters are copied to the output except the following special
characters:

	'	escape next character (literal)
	%	begin a formatted output string
	(	switch to execute mode (stack driven, RPN interpreter)

An ( appearing in the format string causes a mode switch to execute mode.
In execute mode characters are metacode instructions to be executed.  An
unescaped ) causes reversion to copy mode.  Parens may not be nested; an
( in execute mode is an instruction to push the binary value of ( on the
stack, and an ) in copy mode is copied to the output as a character.  In
execute mode the following characters are recognized as special instructions.
All other characters are instructions too, telling the encoder to push the
ASCII value of the character on the stack.

	'	escape next character (recognized everywhere)
	%	formatted output
	)	revert to copy mode
	#nnn	push signed decimal integer number nnn
	$ 	switch case construct
	.	pop number from stack and place in output string
	,	get next character from input string and push on stack
	&	modulus (similar to AND of low bits)
	+	add (similar to OR)
	-	subtract (similar to AND)
	*	multiply (shift left if pwr of 2)
	/	divide (shift right if pwr of 2)
	<	less than (0=false, 1=true)
	> 	greater than (0=false, 1=true)
	=	equals (0=false, 1=true)
	;	branch if: <bool> <offset> ;.  The ; is at offset zero.
       0-9	push register
	!N	pop stack into register N
	!!	pop N from stack and output an N millisecond delay

The encoder communicates with the outside world via three general purpose
data structures.

	registers	0-9 (integer only)
	memory		char array
	program		char array

The registers are used for parameter input and output as well as for storing
intermediate results.  R 1-3 are used for input and output arguments.  R 4-9
and R0 (R10) are reserved for use by the program.  R11 is the i/o pointer into
encoder memory, used for character input and output.  R12 should contain the
maximum memory address upon input.  Memory may be used for anything but is
normally used only for the input string or output string.  The program is the
format string.

Further documentation is given in the GIO reference manual.
.endhelp _____________________________________________________________________

define	SZ_FORMAT	10		# max length printf format
define	SZ_NUMSTR	10		# encoded numeric string

define	R1	registers[1]		# argument
define	R2	registers[2]		# argument
define	R3	registers[3]		# argument
define	R4	registers[4]		# scratch
define	R5	registers[5]		# scratch
define	R6	registers[6]		# scratch
define	R7	registers[7]		# scratch
define	R8	registers[8]		# scratch
define	R9	registers[9]		# scratch
define	R0	registers[10]		# scratch
define	IOP	registers[11]		# i/o pointer into encoder memory
define	TOP	registers[12]		# max memory location

# Inline macros.

define	memory_overflow_	1
define	stack_underflow_	2
define	stack_overflow_		3

define	input	{$1=memory[iop];iop=iop+1}
define	output	{memory[iop]=($1);iop=iop+1;if(iop>top)goto memory_overflow_}
define	push	{stack[sp]=($1);sp=sp+1}
define	pop	{sp=sp-1;$1=stack[sp]}


# STG_ENCODE -- Interpret a program, encoding values passed in registers into
# memory, or decoding memory into registers.

int procedure stg_encode (program, memory, registers)

char	program[ARB]			# program to be executed
char	memory[ARB]			# data space
int	registers[NREGISTERS]		# general purpose registers

int	x, y, num, ch, status
int	stack[LEN_STACK]
int	sp, pc, iop, top, incase
common	/sgecom/ pc, sp, iop, top, incase, stack
int	sge_execute()
include	"stdgraph.com"

begin
	# TEK format, %t.  This format deserves special treatment due to the
	# prevalence of tektronix compatible graphics terminals.

	if (program[1] == '%' && program[2] == 't') {
	    x = R1
	    y = R2
	    iop = IOP + 4
	    if (iop > top)
		goto memory_overflow_

	    memory[iop-4] = g_hixy[y+1]
	    memory[iop-3] = g_loy[y+1]
	    memory[iop-2] = g_hixy[x+1]
	    memory[iop-1] = g_lox[x+1]

	    IOP = iop
	    if (program[3] == EOS)
		return (OK)
	}

	# Process a general format string (as well as any chars following the
	# %t format).

	incase = NO
	iop    = IOP
	top    = TOP
	pc     = 1
	sp     = 1

	for (ch=program[pc];  ch != EOS;  ch=program[pc]) {
	    pc = pc + 1
	    if (ch == '%' && program[pc] != EOS) {
		if (program[pc] == 't') {
		    # Tek format again.
		    pc = pc + 1
		    x = R1
		    y = R2
		    iop = iop + 4
		    if (iop > top)
			goto memory_overflow_

		    memory[iop-4] = g_hixy[y+1]
		    memory[iop-3] = g_loy[y+1]
		    memory[iop-2] = g_hixy[x+1]
		    memory[iop-1] = g_lox[x+1]

		} else {
		    # Extract a general format specification and use it to
		    # encode the number on top of the stack.
		    pop (num)
		    if (sp < 1) {
			IOP = iop
			return (stack_underflow_)
		    } else
			call sge_printf (num, memory, iop, top, program, pc)
		}

	    } else if (ch == '(' && program[pc] != EOS) {
		# Switch to execute mode.
		status = sge_execute (program, memory, registers)
		if (status != OK)
		    return (status)

	    } else if (ch == '\'' && program[pc] != EOS) {
		# Escape next character.
		output (program[pc])
		pc = pc + 1

	    } else {
		# Copy an ordinary character to the output string.
		output (ch)
	    }
	}

	IOP = iop
	return (OK)

memory_overflow_
	IOP = iop
	return (memory_overflow_)
end


# SGE_EXECUTE -- Execute a metacode program stored in encoder memory starting
# at the location of the PC.  The stack, program counter, i/o pointer, and
# registers are shared by the copy and execute mode procedures via common.

int procedure sge_execute (program, memory, registers)

char	program[ARB]			# program to be executed
char	memory[ARB]			# data space
int	registers[NREGISTERS]		# general purpose registers

int	num, ch, a, b, neg, x, y
int	stack[LEN_STACK]
int	sp, pc, iop, top, incase, msec, npad, baud, envgeti(), btoi()
common	/sgecom/ pc, sp, iop, top, incase, stack
include	"stdgraph.com"
errchk	envgeti

begin
	# Execute successive single character instructions until either ) or
	# EOS is seen.  On a good host machine this case will be compiled as
	# a vectored goto with a loop overhead of only a dozen or so machine
	# instructions per loop.

	for (ch=program[pc];  ch != EOS;  ch=program[pc]) {
	    pc = pc + 1

	    switch (ch) {
	    case '\'':
		# Escape next character (recognized everywhere).
		ch = program[pc]
		if (ch != EOS) {
		    # Push ASCII value of character.
		    push (ch)
		    pc = pc + 1
		}

	    case '%':
		if (program[pc] == 't') {
		    # Tek format again.
		    pc = pc + 1
		    x = R1
		    y = R2
		    iop = iop + 4
		    if (iop > top)
			goto memory_overflow_

		    memory[iop-4] = g_hixy[y+1]
		    memory[iop-3] = g_loy[y+1]
		    memory[iop-2] = g_hixy[x+1]
		    memory[iop-1] = g_lox[x+1]

		} else {
		    # Formatted output.
		    if (program[pc] != EOS) {
			pop (num)
			call sge_printf (num, memory, iop, top, program, pc)
		    } else
			output (ch)
		}

	    case ')':
		# End interpreter mode.
		return (OK)

	    case '#':
		# Push signed decimal integer number.
		neg = NO
		if (program[pc] == '-') {
		    neg = YES
		    pc = pc + 1
		}

		num = 0
		while (IS_DIGIT (program[pc])) {
		    num = num * 10 + TO_INTEG (program[pc])
		    pc = pc + 1
		}

		if (neg == YES)
		    push (-num)
		else
		    push (num)

	    case '$':
		# Switch case instruction.

		if (incase == NO) {
		    # Pop the switch off the stack.
		    pop (num)

		    # Search for case number 'num'.
		    for (ch=program[pc];  ch != EOS;  ch=program[pc]) {
			if (ch == '$') {
			    # End of switch statement.
			    pc = pc + 1
			    incase = NO
			    break

			} else if (program[pc+1] == '-') {
			    # Range of cases.
			    a = TO_INTEG (ch)
			    b = TO_INTEG (program[pc+2])
			    pc = pc + 3
			    if (a <= num && num <= b) {
				incase = YES
				break
			    }
			} else if (ch == 'D' || TO_INTEG(ch) == num) {
			    # Default or requested case.
			    pc = pc + 1
			    incase = YES
			    break

			}

			# Advance to the next case.  Leave pc pointing to the
			# N of case $N.

			if (ch != '$' && incase == NO) {
			    while (program[pc] != EOS && program[pc] != '$')
				pc = pc + 1
			    if (program[pc] == '$')
				pc = pc + 1
			}
		    }

		} else {
		    # $ encountered delimiting a case.  Search forward for
		    # $$ or EOS.

		    if (program[pc] != '$')
			for (ch=program[pc];  ch != EOS;  ch=program[pc]) {
			    pc = pc + 1
			    if (ch == '$' && program[pc] == '$')
				break
			}

		    if (program[pc] == '$')
			pc = pc + 1

		    incase = NO
		}

	    case '.':
		# Pop number from stack and place in output string as a
		# binary character.
		pop (num)
		output (num)

	    case ',':
		# Get next character from input string and push on stack.
		input (num)
		push (num)

	    case '&':
		# Modulus (similar to AND of low bits).
		pop (b)
		pop (a)
		push (mod (a, b))

	    case '+':
		# Add (similar to OR).
		pop (b)
		pop (a)
		push (a + b)

	    case '-':
		# Subtract (similar to AND).
		pop (b)
		pop (a)
		push (a - b)

	    case '*':
		# Multiply (shift left if pwr of 2).
		pop (b)
		pop (a)
		push (a * b)

	    case '/':
		# Divide (shift right if pwr of 2).
		pop (b)
		pop (a)
		push (a / b)

	    case '<':
		# Less than (0=false, 1=true).
		pop (b)
		pop (a)
		push (btoi (a < b))

	    case '>':
		# Greater than (0=false, 1=true).
		pop (b)
		pop (a)
		push (btoi (a > b))

	    case '=':
		# Equals (0=false, 1=true).
		pop (b)
		pop (a)
		push (btoi (a == b))

	    case ';':
		# If 2nd value on stack is true add 1st value on stack to PC.
		# Example: "12<#-8;".  The ; is at offset zero.
		pop (a)
		pop (b)
		if (b != 0)
		    pc = pc - 1 + a

	    case '0':
		# Push contents of register 0 (10).
		push (R0)
	    case '1':
		# Push contents of register 1.
		push (R1)
	    case '2':
		# Push contents of register 2.
		push (R2)
	    case '3':
		# Push contents of register 3.
		push (R3)
	    case '4':
		# Push contents of register 4.
		push (R4)
	    case '5':
		# Push contents of register 5.
		push (R5)
	    case '6':
		# Push contents of register 6.
		push (R6)
	    case '7':
		# Push contents of register 7.
		push (R7)
	    case '8':
		# Push contents of register 8.
		push (R8)
	    case '9':
		# Push contents of register 9.
		push (R9)

	    case '!':
		if (program[pc] == '!') {
		    # !!: Pop stack and generate delay.
		    pc = pc + 1
		    pop (msec)
		    iferr (baud = envgeti ("ttybaud"))
			baud = 9600
		    npad = real(msec) * (real(baud) / 8. / 1000.)
		    while (npad > 0) {
			output (PADCHAR)
			npad = npad - 1
		    }
		} else {
		    # !N: Pop stack into register N.
		    num = TO_INTEG (program[pc])
		    if (num >= 0 && num <= 9) {
			if (num == 0)
			    num = 10
			pop (registers[num])
			pc = pc + 1
		    } else
			output (ch)
		}

	    default:
		# Push ASCII value of character.
		push (ch)
	    }

	    if (sp <= 0)
		return (stack_underflow_)
	    if (sp > LEN_STACK)
		return (stack_overflow_)
	}

	return (OK)

memory_overflow_
	return (memory_overflow_)
end


# SGE_PRINTF -- Process a %.. format specification.  The number to be encoded
# has already been popped from the stack into the first argument.  The encoded
# number is returned in memory at IOP, leaving IOP positioned to the first
# char following the encoded number.  The format used to encode the number is
# extracted from the program starting at PC.  PC is left pointing to the first
# character following the format.

procedure sge_printf (number, memory, iop, top, program, pc)

int	number			# number to be encoded
char	memory[top]		# output buffer
int	iop			# index of first char to be written	(in/out)
int	top			# size of output buffer
char	program[ARB]		# contains printf format string
int	pc			# index of first char of format string	(in/out)

char	format[SZ_FORMAT]
char	numstr[SZ_NUMSTR]
int	op, ch, junk
int	gstrcpy(), itoc()

begin
	# Extract format %w.dC, a string of digits, -, or ., delimited by a
	# letter.  The format %w.dr is followed by a single character which
	# must also be included in the format string.

	format[1] = '%'
	op = 2
	for (ch=program[pc];  ch != EOS;  ch=program[pc]) {
	    pc = pc + 1
	    format[op] = ch
	    op = op + 1
	    if (IS_LOWER(ch)) {
		if (ch == 'r' && program[pc] != EOS) {
		    # Radix digit follows %r.
		    format[op] = program[pc]
		    op = op + 1
		    pc = pc + 1
		}
		break
	    }
	}
	format[op] = EOS

	# Encode the number using the extracted format string.  The case of
	# a simple decimal encoding is optimized.

	if (format[2] == 'd')
	    junk = itoc (number, numstr, SZ_NUMSTR)
	else {
	    iferr {
		call sprintf (numstr, SZ_NUMSTR, format)
		    call pargi (number)
	    } then
		numstr[1] = EOS
	}

	# Move the encoded number to encoder memory, advancing the i/o
	# pointer and taking care not to overrun memory.  Leave the iop
	# pointing AT, not after, the EOS output by gstrcpy.

	iop = iop + gstrcpy (numstr, memory[iop], top - iop + 1)
end
