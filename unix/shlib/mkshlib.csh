#! /bin/csh
# Make the Sun/IRAF shared library and associated objects.

#set	echo
unset	noclobber

set	BMACH = `ls -l $iraf/bin | sed -e 's+^.*bin\.++'`
set	MACH  = $BMACH
set	GCRT0 = crt0.o
set	PGLIB = ""

if ($BMACH == pg) then
    set MACH = f68881
    set GCRT0 = gcrt0.o
    set PGLIB = -lc_p
endif
if (`mach` == mc68020) then
    setenv FLOAT_OPTION $MACH
endif

set	PROC = S.e
set	SNML = S.nm.$BMACH
set	SVER = S.ver.$BMACH
set	OMIT = omit.$BMACH
set	EXCL = "zshlib.o zzstrt.o"
set	ADDR = 0a000000		# default base address of shared region
set	PGSZ = 0x2000		# page size

set	FIOCOMSZ = 0x1560	# exported commons
set	XERCOMSZ = 0x158
set	TOTCOMSZ = 0x16b8

if (`mach` == i386) then
    set	FHSZ = 0xd0		# .e file header size
else
    set	FHSZ = 0x20
endif

# Process command line options.
while ("$1" != "")
    switch ($1)
    case "-a":			# set base address of shared library
	shift
	set ADDR = $1
	# I couldn't get a direct !~ csh pattern match test to work here.
	if ("`echo $ADDR | grep '[0-9][0-9a-f]*'`" == "") then
	    set ADDR = 0a000000
	    echo -n "Warning: shared library base address not given,"
	    echo " defaults to $ADDR"
	endif
	breaksw
    case "-as":
	set PROC = assemble	# assemble S.s, V.s
	breaksw
    case "-c":			# delete any temporary files
	set PROC = clean
	breaksw
    case "-f":
	set PROC = files
	set version = `cat $SVER`
	breaksw
    case "-l":			# merely relink the shared library
	set PROC = link
	breaksw
    case "-nm":
	set PROC = names
	breaksw
    case "-rl":			# merely relink the shared library
	set PROC = relink
	breaksw
    endsw
    shift
end

set	TB = `echo 0x$ADDR+$FHSZ=X | adb`
set	libs = "libos.a libex.a libsys.a libvops.a"

if ($?IRAFULIB) then
    if ($PROC == S.e) then
	echo "Warning: user library IRAFULIB=$IRAFULIB will be searched"
    endif
    set dirs = "$IRAFULIB $iraf/lib $iraf/unix/bin.`mach`"
else
    set dirs = "$iraf/lib $iraf/unix/bin.`mach`"
endif

# In the following, the object V.o must be the first object to be linked,
# as we require it to be at a fixed and predictable address.

set	OBJS = "Slib.o Malloc.o lib*.o zzzend.o"

switch ($MACH)
case sparc:
	set lflags = "-Bstatic -d -dc -dp -e start -X -T $TB"
	set objs   = "V.o /usr/lib/crt0.o $OBJS"
	set hlibs  = "-lm -lF77 -lI77 -lm -lc"
	set mcode  = 1
	breaksw
case i386:
	set lflags = "-Bstatic -d -dc -dp -e _start -X -T $TB"
	set objs   = "V.o /lib/crt0.o $OBJS"
	set hlibs  = "-lm -lF77 -lI77 -lm -lc"
	set mcode  = 2
	breaksw
case f68881:
	set lflags = "-Bstatic -d -dc -dp -e start -X -T $TB"
	set objs = "V.o /usr/lib/$GCRT0 /usr/lib/Mcrt1.o						-L/usr/lib/$MACH $OBJS"
	set hlibs  = "-lm -lF77 -lI77 -lm -lc"
	set mcode  = 3
	breaksw
case ffpa:
	set lflags = "-Bstatic -d -dc -dp -e start -X -T $TB"
	set objs = "V.o /usr/lib/crt0.o /usr/lib/Wcrt1.o						-L/usr/lib/$MACH $OBJS"
	set hlibs  = "-lm -lF77 -lI77 -lm -lc"
	set mcode  = 4
	breaksw
default:
	set lflags = "-Bstatic -d -dc -dp -e start -X -T $TB"
	set objs   = "V.o /usr/lib/crt0.o /usr/lib/Mcrt1.o						-L/usr/lib/fsoft $OBJS"
	set hlibs  = "-lm -lF77 -lI77 -lm -lc"
	set mcode  = 0
endsw

alias	link "ld -o S.e $lflags $objs $hlibs $PGLIB"
alias	names "(nm -p S.e | egrep 'T [_]?[a-z0-9]+_"'$'"' | fgrep -v -f $OMIT |			sed -e 's+^.* ++' | sort)"

goto $PROC

# Build the shared library and associated runtime files.
# --------------

S.e:
link:
	# Initialize the `objs' working directory.
	echo "initialize the 'objs' working directory"
	if (-e objs) then
	    rm -rf objs
	endif
	mkdir objs

	# Recompile the shlib support objects if necessary.
	if (! -e Slib.o) cc -c Slib.c
	if (! -e zzzend.o) cc -c zzzend.c

	# Construct private version of MALLOC etc. for S.e run standalone;
	# extract standard object and edit the symbol table.

	if (! -e Malloc.o) then
	    if (! -e medit.e) then
		if (`mach` == mc68020) then
		    cc -fsoft medit.c -o medit.e
		else
		    cc medit.c -o medit.e
		endif
	    endif
	    ar x /usr/lib/libc.a malloc.o; mv malloc.o Malloc.o
	    medit.e Malloc.o malloc Malloc realloc Realloc free Free
	endif

	if (! -e $OMIT) then
	    echo "Warning: $OMIT file not found"
	    echo "fiocom" >> $OMIT
	    echo "onenty" >> $OMIT
	    echo "ushlib" >> $OMIT
	    echo "vshend" >> $OMIT
	    echo "vshlib" >> $OMIT
	    echo "xercom" >> $OMIT
	    echo "zcall"  >> $OMIT
	    echo "zfunc"  >> $OMIT
	    echo "zgtenv" >> $OMIT
	    echo "zzstop" >> $OMIT
	    echo "zzstrt" >> $OMIT
	endif

	# Create a dummy transfer vector V.o for linking purposes.
	if (! -e V.o) then
	    echo "vshlib_(){}vshend_(){}" > V.c
	    cc -c V.c; rm V.c
	endif

	# Link a new shared library.  Custom IRAFULIB libraries are supported.
	cd objs
	set noclobber
	foreach i ($libs)
	    foreach j ($dirs)
		set file = $j/$i
		if (-e $file) then
		    break
		endif
	    end
	    echo "prelink $file"
	    ar x $file
	    if (-e __.SYMDEF) then
		rm __.SYMDEF
	    endif
	    foreach j ($EXCL)
		if (-e $j) rm $j
	    end
	    ld -r -o ../$i.o *.o
	    rm *.o
	end
	unset noclobber
	cd ..

	echo "link the shared library"
	link;  if ($PROC == "link") exit 0

names:
	# Generate the external names list for the new shared library.
	echo "generate the name list for the new shared library"
	names > S.nm.new
	if (-e $SNML) then
	    sort $SNML > S.nm.old
	else
	    cp S.nm.new $SNML
	    cp S.nm.new S.nm.old
	endif

	# If any externals present in the old library have been deleted,
	# increment the shlib version number to indicate that old executables
	# much be relinked.  If any new symbols have been added, append these
	# to the end of the name list so that the order of the existing
	# externals is not changed, allowing old executables to be used with
	# the new shared library without relinking.

	if (! -e $SVER) then
	    echo "1" > $SVER
	endif

	set new_version = no
	set version = `cat $SVER`
	comm -23 S.nm.old S.nm.new > S.nm.deleted
	comm -13 S.nm.old S.nm.new > S.nm.added

	if ($PROC == "names") then
	    exit 0
	endif

	if ("`head -1 S.nm.deleted`" != "") then
	    set version = `expr $version + 1`
	    echo $version > $SVER
	    echo "shlib version incremented to $version"
	    echo "deleted externals: `cat S.nm.deleted`"
	    set new_version = yes
	    cp S.nm.new $SNML
	else if ("`head -1 S.nm.added`" != "") then
	    echo "new externals: `cat S.nm.added`"
	    cat S.nm.added >> $SNML
	endif
files:
	if (-e S.s) rm S.s
	if (-e V.s) rm V.s

	# Get the number of symbols in the name list.
	foreach i (`wc $SNML`)
	    set nsymbols = $i
	    break
	end

	# Write out the shared library transfer vector module.  Each external
	# in the shared library has a fixed offset in the transfer vector;
	# the instruction at that offset is a jump to the actual procedure.
	# Memory is allocated as follows: 0x20 byte file header, 0x14 byte
	# transfer vector header, FIO common storage, and then the transfer
	# vector.  The FIO common is allocated the entire first page (8192
	# bytes) of the mapped file.  This first page will be mapped RW even
	# though it is technically part of the text area.  The transfer vector
	# and the remainder of the text area are mapped RO.  The FIO common
	# and the MEM common need to be located at absolute addresses (MEM is
	# at zero) so that they may be referenced in both the client process
	# and in the shared library.

	echo "create the V.s file"
	switch ("`mach`")
	case "i386":
	    echo	'	.file	"V.s"'		>  V.s
	    echo	"	.text"			>> V.s
	    echo	"	.globl	mem_"		>> V.s
	    echo	"	.set 	mem_,	 0" 	>> V.s
	    echo	"	.globl	fiocom_"	>> V.s
	    echo	"fiocom_:"			>> V.s
	    echo	"	.set ., [ . + $FIOCOMSZ ]"	>> V.s
	    echo	"	.globl	xercom_"	>> V.s
	    echo	"xercom_:"			>> V.s
	    echo	"	.set ., [ . + $XERCOMSZ ]"	>> V.s
	    echo	"	.set ., [ . + $PGSZ - $FHSZ - $TOTCOMSZ ]" >> V.s
	    echo	"	.globl	vshlib_"	>> V.s
	    echo	"vshlib_:"			>> V.s
	    echo	"	.long	$version"	>> V.s
	    echo	"	.long	0x$ADDR"	>> V.s
	    echo	"	.long	etext"		>> V.s
	    echo	"	.long	edata"		>> V.s
	    echo	"	.long	end"		>> V.s
	    echo	"	.long	$nsymbols"	>> V.s
	    echo	"	.long	$mcode"		>> V.s
	    echo	"	.long	8"		>> V.s
	    sed -e 's+.*+	jmp	&+' < $SNML	>> V.s
	    echo	"	.globl	vshend_"	>> V.s
	    echo	"vshend_:"			>> V.s
	    breaksw

	case "sparc":
	    echo	'	.seg	"text"'		>> V.s
	    echo	"	.global	_mem_"		>> V.s
	    echo	"	_mem_	= 0"	 	>> V.s
	    echo	"	.global	_fiocom_"	>> V.s
	    echo	"_fiocom_:"			>> V.s
	    echo	"	.skip	$FIOCOMSZ"	>> V.s
	    echo	"	.global	_xercom_"	>> V.s
	    echo	"_xercom_:"			>> V.s
	    echo	"	.skip	$XERCOMSZ"	>> V.s
	    echo	"	.skip	( $PGSZ - $FHSZ - $TOTCOMSZ )" >> V.s
	    echo	"	.global	_vshlib_"	>> V.s
	    echo	"_vshlib_:"			>> V.s
	    echo	"	.long	$version"	>> V.s
	    echo	"	.long	0x$ADDR"	>> V.s
	    echo	"	.long	_etext"		>> V.s
	    echo	"	.long	_edata"		>> V.s
	    echo	"	.long	_end"		>> V.s
	    echo	"	.long	$nsymbols"	>> V.s
	    echo	"	.long	$mcode"		>> V.s
	    echo	"	.long	8"		>> V.s
	    sed -e 's+.*+	set &, %g1;  jmp %g1;  nop+' < $SNML >> V.s
	    echo	"	.global	_vshend_"	>> V.s
	    echo	"_vshend_:"			>> V.s
	    breaksw

	case "mc68020":
	    echo	"	.text"			>> V.s
	    echo	"	.globl	_mem_"		>> V.s
	    echo	"	_mem_	= 0"	 	>> V.s
	    echo	"	.globl	_fiocom_"	>> V.s
	    echo	"_fiocom_:"			>> V.s
	    echo	"	. = ( . + $FIOCOMSZ )"	>> V.s
	    echo	"	.globl	_xercom_"	>> V.s
	    echo	"_xercom_:"			>> V.s
	    echo	"	. = ( . + $XERCOMSZ )"	>> V.s
	    echo	"	. = ( . + $PGSZ - $FHSZ - $TOTCOMSZ )" >> V.s
	    echo	"	.globl	_vshlib_"	>> V.s
	    echo	"_vshlib_:"			>> V.s
	    echo	"	.long	$version"	>> V.s
	    echo	"	.long	0x$ADDR"	>> V.s
	    echo	"	.long	_etext"		>> V.s
	    echo	"	.long	_edata"		>> V.s
	    echo	"	.long	_end"		>> V.s
	    echo	"	.long	$nsymbols"	>> V.s
	    echo	"	.long	$mcode"		>> V.s
	    echo	"	.long	8"		>> V.s
	    sed -e 's+.*+	jmp	&+' < $SNML	>> V.s
	    echo	"	.globl	_vshend_"	>> V.s
	    echo	"_vshend_:"			>> V.s
	    breaksw

	default:
	    echo "unknown machine type `mach`"
	    exit 1
	endsw

	# Write out the shared library object containing the names of all
	# shared library externals, to be linked into each applications
	# program.  Each external is represented in the object (S.o) by the
	# address (i.e., as a symbol) of the corresponding jmp instruction
	# in the transfer vector in the shared library.

set	LOC = `echo 0x$ADDR+0x$PGSZ+0x20=D | adb`

	echo "create the S.s file"
	switch ("`mach`")
	case "i386":
	    echo	'	.file	"S.s"'		>  S.s
	    echo	"	.data"			>> S.s
	    echo	"	.globl	ushlib_"	>> S.s
	    echo	"ushlib_:"			>> S.s
	    echo	"	.long	$version"	>> S.s
	    echo	"	.long	0x$ADDR"	>> S.s
	    echo	"	.long	0"		>> S.s
	    echo	"	.long	0"		>> S.s
	    echo	"	.long	0"		>> S.s
	    echo	"	.long	$nsymbols"	>> S.s
	    echo	"	.long	$mcode"		>> S.s
	    echo	"	.long	8"		>> S.s
	    echo	"	.text"			>> S.s
	    echo	"	.globl	mem_"		>> S.s
	    echo	"	.set 	mem_,	 0" 	>> S.s
	    echo	"	.globl	fiocom_"	>> S.s
	    echo	"	.set 	fiocom_, [ 0x$ADDR+$FHSZ ]" >> S.s
	    echo	"	.globl	xercom_"	>> S.s
	    echo	"	.set	xercom_, [ 0x$ADDR+$FHSZ+$FIOCOMSZ ]" >> S.s
	    echo	"	.globl	vshlib_"	>> S.s
	    echo	"	.set 	vshlib_, [ 0x$ADDR + $PGSZ ]" >> S.s
	    echo	"	.set 	LOC, [ 0x$ADDR + $PGSZ + 0x20 ]" >> S.s
	    sed -e	's/.*/	.globl	&; .set &, [ LOC ]; .set LOC, [ LOC + 5 ]/' < $SNML >> S.s
	    echo	"	.globl	vshend_"	>> S.s
	    echo	"vshend_:"			>> S.s
	    breaksw

	case "sparc":
	    echo	'	.seg	"data"'		>> S.s
	    echo	"	.global	_ushlib_"	>> S.s
	    echo	"_ushlib_:"			>> S.s
	    echo	"	.long	$version"	>> S.s
	    echo	"	.long	0x$ADDR"	>> S.s
	    echo	"	.long	0"		>> S.s
	    echo	"	.long	0"		>> S.s
	    echo	"	.long	0"		>> S.s
	    echo	"	.long	$nsymbols"	>> S.s
	    echo	"	.long	$mcode"		>> S.s
	    echo	"	.long	8"		>> S.s
	    echo	'	.seg	"text"'		>> S.s
	    echo	"	.global	_mem_"		>> S.s
	    echo	"	_mem_	= 0"	 	>> S.s
	    echo	"	.global	_fiocom_"	>> S.s
	    echo	"	_fiocom_ = ( 0x$ADDR+$FHSZ )" >> S.s
	    echo	"	.global	_xercom_"	>> S.s
	    echo	"	_xercom_ = ( 0x$ADDR+$FHSZ+$FIOCOMSZ )" >> S.s
	    echo	"	.global	_vshlib_"	>> S.s
	    echo	"	_vshlib_ = ( 0x$ADDR + $PGSZ )" >> S.s
	    awk		"BEGIN { s = $LOC }"' { printf ("\t.global %s; %s = 0x%x\n", $1, $1, s);  s += 16 }' S.nm.sparc >> S.s
	    echo	"	.global	_vshend_"	>> S.s
	    echo	"_vshend_:"			>> S.s
	    breaksw

	case "mc68020":
	    echo	"	.data"			>> S.s
	    echo	"	.globl	_ushlib_"	>> S.s
	    echo	"_ushlib_:"			>> S.s
	    echo	"	.long	$version"	>> S.s
	    echo	"	.long	0x$ADDR"	>> S.s
	    echo	"	.long	0"		>> S.s
	    echo	"	.long	0"		>> S.s
	    echo	"	.long	0"		>> S.s
	    echo	"	.long	$nsymbols"	>> S.s
	    echo	"	.long	$mcode"		>> S.s
	    echo	"	.long	8"		>> S.s
	    echo	"	.text"			>> S.s
	    echo	"	.globl	_mem_"		>> S.s
	    echo	"	_mem_	= 0"	 	>> S.s
	    echo	"	.globl	_fiocom_"	>> S.s
	    echo	"	_fiocom_ = ( 0x$ADDR+$FHSZ )" >> S.s
	    echo	"	.globl	_xercom_"	>> S.s
	    echo	"	_xercom_ = ( 0x$ADDR+$FHSZ+$FIOCOMSZ )" >> S.s
	    echo	"	.globl	_vshlib_"	>> S.s
	    echo	"	_vshlib_ = ( 0x$ADDR + $PGSZ )" >> S.s
	    echo	"	LOC = ( 0x$ADDR + $PGSZ + 0x20 )" >> S.s
	    sed -e	's/.*/	.globl	&; & = ( LOC ); LOC = ( LOC + 6 )/' < $SNML >> S.s
	    echo	"	.globl	_vshend_"	>> S.s
	    echo	"_vshend_:"			>> S.s
	    breaksw

	default:
	    echo "unknown machine type `mach`"
	    exit 1
	endsw

	if ($PROC == "files") then
	    exit 0
	endif

assemble:
	if (`mach` == sparc) then
	    echo "assemble V.s";  as V.s -o V.o |& grep -v "questionable use"
	    echo "assemble S.s";  as S.s -o S.o |& grep -v "questionable use"
	else
	    echo "assemble V.s";  as V.s -o V.o
	    echo "assemble S.s";  as S.s -o S.o
	endif
	if ($PROC == "assemble") exit 0

relink:
	# Relink the shared library with the new transfer vector.
	echo "relink the shared library with the new transfer vector"
	link;  if ($PROC == "relink") exit 0

# All done with build S.e sequence.
echo "delete the 'objs' working directory"
rm -rf objs
exit 0

# Utilities.
# -------------------

clean:
	# Delete all intermediate files.
	if (-e objs) then
	    rm -rf objs
	endif

	foreach i (V.s S.s S.nm.added S.nm.deleted S.nm.old S.nm.new)
	    if (-e $i) then
		rm -f $i
	    endif
	end

	if ("`find . -name '*.[aoe]' -print | head -1`" != "") then
	    rm -f *.[aoe]
	endif
	exit 0
