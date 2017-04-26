Notes file for VMS/IRAF V2.2 Installation
Beginning 16 January 1985 (code prepared on UNIX)
------------------------------------------------

vms/*
	Moved unix/* to vms/*.  (1/16)

vms/os/*
	Moved the current VMS kernel into host$os.  (1/16)

vms/os/mkpkg
	Wrote mkpkg file for OS.  (1/16)

vms/as/*.s
	Replaced all these assembler files by their VMS equivalents.  Deleted
	all the arithmetic VOPS machine files, as they are not worth it on 
	VMS (probably not worth it any more on UNIX, either).  Wrote the new
	routine ACLR.S for VMS.  (1/17)

vms/os/zsvjmp.s
	Moved to AS. (1/17)

vms/os/str.s
	Added _strupk, _strpak, since the VMS kernel references these
	internally, they need to be in LIBOS.  Added the underscore to avoid
	conflict with the OSB (C) versions used by the VOS.  I know it seems
	wasteful to duplicate essentially the same routine in different parts
	of the system, but the subsystem interfaces must be respected.  The
	alternative is to add the routines to the kernel, but this involves
	an expensive interface modification, and I don't think its worth it
	for such trivial little routines.  (1/17)

vms/os/zfiotx.c
vms/os/zfioty.c
vms/os/zfrnam.c
	Changed references to strpak, strupk to _strpak, _strupk.  (1/17)

vms/hlib/*
	Set up the HLIB directory.  This involved minor edits to config.h,
	clpackage.cl, etc., and most of the .cl scripts therein.  Moved in
	all the .com files from the original "vms" directory from V2.0 of
	VMS/IRAF.  Added logical names for TMP and HOST to irafuser.com,
	also logical names for MKPKG, RMBIN, etc.  (1/18)

vms/hlib/libc/iraf.h
	Modified for VMS.  The remaining files are now the same for both
	UNIX/IRAF and VMS/IRAF.  (1/18)

vms/boot/bootlib/*
	Minor changes for VMS.  (1/19-20)

vms/boot/generic/*
	Minor changes for VMS.  (1/20)

vms/boot/mkpkg/*
	Fairly extensive changes for VMS.  Picked up the SCANLIB code from
	the old VMS mkpkg; for the rest (mkpkg/host.c) I just modified the
	new UNIX code as necessary for VMS.  Still using the docvcl() stuff
	to call XC, which is a kludge, but presumably it helps cut down on
	process spawns.  (1/20-21)

vms/boot/mkpkg/scanlib.c,rec.c
	Had second thoughts about this code.  A full but null sysgen must be
	incredibly slow on anything less than an 8600, since the old vms mklib
	would physically open and read each file to map an object file name
	into an object module name to get a date.  Even on the 8600 it is
	probably an order of magnitude slower than the new MKPKG on the UNIX
	750.  Opening the source files with the C i/o can also change the file
	modify dates, and the modify date is valuable information which I hate
	to lose.

	To avoid this dreadful situation, I have decided to not use the VMS
	librarian at all to track object module dates.  Instead, for each
	library there is an associated module date database file, e.g.,
	libsys.mlb for libsys.olb.  This is updated by MKPKG when
	objects are inserted into the library.  If modules are replaced in
	the library at the VMS level, MKPKG will simply recompile those
	modules when eventually it is run (unless a utility program is run
	to update the .mlb dates).  But since the new MKPKG is such a screamer
	anyhow, there is no need to use REPLACE.COM, or whatever, to update
	modules in libraries - and the thing is dangerous anyhow (it is easy
	to put the module in the wrong library, for example).

	This approach has the further advantage that the technique and the
	code are both portable and can be reused on other systems that, like
	VMS, do not manage libraries the way we would like.  (1/21)

vms/boot/rmbin
vms/boot/rtar
vms/boot/wtar
	No changes for VMS, other than to replace the mkpkg.csh by a mkpkg.com.
	(1/22)

vms/boot/xyacc
	Not supported on VMS.  (1/22)

vms/boot/spp/[...]
	Some work was required here for VMS.  The VMS version of RATLIBC was
	picked up wholesale, but the UNIX versions of RATLIB[RF], RPPRAT, and
	RPPFOR were used without change (these contain recent revisions which
	were documented elsewhere).  The VMS version of SPP.C was used (it
	does more), but the UNIX versions XPPMAIN.C, XPPCODE.C, and DECL.c
	were used, since they should be portable, now address all of the
	issues dealt with in the VMS versions, and have recent revisions.
	The VMS version of XC.C was used with very minor revisions.  DCL.C
	(the special ZOSCMD) was moved to BOOTLIB and hooked into os_cmd().

	To summarize, the VMS versions of XC.C, RPP.C, and RATLIBC were used
	with minor changes since they have been extensively modified for VMS.
	The UNIX versions of the remaining files were used since they should
	work for VMS, and it is desirable to minimize the differences between
	the two versions to ease future merges.

	A DCL mkpkg.com was written for each directory under SPP.  These are
	for bootstrap purposes and will always recompile everything.  It is
	assumed that some version of LIBSYS and LIBVOPS is available for
	filename mapping purposes.  (1/22)

vms/os/zfiomt.c
	Removed the device table stuff, since the drive argument to ZZOPMT
	is now the VMS device name.  Density checking is performed at allocate
	time by the VOS.  It appears that the density is not being set
	anywhere.  (1/22)

vms/os/zalloc.c +
vms/os/cltape.c -
vms/os/devtable.c -
	Added the two new kernel routines ZDVALL and ZDVOWN, in the file
	zalloc.c.  They were easy to generate as the original cltape.c code
	was quite well structured.  Deleted the files cltape.c and devtable.c
	since they are no longer used.  (1/22)

-------------------------------------------
That completes the coding modifications for VMS/IRAF, done on UNIX.
System moved to VMS for compile test beginning the 23rd.  (1/22)

(edited the mkpkg.coms, fixed compile time bugs, not worth recording here)

vms/boot/generic/lexyy.c
	Made the same changes for VMS as were made for the Lex/C output of
	xpp.l.  Should add editor script in UNIX version to make these
	changes whenever lexyy.c is generated.  (1/23)

vms/os/zfnbrk.x
vms/os/zfxdir.x
vms/os/zfpath.x
vms/os/zfsubd.x
	Replaced these SPP source files by equivalent C versions, to simplify
	bootstrapping the system.  It is just as easy to work on XCHAR arrays
	in a C program as char arrays, hence there is no advantage to using
	SPP for these routines.  (1/23)

TODO
	Note - ZFNBRK differs from the UNIX version in that it ignores \ in
	filenames (rather than interpreting them as directory delimiters).
	Should merge back into UNIX version, too.  (1/23)

vms/boot/bootlib/osfn2vfn.c
vms/boot/bootlib/vfn2osfn.c
	Added #undefs for fprintf, strcpy to cancel out the defines in libc.h.
	(1/23)

vms/hlib/libc/xnames.h
vms/hlib/libc/xnames.no_
	Added a define for VFNUNMAP.  (1/23)

vms/os/zzstrt.c
vms/hlib/libc/knames.h
vms/hlib/libc/knames.no_
	Discovered that the VMS kernel does not have the entry points _zstartup
	and _zshutdown.  These are used in programs that do not have a ZMAIN.
	They are currently no-ops in both UNIX and VMS, but in general it sure
	seems that they might be needed; the bootstrap utilities would not
	link because the routines were missing.  If we are going to have them
	they should be Fortran callable too, so I changed the names to ZZSTRT
	and ZZSTOP and added them to the VMS kernel (as no-ops).  (1/23)

vms/boot/*
	Replaced all references to _zstartup and _zshutdown by references to
	ZZSTRT and ZZSTOP.  (1/23)

vms/boot/rtar/rtar.c
	Declared getblock() at the head of the file and deleted the local
	definitions in each procedure.  (1/23)

vms/hlib/libc/xnames.h
vms/hlib/libc/xnames.no_
vms/boot/bootlib/vfn2osfn.c
vms/boot/bootlib/envinit.c
	Use of the VOS for filename mapping requires initialization of the
	iraf environment table.  VFN2OSFN will now initialize the env. table
	the first time it is called, taking SET declarations from the
	zzsetenv.def file in hlib.  Added a define for ENVINIT to xnames.h
	to initialize the env data structures; ENVPUTS is called to make
	entries in the table.  The bootlib init procedure is _envinit(). (1/23)

vms/boot/bootlib/osfpathname.c
	Added call to vfn2osfn() before call to ZFPATH.  (1/23)

vms/boot/bootlib/oschdir.c
	Replaced call to os_fpathname() by call to ZFSUBD, since the former
	produces an OS filename rather than an OS directory name, and VMS
	insists on the latter.  (1/23)

vms/boot/hlib/libc/spp.h
	Added a definition OSOK to spp.h.  This is the value a foreign task
	should return for normal successful completion, i.e., 0 for UNIX,
	and 1 for VMS.  (1/23)

vms/boot/mkpkg/host.c
vms/boot/mkpkg/modlist.c
vms/boot/bootlib/*.c
	Lots of changes to the system interface code to get MKPKG to work
	properly on VMS.  (1/23-24)

vms/boot/bootlib/osdir.c
	This code was rewritten to vfnopen the directory at diropen time
	for efficiency reasons.  Debugged the VOS filename mapping interface.
	RMBIN was used to test the OSDIR code.  (1/24)

vms/boot/hlib/libc/libc.h
	Added a #ifdef to turn off LIBC name redefinitions, for programs like
	the bootstrap utilities which need to import_libc, but which want to
	use the host system strcpy, printf, etc.  (1/24)

vms/boot/rtar.c
vms/os/zfiotx.c
	I had a binary file on a supposedly text only tar tape.  The file would
	pass the ZFACSS test and appear to be a text file.  RTAR would try to
	use ZFIOTX to write the output file; the result was an access violation
	in ZPUTTX, presumably because of the binary data. (1/24)

	[not yet resolved]

vms/boot/rmbin				[!!! NASTY BUG !!!]
vms/os/zfdele.c
	Ran into an interesting problem with RMBIN.  This task reads through
	a directory, deleting any binary files found.  The problem is that on
	VMS, while the directory is open for reading, one cannot delete any
	files in the directory because the directory file is locked.  Not only
	does this make RMBIN fail, it will also make a regular file delete,
	create, rename, etc., fail, if the request is issued while another
	process is reading from the directory!!!  This means that bkg jobs
	will mysteriously bomb out at random intervals on a file error. (1/24)

	[not yet resolved]

sys/mkpkg
	Added a $purge lib$ to the end of the sysgen section (after updating
	all the system libraries).  This will purge all the old versions of
	the libraries after a sysgen.  In the first attempt at a full sysgen
	on the 24th, MKPKG went into a loop and filled up a disk, creating
	43 versions of each system library!  (1/25)

sys/mkpkg/pkg.c
	The MKPKG loop mentioned earlier turned out to be due to a bug in the
	VMS C stdio.  FTELL returns the offset of the current record, rather
	than the offset of the next record as the documentation states (and as
	is necessary for UNIX compatibility).  MKPKG would therefore repeat
	the last command after pushing (ftell) and restoring (fseek) its state
	to call a lower level module.  This explains the problem, but does not
	explain how it ever worked as well as it did in the first trial; we
	may not know the whole story yet.

	No choice but to add an #ifdef vms to pop_context() in pkg.c to skip
	forward one line after an FSEEK.  (1/25)

vms/os/net/kutil.c
	Eliminated the reference to the macro define IRAFDIR, which has been
	deleted.  (1/25)

vms/mkpkg
	Added a call at the end to MKPKG to update LIBOS, since the bootstrap
	does not get the networking stuff.  (1/25)

sys/libc/cfnames.c -
	Deleted this file, which was apparently forgotten about after the
	contents were unpacked into separate files.  (1/25)

vms/boot/mkpkg/pkg.c
	As expected, the FTELL/FSEEK problem has not yet been resolved.  After
	running tests for several hours, most of which worked, I was unable to
	isolate the problem.  Simple little test programs always work correctly
	(i.e., they always seek to the wrong line).  I was able to determine
	that when it fails it is because FTELL returns the wrong offset.  It is
	conceivable that MKPKG could be corrupting a stdio data structure, but
	I doubt it.  More likely there is a bug in the VMS FTELL.

	The next attempt at a workaround is to avoid the use of FTELL entirely
	for VMS.  Instead, I wrote a little package k_getc, k_fgets, k_ftell,
	k_fseek.  The getc counts characters, the ftell returns the char
	offset, and the fseek rewinds the bloody file and reads so many
	characters to get to the seek position.  With this change, "mkpkg -n"
	was able to traverse the system successfully.  This code could be
	useful on any system, so I will leave it in with a switch; there is
	little effect on speed.  (1/25)

vms/os/zfiotx.c
	It appears that the RTAR crash when writing binary data into a text
	file was due to the lack of overflow checking in ZPUTTX.  Added
	overflow checking.  (1/25)

------------
Started a full sysgen on 25 January.  This ran to completion, uncovering the
usual assortment of compile time bugs not found on UNIX/IRAF.

NOTE TO VMS/IRAF PROGRAMMERS
	To relink a package executable for debugging on VMS, enter the
	following command to relink with the "-x" flag:

		mkpkg lflags=-x

	For most packages, this will produce a debug version of x_test.e in
	the local directory.  The user package in the IRAF login.cl file
	defines a foreign task "mkdebug" to make this easier:

		task $mkdebug = "$mkpkg lflags=-x"

	After debugging is complete, the following command will relink and
	install a new executable:

		mkpkg update

	Alternatively, you can always type in a long "xc -x ..." command.
	DCL .com files should not be used in the SYS or PKG directories.

sys/mkpkg
	Made compilation of libmain.o optional, under control of a switch in
	hlib$mkpkg.inc.  The VMS XC does not need a separate object module,
	hence it is turned off for VMS.  LIBMAIN is a prelinked object module
	on UNIX, and will eventually be the sharable library on VMS, once we
	figure out how to have MKPKG udpated during a sysgen.  (1/26)

vms/hlib/libcalcomp.a			[SITE DEPENDENT]
	Copied the VMS version of this library into HLIB.  The source for the
	library is local and is not part of IRAF.  To link with this library,
	one must reference the C runtime library CRTLIB.OLB in their login.com
	file.  Other sites may have a different version of the calcomp library,
	or the Versaplot library, any of which will probably do as well for
	the GIO calcomp kernel.  (1/26)

math/llsq/sva.for
	On lines 179, 181, the comma at the end of the line was in column 73,
	hence VMS/FORTRAN would ignore it.  Moved to column 7 of the first
	continuation line.  (1/26)

pkg/dataio/idsmtn/bswap6.c
	Changed the  [include "iraf.h"]  to  [include <iraf.h>].  (1/26)

pkg/images/geometry/geotran.x
	On line 531, changed a couple of '0's to '0.'s in calls to the intrinsic
	function MAX (argument type mismatch).  (1/26)

pkg/local/mkpkg
	Disabled the PERITEK code on VMS.  This is a local, UNIX dependent pkg 
	(written by a staff astronomer) not intended for use outside of NOAO.
	Will compile now only if site=noao and hostid=unix.  (1/26)

pkg/local/intrp.f
	Near the end of file, contained a line that extended way beyond col 72.
	Detabbed the file and made extra continuation for the long line.  Has
	already been updated on UNIX/IRAF (where I did the detab).  (1/26)

pkg/onedspec/identify/idpeak.x
	Argument type mismatch in call to max().  (1/26)

pkg/twodspec/longslit/transform/trtransform.x
	Several array arguments were dimensioned using variables NX,NY defined
	in COMMON.  Changed to ARB to get it to compile (the original is not
	legal Fortran), but passing the array dimensions via common is a more
	serious problem.  (1/26)

pkg/utilities/t_polyfit.x
pkg/utilities/pfregres.f +
	The polyfit task called the Bevington procedure REGRES which implicitly
	called a function with the explicit external name FCTN.  The source
	for FCTN was provided with that for the polyfit task, but the VMS
	librarian would store it in a separate library object module.  Hence,
	at link time REGRES causes a backwards reference to FCTN in the package
	library, but since that library had already been searched the module
	would not be found and the link would fail.

	This could be solved by moving FCTN into a separate object file not
	in the package library, and linking it explicitly by name.  Instead,
	I copied the source for REGRES into the local file pfregres.f, and
	added a reference to FCTN (renamed pf_fctn) in the pf_regres argument
	list as an external.  The real problem here is the Bevington library,
	a simple instructional tool which was never intended to be used in an
	application such as ours.  Coupling to the outside world via a reserved
	external name (FCTN) is as bad as common coupling.  In both cases the
	information should come into the procedure in an obvious way via the
	argument list.  (1/26)

	The new code should be tested.

pkg/imred/vtel/readvt.x
	Assignment to DO variable `subswath' within loop.  Recoded to use a
	FOR loop.  (1/26)

pkg/mkpkg
	Added a "purge [...]" at the end, to be executed only on a vms host.
	(1/26)

-------------
(End of compile time bug fixes)
(All of the above files where also updated in UNIX/IRAF, so that the PKG
(sources are still identical in both systems).


pkg/cl/opcodes.c	[+  globaldef]
pkg/cl/globals.c	[+  globaldef]
pkg/cl/main.c		[+2 globalref]
	I was forced to add #ifdef vms globaldef, globalref nonsense to these
	files to enable a standard XC call to link the CL.  I suspect that
	linking C data-only modules will be a problem on other systems as well;
	in the future, we may want to outlaw data only modules in portable C
	code.  (1/26)

(With this last fix, the new system is fully up and seems to be running
(fine, performing all basic functions normally).

dev/devices
	Edited for VMS.  Decided to use the third field ("aliases" for UNIX)
	to pass the density at device allocation time.  For VMS it appears
	the density must be set at ALLOCATE (mount) time.  (1/26)

vms/hlib/devstatus.cl
	Edited for VMS.   After printing the IRAF status (which is obtained
	by a direct inquiry to VMS, not by just looking for a lock file),
	devstatus will scan the device table dev$devices and do a
	"!show devices/full device" to print the VMS info.  (1/26)

pkg/cl/scan.c
	Would only skip blanks between the tokens; will now skip both spaces
	and tabs.  (1/26)

vms/boot/spp/xc.c
	Modified to actually create a shareable image with no traceback when
	the -n flag is specified (it was a no-op before).  Doesn't seem very
	useful yet (the resultant image cannot be executed or installed),
	but it will be useful for testing purposes.  (1/26)

vms/hlib/devstatus.cl
	Added a boolean param verbose, default no, controlling whether the
	SHOW DEV info is printed.  Since the IRAF device status now comes
	from VMS, we no longer need the VMS output, and it is a lot faster
	without it.  (1/26)

vms/hlib/login.cl
	Added a default NOAO USER package to the default login.cl.  This serves
	as a starter package for each user to modify as they wish.  Each site
	will wish to set up their own default package.  (1/27)

vms/os/open.c
	Modified to provide shared access to files opened for reading, to
	avoid the locked directory problem mentioned earlier.  (1/27)

sys/etc/syserr.x
	In syserrs, added a check for overflow on the error message string.
	These messages do not necessarily come from <syserrmsg>, and can be
	quite long.  (1/27)

sys/ki/kiconnect.x
	Added a call to ki_gethosts() if the node table has not yet been
	read.  There are circumstances (although unusual) in which ki_connect
	can be called before the node table has been read, causing the system
	to try to open the resource on the local node.  (1/27)

(All these bugs I am fixing have been in the system for some time, they
(are not new with this release...)

vms/os/zmain.c
vms/os/zfiopr.c
	Added support for monitoring IPC messages a run time, a useful tool
	for debugging connected subprocesses.  To enable logging of IPC
	communications for a process, the programmer runs the VMS PATCH
	facility to set the value of the global variable debug_ipc to 1.
	The process must first be linked -x (or -M) to get the address of
	the variable debug_ipc.

		cl> mkdebug
		cl> adb x_test
		DBG> ev debug_ipc
		1D58
		DBG> exit
		cl> !patch x_pkg
		PATCH> deposit 1d58=1
		PATCH> update
		PATCH> exit

	This is similar to the way one does this on UNIX.  (1/29)

sys/fmtio/lexnum.x
	Changed stk_ip[] from short to int.  (1/29)

pkg/cl/builtin.c
	Changed two instances of
		
		flags != LT_PFILE;
	to
		flags &= ~LT_PFILE; 	(1/29)

sys/clio/zfiocl.x
	There was some confusion between bytes and chars here, causing the
	numstr[..] = EOS to overwrite the static "xfer" string, resulting in
	the command "x(3,2044)" being sent to the CL.  This would cause
	tasks which read from the standard input to fail.  (1/29)

pkg/system/help/t_help.x
	Added , to the stridxs search for pattern metacharacters, so that
	commands like "help taska,taskb" do not stop after printing only the
	first task.  (1/29)

pkg/images/tv/display/imdopen.x
pkg/images/tv/cv/iism70/imdopen.x
	Added an include <knet.h>.  (1/29)

vms/os/net/zfioks.c
	Modified to not return a channel value of zero, which makes the KI
	think the server has not been connected, even though it has.  (1/29)

Proposed Changes based on recent Installation Experiences
----------------------
The following observations followed a trip to the Univ. of Wash. to install
IRAF and interface the display software to the model 75 IIS.  This served
as a test case to refine our installation procedures before the release.

	- The quota requirements need to be better defined.  In this case
	    the paging file quota was too small, and we had problems spawing
	    connected subprocesses.
	- Make the [iraf.local] directory a standard thing, to hold all
	    local and VMS junk, e.g., login.com, test login.cl and uparm,
	    notes file, sysbull.dat, etc. etc.
	- Replace IMAGEDISK by a definition of the default image storage
	    directory, which need not be a disk device.
	- The symbol IRAF (used to load the logical names) collides with
	    the iraf system login IRAF.  Mail cannot be sent to IRAF, it
	    has to be sent to _IRAF, which is confusing to the user.
	    Solution: use "IRAF" for the system login, [IRAF] for the
	    root directory, and IRAFDEFS for the symbol which used to be
	    called IRAF.
	- Add a new subdirectory off the root called BIN, and install all
	    the executables there.  Modify the CL to read the parameter
	    files from the source directories for the package, rather than
	    constraining the .par files to be in the same directory as the
	    executable.
	- Add a utility to strip the system of all sources, objects, and
	    libraries not needed at runtime or for simple user software
	    development applications.  All machines do not need to have
	    a full copy of the system with all sources.
	- We need to include NSPP/MCVAX in the distribution, so that people
	    can get hardcopy on a Versatec or Printronix.

(end of observations)
-----------------------

vms/gdev/zfiogd.x
	Broke iism70, iism75 into two separate cases, calling two separate
	sets of interface procedures for each device.  (2/5)

vms/gdev/iism75/*
	Added a new subdirectory IISM75 containing the interface procedures
	for the Model 75 IIS.  This device was interfaced without any changes
	at all to the existing high level code, which was written for the
	Model 70.  Instead, the IISM75 procedures perform transformations on
	the data stream to and from the physical device.  M70 output
	instructions are converted to M75 format, and M75 responses are
	converted to M70 format.  In effect, we have added software to convert
	the model 75 back into a 70.  This makes it possible for us to modify
	the complex high level software and test in on the model 70, with a
	reasonable expectation that it will still work on the 75 as well. (2/5)

vms/gdev/iism70/m70io.f
	The .for QIO interface procedures in gdev/iism70 are used for both
	the model 70 and 75 (probably they should be moved into a separate
	directory).  The M70IO procedure would fail with the max transfer
	size set to 32768 bytes.  This was traced to the following expression
	in a QIO call:

		%val(2*count)

	Here, `count' is a short integer variable.  The result of the
	expression is a long integer.  The Fortran compiler would do the
	multiplication with short integer arithmetic and then convert the
	result to long integer, causing the unsigned integer result to overflow
	into the sign bit, causing sign extension and an invalid QIO request
	resulting in a cryptic insufficient working set message from VMS.

	The solution was to store count into a long integer temporary before
	evaluating the expression.  Applying the INT intrinsic function to
	COUNT had no effect.  Note also that one might assume that 2 is of
	type "integer" and that the multiplication would be evaluated using
	integer arithmetic, but that is not how this compiler does it.

	After this modification, the interface still failed, no doubt due
	to the same problem elsewhere in the interface; I did not persue the
	matter any further.  I am documenting this carefully here because it
	illustrates the danger of short integer arithmetic with the VMS
	Fortran compiler, and anyone using such arithmetic must understand
	in detail how such expressions are evaluated.  Short integer arithmetic
	was used in this particular application only because I cribbed the
	QIO level interface routines from somewhere else, and they were
	written to be compiled /I2.  (2/5)

pkg/images/tv/display/iisrcr.x
	Added a 30 millisecond delay, so that cursor loop applications (e.g.,
	to window the display) do not hog the cpu polling the system in an
	infinite loop.  (2/5)

dev/graphcap
	Added an entry for the CIT-414a graphics terminal.  (2/5)

sys/tty/mkpkg
sys/tty/ttyclear.x
sys/tty/ttyclln.x
sys/tty/ttyctrl.x
sys/tty/ttydelay.x
sys/tty/ttygoto.x
sys/tty/ttyputs.x
sys/tty/ttywrite.x
	In the process of developing a graphcap for the CIT-414a, I found
	a problem with the TTY interface.  The interface was set up to
	operate upon EOS delimited control strings.  This would prevent
	inclusion of NULs in the control string (used to generate delays),
	since the NUL would be interpreted as the EOS.  A new routine TTYWRITE
	was added, and TTYPUTS was modified to call STRLEN and then TTYWRITE.
	The remaining routines were modified to call TTYWRITE rather than
	TTYPUTS.  I considered adding a notation to be used in control strings
	to represent delays, but did not do so since the resultant TERMCAP
	entries would no longer be compatible with UNIX TERMCAP.  Instead,
	one must explicitly include the nulls as \0\0\0 in the control strings.
	(2/5)

sys/gio/stdgraph/stdgraph.h
sys/gio/stdgraph/stgencode.x
	Added a new operator !! to the stdgraph encoder, used to generate
	millisecond delays by inserting NUL characters into the output text.
	The delay in milliseconds is popped from the stack.  For example,
	(#20!!) is a 20 millisecond delay.  (2/5)

sys/gio/stdgraph/t_showcap.x
	Modified to use the count returned by the encoder to find the end
	of the output string, rather than assuming that the control string
	is EOS delimited.  This is necessary because the string may contain
	embedded NUL characters.  SHOWCAP will represent these as \0 in
	diagnostic output.  (2/5)

dev/graphcap
	Made a pass through this and made minor revisions to bring the comments
	up to date.  Deleted all the "old graphcap" stuff at the end of the
	file, as the code which used it has all been deleted.  (2/6)

vms/boot/mkpkg/host.c
	Modified h_outcheck() ($checkout) to always check out the latest
	version of a file, even if a local file already exists with the
	same name as the file to be checked out (the old version is deleted
	and a new version is checked out).  (2/6)

vms/boot/mkpkg/tok.c
	Added verbose messages to do_copyfile() and do_movefile().  (2/6)

vms/os/zgtenv.c
vms/hlib/libc/iraf.h
sys$library:iraf.h
	After having what appeared to be problems with the passing of logical
	names to subprocesses spawned by batch queue jobs, I added the logical
	name table lookup code from UNIX/IRAF to the VMS version of ZGTENV.
	This code will fetch the values of the three essential logical names
	IRAF, HOST, or TMP from the file <iraf.h> at runtime, if not found in
	the VMS environment.  These logical names are currently defined as VMS
	logical names in hlib$irafuser.com as well, but these values can be
	deleted should it prove desirable, and the system will automatically
	pick up the default values from <iraf.h>.  This is the way this works
	on UNIX/IRAF too.  (2/6)

vms/boot/boot/*		[UNRESOLVED]
	This is a problem with the bootstrap utilities.  At present, these
	work great when run interactively from either DCL or the CL.
	Even background execution works, provided the subprocess type of
	bkg job is used.  When run in a batch queue, however, my test case
	(MKPKG) apparently completes successfully (an executable is linked),
	but the spool file contains a DCL dump claiming that the MKPKG died
	during startup because it could not find a file (it looks like the
	hlib$zzsetenv.def file, but the RMS error message does not give the
	file name).  No other output is produced.  I looked for a while but
	could not figure out where the real output is going, or why this
	apparently ficticious error output is being generated.  Since the
	MKPKG job apparently completes normally, it must be something during
	job termination, but I don't have time to look into this further
	now.  (2/6)

vms/os/net/zfioks.c
vms/os/net/inet.h
	Reduced the max transfer size to 32256.  A transfer size of 32768 or
	larger causes the Wollongong network driver to crash VMS (this was
	reported before, but the value checked for was still too large). (2/9)

pkg/images/tv/display/iisopn.x
	Added a strupk to unpack devinfo before calling ki_gnode.  (2/9)

pkg/images/tv/display/zsttim.x
	Added code to multiply the max buffer size by 2 if "packit" is set,
	allowing twice as many pixels to be stored in the same space.  (2/9)

vms/os/zgtenv.c
	We don't want to use VMS logical names like "IRAF" and "TMP" because
	problems are likely due to name collisions.  On the other hand, we
	don't want to change the IRAF names.  The solution adopted was to
	have ZGTENV, when asked to lookup NAME, look for IRAFNAME first.
	This allows all logical names to be defined in irafuser.com with the
	IRAF prefix.

	The exact sequence of events followed by ZGTENV when a request for NAME
	is received is the following:

	    [1]	Look in VMS land for IRAFNAME.
	    [2] If NAME is one of "IRAF", "HOST", or "TMP", the three host level
		logical names required by the IRAF VOS, and the VMS lookup for
		"IRAFIRAF", "IRAFHOST", or "IRAFTMP" failed, fetch the default
		value from <iraf.h>.
	    [3] Lastly, simply look in the VMS tables for NAME.

vms/hlib/irafuser.com
vms/hlib/mkiraf.com
	Now defines the following machine dependent logical names:

	    IRAFDISK		locates IRAF root at IRAFDISK:[IRAF]
	    IRAFTMP		default TMP directory (public)

	    fast,batch,slow	aliases for batch queues
	    (SYS$NODE)		not needed for 4.X VMS.

	The remaining definitions in irafuser.com are site independent.
	The old logical IMDIRDISK has been replaced by IRAFTMP.  The latter
	is needed in any case to define the IRAF "tmp$", and it was desirable
	for the default image storage location to be a directory rather than a
	disk.  MKIRAF was modified to create the default user IMDIR as a
	subdirectory of the directory IRAFTMP.  The temporary logical names
	IRAF, HOST, and TMP were replaced by IRAFIRAF, IRAFHOST, and IRAFTMP.
	The logical names EDT and VI were replaced by IRAFEDT and IRAFVI,
	under the assumption that code will be added to EDCAP to define what
	name to pass to the host to run the editor.  It was annoying to have
	an important symbol like VI used by IRAF, and always calling the
	editor via the very slow @irafvi.com script.  (2/11)

	TODO: add "host command" string to EDCAP, modify CL to use it.

VMS Environment
	This is a summary of the VMS environment configuration recommended
	for IRAF.

		IRAF			Symbol, loads irafuser.com
		IRAFDISK		Logical name, disk iraf sources are on
		IRAFTMP			Default public scratch directory

		IRAFDISK:[IRAF]		Root directory of iraf on IRAFDISK
		IRAFDISK:[IRAF.LOCAL]	Login directory for vms user "IRAF".

	If no IMDIR is defined, the CL uses IRAFTMP.  The default IMDIR
	created by MKIRAF is the subdirectory .USER] of IRAFTMP.  The system
	should be installed under the login name IRAF.  IRAF related VMS
	mail should be sent to user IRAF (no need to use _iraf).  (2/11)

pkg/cl/builtin.c
pkg/cl/edcap.c
pkg/cl/globals.c
pkg/cl/eparam.h
	Added a new field EDITOR_CMD to the EDCAP database.  The "escape"
	field contains the command to be sent to the host system to run the
	editor.  The function HOST_EDITOR() accesses edcap and returns the
	command string for the named editor.  (2/11)

dev/edt.ed
dev/emacs.ed
dev/vi.ed
	Added an EDITOR_CMD entry to each table file.  (2/11)

vms/hlib/clpackage.cl
vms/hlib/zzsetenv.def
	Since the CL is an IRAF task run from the host, it reads the default
	zzsetenv.def file during startup.  Since this file already defines
	all the logical directories, I moved the remaining non CL specific
	SET definitions from clpackage.cl to zzsetenv.def.  The clpackage.cl
	file is now machine and site independent, and is one less file to
	have to worry about during an installation.  Since the system package
	is loaded during startup now, I also replaced the fscan stuff, used
	to type the message of the day, by a call to PAGE.  (2/12)

sys/ki/kishownet.x
pkg/system/netstatus.x
pkg/system/system.cl,x_system.x
	Added function ki_shownet (outfd), used to print the network status.
	Added a task NETSTATUS to the system package which simply calls the new
	function and prints the network status as seen by x_system.e. (2/12)

dev/hosts
dev/uhosts
	Updated these tables.  (2/12)

vms/os/zopdpr.c
	This interface routine was modified to give the user more control over
	the priority of "interactive" background jobs (bkg jobs run as
	subprocesses, rather than in a batch queue).  The syntax is as follows:

		& [+|-]N

	where +(-) causes the bkg job to run at a higher(lower) priority than
	the parent, and N is a decimal number.  If neither + or - appears N is
	the absolute priority of the process.

	For example,

		&15		run job at priority level 15
		&-1		run job at a priority one level down

	The default if nothing is specified is to run the job at a priority
	level one less that that of the parent.  (2/12)

vms/os/zgtenv.c
	This procedure was returning ERR if it could not find the name or the
	output string overflowed.  Changed to return 0, as is the standard for
	operators that return string values.  (2/12)

iraf$mkpkg
	Added an "update" entry point to the root mkpkg, so that I don't
	have to run update twice in sys and pkg when relinking the system.
	(2/12)

dev/edt.ed
dev/emacs.ed
	Added the NEXT_PAGE and PREV_PAGE operators as indicated in the ST
	notesfile, received last month in the mail (the VI edcap file was
	updated some time ago).  There was a conflict for EDT; there was a
	duplicate entry for MOVE_START which used the same code as one of
	the new commands (keypad-7).  I solved the problem by deleting the
	duplicate entry.  (2/12)

pkg/cl/builtin.c
	Everywhere there was a reference to the _pname (physical task name)
	for an executable file, added a call to findexe() to get the name
	of the executable which is actually used.  If this is not done,
	things like testing to see if a process is in the cache (prc, flpr)
	fail.  (2/12)

sys/sys.hd
host/os/doc/os.hd
	Changed sys$ references to host$ references to reflect the relocation
	of OS to the HOST directories.  (2/12)

pkg/images/tv/cv/ids/idsfont.x
pkg/images/tv/cv/iism70/idsoptn.x -
	There was a name collision with the routines IDS_OPEN and IDS_OPTN.
	The latter was a no-op in any case, so I deleted it and commented
	out the only reference to it in IDS_FONT.  (2/13)

sys/gio/cursor/grcwaitp.x
	Modified to display the "wait" message again after the user responds
	to the previous "wait" with a space bar, causing the system help
	to be printed.  (2/13)

dev/termcap
dev/graphcap
	Added a few new entries to each file, contributed from our early 
	test sites.  Updated the cached entries.  (2/13)

pkg/language/*
	Extensively revised, edited, corrected, extended, brought up to
	date, added new examples, etc. the manual pages for the CL.
	Most of the changes were minor, but some important new material
	was added.  Manual pages were added for those tasks added since
	the docs were originally written.  A manual page CURSORS was
	added for cursor mode.  The BEGIN and END manual pages were
	deleted since these keywords are merely components of the PROCEDURE
	declaration, not entities in themselves.  (2/12-13)

softools/*.hlp
	Added manual pages for all the softools utilities, including all
	bootstrap utilities (generic, mkpkg, rtar, wtar, rmbin, mkhelpdb,
	hdbexamine, mkmanpage, etc.).  (2/13)

pkg/cl/builtin.c
	Deleted a couple of task placeholder entries for the language package
	and added a couple others, so that "? lan" will prompt with a certain
	set of tasks or functions.  The keywords and common intrinsic fcns
	are excluded.  (2/15)

dev/termcap
	Added a new device entry VMSPRINT, used to queue print jobs to the
	standard VMS print queue SYS$PRINT.  (2/15)

sys/imio/imfort/ +
vms/os/zgcmdl.c +
	Added a new interface IMFORT, the Fortran programmer's interface to
	IRAF images.  This is implemented as the library lib$libfort.a.
	Linking requires also libsys.a, libvops.a, and libos.a.  Documentation
	is in the README file, and a manual is on the way.  The interface is
	minimal at present.

	A new routine, ZGCMDL, was added to the kernel to support IMFORT
	(I also have plans for it for the LIBC interface, to be able to
	emulate main(argc,argv)).  This in available in IMFORT as the
	Fortran callable procedure GCMDLN.  The idea is that the user
	interfaces their Fortran (or C) program to the CL as a foreign task,
	then calls it with arguments like any other task.  The Fortran program
	gets the command line and parses it in memory to get the arguments.
	(1/17)

vms/os/zfiopr.c
	We have been having problems connecting subprocesses, so I added a
	call to _zerror if the create and/or map global section fails when
	setting up IPC, so that hopefully we can determine why.  (2/18)

sys/etc/propcpr.x
	A very minor change, while looking over this code.  Added an errchk
	for the syserr SYS_PROVFL, to prevent a memory overrun in the event
	that there are no more process slots.  (2/18)

vms/hlib/gripes.cl
	Modified the gripes script to send the entire text of the gripe to
	IRAF (as in UNIX/IRAF), rather than just a line stating that a gripe
	has been posted.  On our system, mail to IRAF on any node is
	forwarded to a central node, with the mail file for IRAF on that
	node serving as the central gripes database.  A copy of the gripe
	is still appended to the gripesfile on the local node.  The temporary
	file was moved from TMP to UPARM and the extension ".txt" was added
	to keep VMS mail happy.  The second temporary file for the mail
	message is no longer created, since the osfn() function now allows
	us to pass the host name of the first tempfile to the host mail
	program.  (2/19)

vms/os/zfiopr.c
	Installed a new version of this driver from ST, which should fix
	the IPC connect failure problem we have been experiencing lately.
	ST went though the same thing several weeks ago.  The problem was
	traced to a SECTBLFUL (section table full) problem in VMS.  This is
	fixable by rebuilding VMS with a larger table (param GBLPAGFIL),
	but the new driver will hopefully avoid the problem altogether,
	avoiding the need for sites istalling IRAF to change a system
	parameter.  The recent debug_ipc mods were merged into the new
	driver.  (2/19)

exception handling 	[UNRESOLVED]
	After relinking the system with the new IPC driver from ST, we had
	problems when interrupting subprocesses.  Relinking with the old
	IPC driver helped, but while testing I continued to have problems
	with exception handling.  Nothing I have done in recent history
	should have anything to do with these problems, so I ran the same
	tests on an executable in the old system (untouched for some months).
	Exactly the same thing happened there, so evidently the problem has
	been with us for some time.

	The test is this: I run the x_system.e process standalone, entering
	the command "count files=*.x", and then <ctrl/c> interrupt the
	task while it is running.  Five times out of six, or thereabouts,
	it will work, going through error recovery successfully.  When it
	fails VMS prints the message "improperly handled condition" along
	with a dump of the stack and registers, and does a panic shutdown of
	the process.  This is preceeded by an access violation message.

--------------------------------------------------------------------------
VMS/IRAF V2.2 frozen on 21 Feb.


vms/hlib/mkpkg.e
	Relinked; old executable was using an obsolete version of zgetenv
	that had a bug.  (3/4)

pkg/cl/config.h
	Doubled the size of the stack and increased the dictionary from 20K
	to 30K.  (3/6)

dev/termcap
	Added an entry for device "imagend", the new imagen on node draco,
	plus an alias "imagena" for the original imagen on node aquila. (3/13)

sys/ki/kireceive.x
	Will now echo "out of band" error messages to STDERR, prefixed by
	the node name, before returning ERR on the kernel server channel.
	(3/14)

sys/osb/miiupk16.x
sys/osb/miiupk32.x
	These procedures were modifying their input arguments.  (3/14)

mkpkg, zgtenv.c	[OBSERVATION]
	We are running with two different versions of the system, IRAF and
	IRAFX.  When I linked a process with mkpkg from the IRAFX CL I found
	that the old IRAF libraries were being used.  The pathnames in the
	global <iraf.h>, read by ZGTENV when it cannot find IRAF etc. in
	the VMS tables, were still set for the old system.  After editing
	<iraf.h> and repeating the mkpkg, the IRAFX libraries were accessed.
	This proves that indeed the logical names are NOT always getting passed
	down through all the process spawns, and the new ZGTENV facility is
	doing its job.  (3/14)
