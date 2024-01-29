# Installation Instructions

## Distribution Files

IRAF v2.17.1 is available from github at

https://github.com/iraf-community/iraf/releases/latest/


## System Requirements and Dependencies

The distributed binaries require the readline or libedit, curl, expat,
and zlib libraries to be installed.

On Debian and its derivatives (Ubuntu, Mint, Devuan, Raspbian etc.):

	$ sudo apt install gcc make bison flex zlib1g-dev
	$ sudo apt install libcurl4-openssl-dev libexpat-dev libreadline-dev

On Fedora and its derivatives (Redhat, Scientific Linux etc.)

	$ sudo dnf install gcc make perl flex bison zlib-devel
	$ sudo dnf install libcurl-devel expat-devel readline-devel

On MacOS X, you need to have the XCode tools installed. If you
haven't, you can install them with:

	$ xcode-select --install

Click "Install" to download and install Xcode Command Line Tools.


## Unpack the IRAF Distribution

The source distribution file is built as a tarball with the package
name and version as base directory. Thus, distribution files can be
unpacked with the command

	$ tar zxf /<path>/iraf-2.17.1.tar.gz
	$ cd iraf-2.17.1/


## Build from Sources

Now you can compile IRAF on your system with the command

	$ make 2>&1 | tee build.log

The following IRAF architectures are supported:

Architecture | Operating system | Supported CPU types
-------------|------------------|---------------------------------------------
`linux64`    | Linux 64 bit     | x86\_64, arm64, mips64, ppc64, riscv64, alpha, loongarch64
`linux`      | Linux 32 bit     | i386, x32, arm, mips
`macos64`    | macOS 64 bit     | arm64
`macintel`   | macOS 64 bit     | x86\_64
`macosx`     | macOS 32 bit     | i386
`freebsd64`  | FreeBSD 64 bit   | x86\_64
`freebsd`    | FreeBSD 32 bit   | i386, arm
`hurd`       | GNU HURD 32 bit  | i386

Note that Cygwin and big endian architectures like macosx/ppc are not
supported anymore.

## Test the Build

IRAF comes with a small set of basic tests to ensure that the build
works fine. To execute the tests, run:

	$ make test

The output should look like

	ecl.e: README.md
	ecl.e: files.md ......
	ecl.e: images.imcoords.md ...........
	ecl.e: images.imfilter.md .x............
	ecl.e: images.imfit.md ...
	ecl.e: images.imgeom.md ........
	ecl.e: images.immatch.md ..
	ecl.e: lists.md .......
	ecl.e: noao.astutil.md ........
	ecl.e: noao.digiphot.photcal.md .
	ecl.e: numerical-recipes.md ..........
	ecl.e: os.md ..
	ecl.e: programming.md ............
	ecl.e: sys.vops.md .
	ecl.e: test-syntax.md ...xs.
	ecl.e: testproc.md ..........................
	ecl.e: utilities.nttools.md ..............
	Test summary:  128 passed
	   1 skipped
	   2 xfailed

Note that `xfailed` (`x`) are e**x**pected **fail**ures, which one
does not neet to worry about.

For details of the tests, see the file [`test/README.md`](test/README.md).

## Install the software at its final place

There are two options to install the build: system wide (by default
under `/usr/local/lib/iraf`), or in-place for the current user
only. If you have root (sudo, admin) access, the system wide
installation is the preferred option.

### System wide installation

The system wide installation copies everything that is needed to
`/usr/local/lib/iraf`, making it available for all users of the
computer. For this, do the following command:

	$ sudo make install

This also installs the links required to run iraf or its commands to
`/usr/local/bin`. After the installation finishes, one can directly
start using

	$ mkiraf
	$ ecl

Note that the `mkiraf` command is now optional if a local parameter
storage is not needed. Setting the `iraf` environment variable is not
needed to run the system.

### In-place installation

The in-place installation just configures the built system in its
current location.

	$ make inplace

This also installs the links required to run iraf or its commands to
`~/.iraf/bin`. This directory should be added to your `PATH` variable
so that the IRAF commands can be found by their name. Different to
previous versions, the installation does not touch your login files;
you should edit these files manually to add the path if
needed. Setting the `iraf` environment variable is not needed to run
the system.

## Customization variables

### Build customization

The build using **make** can be customized by a number of environment
variables:

 * **CC** - the C compiler to use (default: **cc**, which is the
   standard C compiler of the system). gcc and clang are both known to
   work.

 * **CFLAGS** - C compiler flags (default `-g -O2`). To
   build a 32-bit IRAF on a 64-bit system, add `-m32`.

 * **CPPFLAGS** - C preprocessor flags (default: empty).

 * **LDFLAGS** - Linker flags (default: empty). To
   build a 32-bit IRAF on a 64-bit system, add `-m32`.

 * **IRAFARCH** - IRAF architecture string. This is used as suffix for
   the binary directory names (i.e. `bin.linux64`, `unix/bin.linux64`,
   `noao/bin.linux64`) If not set, the IRAF architecture is determined
   automatically from the system. If set to an empty string `''`, the
   binary directories are just named `bin`, `unix/bin` and
   `noao/bin`. Note that external packages need to be adjusted to also
   allow suffix-less bin directory names when build in this case.

### Installation customization

The installation using **make install** recognizes the following
environment variables:

 * **prefix** - Common installation prefix (default: `/usr/local`). The
   installation goes to `${prefix}/lib/iraf`, with symlinks for the
   user-callable executables in `${prefix}/bin`, and the manpages
   installed in `${prefix}/share/man`. The `iraf` variable is set to
   `${prefix}/lib/iraf/`.

 * **DESTDIR** - directory name prepended to each installed target
   file. This is useful for package creators, where the files are not
   installed by **make** in their finaly installation.
