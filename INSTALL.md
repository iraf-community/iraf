# Installation Instructions

## Distribution Files

The IRAF v2.16.1 snapshots are available from github at

https://github.com/iraf-community/iraf/releases/latest/

The snapshot has the release date as a suffix in the version number
and in the file name.


## System Requirements and Dependencies

The distributed binaries require the readline or libedit, curl, and
expat libraries to be installed. For full functionality and for
installation from source, additionally the C compiler, the make
program, flex, and the development versions are required.

On Debian and its derivatives (Ubuntu, Mint, Devuan, Raspbian etc.):

    $ sudo apt install libcurl4-openssl-dev libexpat-dev libreadline-dev
    $ sudo apt install gcc make flex

On Fedora and its derivatives (Redhat, Scientific Linux etc.)

    $ sudo dnf install libcurl-devel expat-devel readline-devel
    $ sudo dnf install gcc make perl flex

On MacOS X, you need to have the XCode tools installed to build from
source. If you haven't, you can install them with:

    $ xcode-select --install

Click "Install" to download and install Xcode Command Line Tools.


## Install a Binary Distribution

The binary distribution file is built as a tarball of the toplevel
IRAF directory, i.e. they should be unpacked in a directory the user
creates. Thus, distribution files can be unpacked with the command

    $ mkdir iraf.v2161
    $ cd iraf.v2161
    $ tar xzf /<path>/iraf.<arch>.tar.gz

To install IRAF for personal use, execute the install script:

    $ ./install

Answer the prompts, in most cases simply accepting the defaults will
be all that is needed.

To install IRAF as root for system wide use, execute the install
script with root permissions:

    $ sudo ./install --system

This will create a system installation of IRAF for all users of the
machine.  Root permissions are required in order to write to system
directories.

IRAF is then immediately available by typing `cl`.


## Build from Sources

The source distribution file is built as a tarball with the package
name and version as base directory. Thus, distribution files can be
unpacked with the command

    $ tar zxf /<path>/iraf-2.16.1-2018.06.15.tar.gz
    $ cd iraf-2.16.1-2018.06.15/

In the source directory, execute the install script to create needed
links:

    $ ./install 		# execute the install script

Usually, you can everywhere use the default settings when asked from
the install script.  Configure the system for the proper architecture
and build:

    $ make <arch>
    $ make sysgen 2>&1 | tee build.log

For `<arch>`, use the proper IRAF architecture name:

`<arch>`   | Operating system | CPU
-----------|------------------|---------------------------------------
`linux64`  | Linux 64 bit     | x86_64, arm64, mips64, ppc64, riscv64
`linux`    | Linux 32 bit     | i386, x32, arm, mips
`macintel` | Mac OS X 64 bit  | x86_64
`macosx`   | Mac OS X 32 bit  | i386
`freebsd64`| FreeBSD 64 bit   | x86_64
`freebsd`  | FreeBSD 32 bit   | i386, arm
`hurd`     | GNU HURD 32 bit  | i386

Note that Cygwin and big endian architectures like macosx/ppc are not
supported anymore.


## Test the Build

IRAF comes with a small set of basic tests to ensure that the build
works fine.  To execute the tests, run:

    $ ./test/run_tests

For details of the tests, see the file [`test/README.md`](test/README.md).
