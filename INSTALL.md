# Installation Instructions

## Distribution Files

IRAF v2.17 is available from github at

https://github.com/iraf-community/iraf/releases/latest/


## System Requirements and Dependencies

The distributed binaries require the readline or libedit, curl, expat,
and zlib libraries to be installed.

On Debian and its derivatives (Ubuntu, Mint, Devuan, Raspbian etc.):

    $ sudo apt install gcc make flex zlib1g-dev
    $ sudo apt install libcurl4-openssl-dev libexpat-dev libreadline-dev

On Fedora and its derivatives (Redhat, Scientific Linux etc.)

    $ sudo dnf install gcc make perl flex zlib-devel
    $ sudo dnf install libcurl-devel expat-devel readline-devel

On MacOS X, you need to have the XCode tools installed. If you
haven't, you can install them with:

    $ xcode-select --install

Click "Install" to download and install Xcode Command Line Tools.


## Unpack the IRAF Distribution

The source distribution file is built as a tarball with the package
name and version as base directory. Thus, distribution files can be
unpacked with the command

    $ tar zxf /<path>/iraf-2.17.tar.gz
    $ cd iraf-2.17/


## Build from Sources

Now you can configure the system for the proper architecture and build:

    $ make <arch>
    $ make sysgen 2>&1 | tee build.log

For `<arch>`, use the proper IRAF architecture name:

`<arch>`   | Operating system | Supported CPU types
-----------|------------------|---------------------------------------
`linux64`  | Linux 64 bit     | x86_64, arm64, mips64, ppc64, riscv64, alpha
`linux`    | Linux 32 bit     | i386, x32, arm, mips
`macos64`  | macOS 64 bit     | arm64
`macintel` | macOS 64 bit     | x86_64
`macosx`   | macOS 32 bit     | i386
`freebsd64`| FreeBSD 64 bit   | x86_64
`freebsd`  | FreeBSD 32 bit   | i386, arm
`hurd`     | GNU HURD 32 bit  | i386

Note that Cygwin and big endian architectures like macosx/ppc are not
supported anymore.

In the source directory, execute the install script to create needed
links:

    $ ./install 		# execute the install script

The script will prompt you for the path to the default image 
directory, the cache directory and the binary files directory.
Usually, you can everywhere use the default settings when asked from 
the install script.


## Test the Build

IRAF comes with a small set of basic tests to ensure that the build
works fine.  To execute the tests, run:

    $ ./test/run_tests

For details of the tests, see the file [`test/README.md`](test/README.md).
