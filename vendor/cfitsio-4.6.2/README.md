# CFITSIO Interface Library

CFITSIO is a library of ANSI C routines for reading and writing FITS format data files. A set of Fortran-callable wrapper routines are also included for the convenience of Fortran programmers.  This README file gives a brief summary of how to build and test CFITSIO, but the latest and most complete information may be found in the "docs" folder in the CFITSIO User's Guide:

### User Guides for C programmers
* `cfitsio.tex` (LaTeX source file)
* `cfitsio.ps`  (PostScript file)
* `cfitsio.pdf` (Portable Document Format)

### User Guides for Fortran programmers
* `fitsio.tex` (LaTeX source file)
* `fitsio.ps`  (PostScript file)
* `fitsio.pdf` (Portable Document Format)

### Quick Start Guide
* `quick.tex` (LaTeX source file)
* `quick.ps`  (PostScript file)
* `quick.pdf` (Portable Document Format)

## Building CFITSIO

The CFITSIO code (contained in `*.c` source files and several `*.h` header files) should compile and run on most Unix platforms without modification. The standard way to build the library on Unix systems is the usual GNU-like approach, i.e. by first typing

```bash
% ./configure  [--prefix=/target/installation/path]
```

at the operating system prompt.  Type `./configure` and not simply `configure` to ensure that the configure script in the current directory is run and not some other system-wide configure script. The optional `prefix` argument to configure gives the path to the directory where the CFITSIO library and include files should be installed via the later `make install` command. For example,

```bash
% ./configure --prefix=/usr1/local
```

will cause the later `make install` command to copy the library file(s) to `/usr1/local/lib` and the necessary header files to `/usr1/local/include` (assuming of course that the process has permission to write to these directories).

All the available configure options can be seen by entering the command

```bash
% ./configure --help
```

The configure command customizes the Makefile for a particular system, so after it has been run, type

```bash
% make
```

at the prompt, and this will compile the source files and build the library (static `libcfitsio.a` as well as the shared version `libcfitsio.so|.dylib`) and the helper utilities (`fpack`, `funpack`, `fitscopy`, `imcopy`, et al.) and test program (`testprog`).  To copy the library, header files, and utilities to the chosen install location, type this command:

```bash
% make install
```

When installing in /usr/local on Linux and some other systems, it may be necessary to rebuild the linker cache by running:

```bash
% sudo ldconfig
```

Alternatively, the library and utilities may be built on many systems using the CMake program.  Specific instructions for using CMake on Windows platforms can be found in the `README.win` file, but for Unix systems (e.g., Linux or macOS) the procedure should be similar to the following:

While in the CFITSIO source code directory:

```bash
% mkdir cmbuild
% cd cmbuild
% cmake -G "Unix Makefiles" ..
% cmake --build .
% cmake --install . [--prefix /usr/local]
```

Where the final step uses an optional installation prefix.

Additional options for installing CFITSIO on macOS via third-party package managers or the XCode GUI can be found in the `README.MacOS` file.


## Testing CFITSIO

The CFITSIO library may be tested by building and running the `testprog.c` program that is included with the release (in the `utilities` folder). On Unix systems, type:

```bash
% make testprog
% ./testprog > testprog.lis
% diff testprog.lis testprog.out
% cmp testprog.fit testprog.std
```

The `testprog` program should produce a FITS file called `testprog.fit` that is identical to the `testprog.std` FITS file included in this release.  The diagnostic messages (which were piped to the file `testprog.lis` in the Unix example) should be identical to the listing contained in the file `testprog.out`. The `diff` and `cmp` commands shown above should not report any differences in the files.

## Using CFITSIO

The CFITSIO User's Guide, contained in the files mentioned above, provides detailed documentation about how to build and use the CFITSIO library. It contains a description of every user-callable routine in the CFITSIO interface.

The `cookbook.c` file in the utilities folder provides some sample routines for performing common operations on various types of FITS files. Programmers are urged to examine these routines for recommended programming practices when using CFITSIO. Users are free to copy or modify these routines for their own purposes.

Any problem reports or suggestions for improvements are welcome and should be sent to the CFITSIO/CCFITS help desk at:

[ccfits@heasarc.gsfc.nasa.gov](mailto:ccfits@heasarc.gsfc.nasa.gov)

-------------------------------------------------------------------------
William D. Pence
HEASARC, NASA/GSFC
