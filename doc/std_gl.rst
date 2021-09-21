=====================
Standard Nomenclature
=====================

AAS 
  The American Astronomical Society.

C 
  C is a powerful modern language for both systems and general
  programming. C provides data structuring, recursion, automatic
  storage, a fairly standard set of control constructs, a rich set of
  operators, and considerable conciseness of expression. Developed by
  Ken Thompson and Dennis Ritchie at Bell Labs in the early 1970’s, C
  is the language used to implement the kernel of the UNIX operating
  system, as well as the standard UNIX utilities.

CL 
  The IRAF Command Language. The CL is an interpreted language
  designed to execute external **tasks**, and to manage their
  **parameters**. The CL organizes tasks into a hierarchical structure
  of independent **packages**. Tasks may be either **script tasks**,
  written in the CL, or compiled **programs**, written in the SPP
  language, and linked together to form **processes**. A single
  process may contain an arbitrary number of tasks.

  The CL is itself both a task and a process. The CL process runs
  concurrently with the subtasks which it executes. The CL process and
  the process containing the subtask being executed communicate
  dynamically via interprocess communication, providing both a highly
  interactive mode of execution, as well as a batch mode.

  The CL provides **redirection** of all i/o streams, including
  graphics output and cursor readback. Other facilities include
  **menus**, **command logging**, parameter **prompting**, an online
  **help facility**, a "programmable desk calculator" capability, and
  a **learn mode**. New packages and tasks are easily added by the
  user, and the CL environment is maintained in the user’s own
  directories, providing continuity from session to session.

CLIO 
  CL I/O. A package of SPP callable library routines, used to
  communicate with the CL.

FIO 
  File I/O. A package of SPP callable library routines, used to access
  files.

FITS
  A "Flexible Image Transport System". FITS is a standard data format
  used to transport images (pictures) between computers and
  institutions.  Developed in the late 1970s by Donald Wells (KPNO)
  and Eric Greisen (NRAO), the FITS standard is now widely used for
  the interchange of image data between astronomical centers, and is
  officially sanctioned by both the AAS and the IAU.

FMTIO
  Formatted I/O. A package of SPP callable library routines, used to
  perform formatted i/o (decoding and encoding of character strings).

Fortran
  As the most widely used language for scientific computing for the
  past twenty years, Fortran needs no introduction. Fortran is used in
  the IRAF system as a sort of "super assembler" language. Programs
  and procedures written in the IRAF SPP language are mechanically
  translated into a highly portable subset of Fortran, and the Fortran
  modules are in turn translated into object modules by the host
  resident Fortran compiler.  Existing numerical and other modules,
  already coded in the Fortran language, are easily linked with
  modules written in the SPP language to produce executable
  programs. The IRAF system and applications software does not use any
  Fortran i/o; all i/o facilities are provided by the IRAF **program
  interface** and **virtual operating system**.

GIO
  Graphics I/O. A package of SPP callable library routines, used to
  interface to graphics and grayscale devices.

IAU
  The International Astronomical Union.

IMIO
  Image I/O. A package of SPP callable library routines, used to access
  imagefiles (bulk data arrays).

IRAF
  The IRAF or "Image Reduction and Analysis Facility", consists of a
  virtual operating system, a command language, a general purpose
  programming language (which was developed especially for IRAF), a
  large i/o library, a numerical library, and numerous support
  utilities and scientific applications programs. The system is
  designed to be transportable to any modern superminicomputer. When
  completed, the system will provide extensive facilities for general
  image processing, astronomical data reduction and analysis,
  scientific programming, and general software development.

Lroff
  The Lroff text formatter is part of the portable IRAF system. The
  online **help** facilities use Lroff, and hence manual pages and
  other online documentation must be maintained in Lroff form. The
  Lroff text formatter is patterned after the UNIX *Troff* text
  formatter.

MTIO
  Magnetic Tape I/O. A package of SPP callable library routines, used to
  read and write magnetic tapes.

Make
  Make is a UNIX utility program, used to compile and link (make)
  programs. Make takes as input a human readable *Makefile* which
  describes the interdependencies of the modules in the package, as
  well as giving exact instructions for making each module. When
  making a target module, Make recompiles only those modules which
  have been changed since the target was last made. A simple command
  like "make all" usually suffices to make all of the modules in a
  package.

  Make is most useful on UNIX systems. Even on non-UNIX systems,
  however, the makefile for the package is useful documentation, for
  it describes precisely how to make each module in the package.

Mklib
  Mklib is a UNIX dependent utility developed for IRAF. Mklib is
  analogous to Make, except that Mklib is used to maintain
  libraries. Mklib checks each module in a library to see if it is up
  to date, and if not, recompiles the module and installs the new
  object module in the library.  Mklib is used by the IRAF Sysgen
  utility to automatically update the IRAF system (consisting of four
  libraries containing several hundred modules).

OS 
  (1) An acronym for the term "operating system". (2) The OS interface
  package, which contains the machine dependent routines required to
  interface the portable IRAF i/o packages to the local operating
  system.

OSFN 
  An acronym for the term "OS dependent file name".

SPP 
  The IRAF Subset Preprocessor Language (SPP), implements a subset of
  the full language scheduled for development in 1984. The SPP
  language is a general purpose language, patterned after Ratfor
  and C. The language provides advanced capabilities, modern control
  constructs, enhanced portability, and support for the IRAF runtime
  library (CL interface, etc.).

Troff 
  Troff is the UNIX text formatter. In IRAF documentation, *Troff* is
  always used in conjunction with the "ms" macro package.

UNIX 
  An operating system developed at Bell Labs in the early 1970s by Ken
  Thompson and Dennis Ritchie. Though originally developed for the
  PDP11, UNIX is now available on a wide range of machines, ranging
  from micros to superminis and mainframes. UNIX is the software
  development system for the IRAF project.

VFN 
  An acronym for the term "virtual file name". A virtual file name is
  a machine independent filename of the form "ldir$root.extn".

VMS 
  The native operating system for Digital Equipment Corporation’s VAX
  series of supermini computers.

VOPS 
  The "vector operators" package, a package of SPP callable library
  routines providing a wide class of vector pseudo-instructions. The VOPS
  routines are written in the SPP language, but may be optimized in
  assembler or interfaced directly to an array processor, depending upon
  the implementation.
  
band 
  The Nth band of a three dimensional array or image is denoted by the
  subscript [∗,∗,N], where ∗ refers to all the pixels in that dimension. A
  band is a two dimensional array.
  
binary file 
  A binary file is an array or sequence of **chars**, where the term char
  defines a unit of storage, and implies nothing about the contents of the
  file. Data is transferred between a binary file and a buffer in the
  calling program by a simple copy operation, without any form of
  conversion. Binary files are created, deleted, and accessed via the
  routines in the FIO interface. Barring device restrictions, binary files
  may be accessed at random, and extended indefinitely. Almost any device
  may be accessed as a binary file via FIO.
  
binary operator 
  An operator which combines two operands to produce a single result
  (i.e., the addition operator in "x + y").
  
brace 
  The left and right braces are the characters "{" and "}". Braces are
  used in the CL and in the SPP language to group statements to form a
  compound statement.
  
bracket 
  The left and right square brackets are the characters "[" and "]".
  Brackets are used in the SPP language to form array subscripts.
  
byte 
  The **byte** is the smallest unit of storage on the host machine. The
  IRAF system assumes that there are an integral number of bytes in a
  **char** and in an address increment (and therefore that the byte is not
  larger than either). On most modern computers, a byte is 8 bits, and a
  char is 16 bits (INTEGER∗2). If the address increment is one byte, the
  machine is said to be **byte addressable**. Other machines are **word
  addressable**, where one word of memory contains two or more bytes. In
  the SPP language, SZB_CHAR gives the number of bytes per char, and
  SZB_ADDR gives the number of bytes per address increment.
  
char 
  The **char** is the smallest signed integer which can be directly
  addressed by programs written in the SPP language. The char is also the
  unit of storage in IRAF programs: the sizes of objects are given in
  units of chars, and binary files and memory are addressed in units of
  chars. Since the SPP language interfaces to the machine via the local
  Fortran compiler, the Fortran compiler determines the size of a char. On
  most systems, the datatype **char** is equivalent to the (nonstandard)
  Fortran datatype INTEGER∗2.
  
column 
  The Nth column vector of a two dimensional array or image is denoted by
  the subscript [N,∗], where ∗ refers to all the pixels in that dimension.
  The Nth column of the Mth band of a three dimensional array or image is
  denoted by [N,∗,M].
  
compiler 
  A compiler for a language X is a program which translates a **source
  module** written in the language X into an **object module**. A
  **linker** subsequently combines a number of object modules to produce
  an executable **process**.
  
coupling 
  Coupling measures the strength of relationships between modules. The
  independence of modules is maximized when coupling in minimized. A
  change in one module is least likely to require a change in another
  module when the two modules are minimally coupled.
  
data structure 
  A data structure is an aggregate of two or more data elements. Examples
  include arrays, descriptors, files, records, linked lists, trees,
  graphs, and so on.
  
database management 
  Database management is a branch of computing science concerned with
  techniques for implementing, maintaining, and accessing databases.
  Databases may be used to store arbitrarily complex data objects. A
  database is self describing and self contained. Access to a database
  typically occurs only through well defined interfaces, which ideally
  provide a high degree of **data independence** (the external world knows
  no more than needed about the contents of the database, or how data is
  stored in the database).
  
  Applications programs communicate with one another via records passed
  through the database, as well as save final results in the database. A
  general purpose query language can be used to inspect and manipulate the
  contents of a database.
  
datafile 
  A datafile is a database storage file. Datafiles are used to store
  program generated **records** or descriptors, containing the results of
  the analysis performed by a program. Datafile records may be the final
  output of a program, or may be used as input to a program.
  
field 
  A field is an element of a structure or record. Each field has a name, a
  datatype, and a value.
  
function 
  A function is a procedure which returns a value. Functions must be
  declared before they can be used, and functions must only be used in
  expressions. It is illegal to **call** a function.
  
header file 
  A header file is a file (extension ".h") containing only defined
  constants, structure definitions, macro definitions, or comments. Header
  files are included in other files by referencing them in **include**
  statements, and are not directly compiled.
  
identifier 
  An identifier is a sequence of characters used to name a procedure,
  variable, etc. in a compiled language. In the SPP language, an
  identifier is an upper or lower case letter, followed by any number
  (including zero) of upper or lower case letters, digits, or instances of
  the underscore character.
  
image 
  An array of arbitrary dimension and datatype, used for bulk data
  storage. An image is an array of **pixels**.
  
imagefile 
  The form in which images are implemented in the IRAF system. The IRAF
  currently supports images of up to seven dimensions, in any of eight
  different datatypes. Only **line storage mode** is currently available.
  The "imagefile" structure is actually implemented as two separate files,
  the **image header file** and the **pixel storage file**.
  
include file 
  An "**include** *<include_file_name>*" statement in the SPP language is
  replaced during compilation by the contents of the named include file
  (the contents of the include file are inserted into the input stream).
  
interface 
  The interface to a module is *defined* by the **external
  specifications** of the module. The *actual* interface to a module is
  everything that is known about the module by other modules in the
  system. The interface to a subroutine library, for example, is defined
  by the manual pages, reference manuals, and other formal documentation
  for the library.
  
line 
  The Nth line of a two dimensional array or image is denoted by the
  subscript [∗,N], where ∗ refers to all the pixels in that dimension. The
  Nth line of the Mth band of a three dimensional array or image is
  denoted by [∗,N,M].
  
list file 
  A list file is a text file, each line of which is a record containing
  one or more fields. Each record in the list has the same format, though
  not all fields need be present (fields can only be omitted from right to
  left).
  
macro 
  A macro, or **inline function**, is a function with zero or more
  arguments, which is expanded by text substitution during the
  preprocessing phase of compilation.
  
newline 
  The newline character (’\n’) delimits each line of text read by the FIO
  input procedures. If a text file is read character by character, a
  single newline character marks the end of each line, and the special
  character EOF marks the end of the file. Newline is logically equivalent
  to a carriage return followed by a line feed.
  
operand 
  An operand is a data object which is operated upon by an operator,
  procedure, or task. Operands may be either input or output, or both.
  
package 
  A package is a set of modules which operate on a specific **abstract
  datatype**. The modules in a package may be either procedures or tasks.
  Examples of abstract datatypes include the CL, the file, the imagefile,
  and so on. Some packages are merely collections of modules which are
  logically related (i.e., the class of system utilities).
  
parameter 
  An externaly supplied argument to a module which directly controls the
  functioning of the module.
  
pathname 
  An absolute OS dependent filename specification, i.e, a filename which
  is not an offset from the current directory.
  
pixel 
  The fundamental unit of storage in an image; a picture element. An image
  is an array of pixels.
  
pointer 
  A pointer is a datum which defines the coordinates of an object in some
  logical coordinate system. To use a pointer, one must know what type of
  object the pointer points to, and what coordinate system the pointer
  references.
  
portable 
  A program is said to be **portable** from computer A to computer B if it
  can be moved from A to B without change. A program is said to be
  **transportable** from computer A to computer B if the effort required
  to move the program from A to B is much less than the effort required to
  write an equivalent program on machine B from scratch.
  
preprocessor 
  A preprocessor is a program which transforms the text of a source file
  prior to compilation. A preprocessor, unlike a compiler, does not fully
  define a language. A preprocessor transforms only those constructs which
  it understands; all other text is passed on to the compiler without
  change.
  
procedure 
  A separately compiled program unit. The procedure is the main construct
  provided by languages for the *abstraction of function*. The external
  characteristics of a procedure are its name, argument list, and optional
  return value.
  
process 
  An executable partition of memory in the host computer. The host OS
  initiates a process by copying or mapping an executable file into main
  memory. In a multitasking, multiuser system, a number of processes will
  in general be simultaneously resident in main memory, and the processor
  will execute each in turn, performing many **context switches** each
  second with the result that all processes appear to be executing
  simultaneously.
  
program 
  A program is a compiled procedure which is called by the CL, via the CL
  interface. The procedure must be referenced in a **task** statement
  before it can be accessed by the CL, and must not have any formal
  arguments. A program communicates with the CL via CLIO. An arbitrary
  number of programs may be linked to form a single **process**.
  
program interface 
  The interface between an applications program and the outside world. The
  program interface is subdivided into a number of **packages**, each of
  which has a well defined interface of its own. The specifications of the
  program interface are summarized in the program interface **crib
  sheet**.
  
record 
  A record is data structure consisting of an arbitrary set of fields,
  used to pass information between program modules, or to permanently
  record the results of an analysis program in a **database**. Often,
  records are organized into arrays, where each record contains the
  results of the analysis of a particular object.
  
script task 
  An interpreted program written in the command language. A script task,
  like a compiled program, may have formal parameters and local variables.
  A script task may call another task, including another script task, but
  may not call itself. To the caller, script tasks and compiled programs
  are equivalent.
  
specifications 
  A detailed description of a software system or subsystem, concentrating
  on the external attributes of the software rather than the on the
  implementation. **Requirements** are similar to specifications, but are
  usually more formal and less detailed. The specifications for a
  subsystem define the interface to the subsystem, and when written in an
  informal style may resemble a reference manual.
  
system interface 
  The interface between the portable IRAF software and the host operating
  system. The system interface is a **virtual operating system**. The
  system interface routines, maintained in the "OS" package, are in
  principle the only part of a system that needs to be changed when
  porting the system to a new computer.
  
task 
  A CL callable program unit. CL tasks may be script tasks, external
  programs, or compiled procedures which are built in to the CL.
  
task statement 
  (1) The **task** statement in the SPP language defines a list of
  programs to be linked together to form a single process. (2) The CL
  **task** statement enters the name of a task in the dictionary, defines
  the type of task, and in the case of a compiled task, the name of the
  process in which it resides.
  
text file 
  A file which contains only text (character data), and which is
  maintained in the form expected by the text processing tools of the host
  OS.
  
unary operator 
  An operator which operates on a single operand, i.e., the minus sign in
  the expression "−x", or the boolean complement operator in the
  expression "!x".
  
virtual memory 
  If the address space of a process exceeds the amount of physical memory
  which the process can directly address, the process is using virtual
  memory. The virtual address space is organized into a series of fixed
  size **pages**. The amount of physical memory available to a process is
  known as the **working set** of a process. Pages which are not **memory
  resident**, i.e., not in the working set, reside on some form of backing
  store, usually a disk file. When a page is referenced which is not in
  the working set, a **page fault** occurs, causing the page to be read
  into the working set. If the pattern of memory accesses is such that a
  page fault occurs on nearly every access, the process is said to be
  **thrashing**, and will run exceedingly slowly.
  
virtual operating system 
  A package of system calls, providing a set of primitive functions
  comparable to those provided by an actual operating system, which can be
  interfaced to a number of actual operating systems. The IRAF virtual
  operating system provides routines (the so-called **z-routines**) for
  file access, process initiation and control, interprocess communication,
  memory management, magtape i/o, exception handling, logical names, and
  time and date.
  
whitespace 
  A sequence of one or more occurrences of the characters blank or tab.
  
z-routines 
  Machine dependent routines, used to interface to the host operating
  system. The IRAF z-routines are maintained in the package "OS".
