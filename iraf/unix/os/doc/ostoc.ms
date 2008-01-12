.RP
.ND
.TL
Contents
.PP
Hi there.
.pn 0
.bp
.ce
\fBContents\fR
.sp 3
1.\h'|0.4i'\fBIntroduction\fP\l'|5.6i.'\0\01
.sp
2.\h'|0.4i'\fBStructure of the IRAF System Software\fP\l'|5.6i.'\0\02
.sp
3.\h'|0.4i'\fBThe IRAF System Interface\fP\l'|5.6i.'\0\04
.br
\h'|0.4i'3.1.\h'|0.9i'The Language Interface\l'|5.6i.'\0\04
.br
\h'|0.9i'3.1.1.\h'|1.5i'Fortran\l'|5.6i.'\0\05
.br
\h'|0.9i'3.1.2.\h'|1.5i'Mixing C and Fortran in the same System\l'|5.6i.'\0\06
.br
\h'|0.9i'3.1.3.\h'|1.5i'Critique of C as a Scientific Language\l'|5.6i.'\0\08
.br
\h'|0.9i'3.1.4.\h'|1.5i'The IRAF Subset Preprocessor Language\l'|5.6i.'\0\09
.br
\h'|0.9i'3.1.5.\h'|1.5i'Limitations of the Subset Preprocessor\l'|5.6i.'\0\010
.br
\h'|0.4i'3.2.\h'|0.9i'Bootstrapping the System\l'|5.6i.'\0\011
.br
\h'|0.4i'3.3.\h'|0.9i'The IRAF Kernel\l'|5.6i.'\0\011
.br
\h'|0.4i'3.4.\h'|0.9i'The Virtual Machine Model\l'|5.6i.'\0\012
.br
\h'|0.9i'3.4.1.\h'|1.5i'The Minimal Host Machine\l'|5.6i.'\0\012
.br
\h'|0.9i'3.4.2.\h'|1.5i'The Ideal Host Machine\l'|5.6i.'\0\013
.sp
4.\h'|0.4i'\fBA Reference Manual for the IRAF Kernel\fP\l'|5.6i.'\0\015
.br
\h'|0.4i'4.1.\h'|0.9i'Conventions\l'|5.6i.'\0\016
.br
\h'|0.4i'4.2.\h'|0.9i'Avoiding Library Conflicts\l'|5.6i.'\0\017
.br
\h'|0.4i'4.3.\h'|0.9i'File I/O\l'|5.6i.'\0\017
.br
\h'|0.9i'4.3.1.\h'|1.5i'Text Files\l'|5.6i.'\0\018
.br
\h'|0.9i'4.3.2.\h'|1.5i'Binary Files\l'|5.6i.'\0\020
.br
\h'|0.9i'4.3.3.\h'|1.5i'Specifying Device Parameters\l'|5.6i.'\0\021
.br
\h'|0.9i'4.3.4.\h'|1.5i'Standard File Devices\l'|5.6i.'\0\023
.br
\h'|1.5i'4.3.4.1.\h'|2.2i'The User Terminal\l'|5.6i.'\0\023
.br
\h'|1.5i'4.3.4.2.\h'|2.2i'The Line Printer Device\l'|5.6i.'\0\024
.br
\h'|1.5i'4.3.4.3.\h'|2.2i'Interprocess Communication\l'|5.6i.'\0\025
.br
\h'|1.5i'4.3.4.4.\h'|2.2i'Imagefile Access\l'|5.6i.'\0\026
.br
\h'|1.5i'4.3.4.5.\h'|2.2i'Magtape Devices\l'|5.6i.'\0\028
.br
\h'|0.4i'4.4.\h'|0.9i'Filename Mapping\l'|5.6i.'\0\031
.br
\h'|0.9i'4.4.1.\h'|1.5i'Virtual Filenames\l'|5.6i.'\0\032
.br
\h'|1.5i'4.4.1.1.\h'|2.2i'Logical Directories and Pathnames\l'|5.6i.'\0\032
.br
\h'|1.5i'4.4.1.2.\h'|2.2i'Filename Extensions\l'|5.6i.'\0\033
.br
\h'|0.9i'4.4.2.\h'|1.5i'Filename Mapping Algorithm\l'|5.6i.'\0\034
.br
\h'|0.4i'4.5.\h'|0.9i'Directory Access\l'|5.6i.'\0\037
.br
\h'|0.4i'4.6.\h'|0.9i'File Management Primitives\l'|5.6i.'\0\038
.br
\h'|0.4i'4.7.\h'|0.9i'Process Control\l'|5.6i.'\0\039
.br
\h'|0.9i'4.7.1.\h'|1.5i'Overview and Terminology\l'|5.6i.'\0\039
.br
\h'|0.9i'4.7.2.\h'|1.5i'Synchronous Subprocesses\l'|5.6i.'\0\040
.br
\h'|0.9i'4.7.3.\h'|1.5i'Standard IPC Commands\l'|5.6i.'\0\043
.br
\h'|0.9i'4.7.4.\h'|1.5i'Example\l'|5.6i.'\0\045
.br
\h'|0.9i'4.7.5.\h'|1.5i'Background Jobs\l'|5.6i.'\0\046
.br
\h'|0.9i'4.7.6.\h'|1.5i'The Process and IRAF Mains\l'|5.6i.'\0\048
.br
\h'|1.5i'4.7.6.1.\h'|2.2i'The Process Main\l'|5.6i.'\0\048
.br
\h'|1.5i'4.7.6.2.\h'|2.2i'The IRAF Main\l'|5.6i.'\0\050
.br
\h'|0.9i'4.7.7.\h'|1.5i'Process Control Primitives\l'|5.6i.'\0\051
.br
\h'|0.4i'4.8.\h'|0.9i'Exception Handling\l'|5.6i.'\0\052
.br
\h'|0.4i'4.9.\h'|0.9i'Memory Management\l'|5.6i.'\0\054
.br
\h'|0.4i'4.10.\h'|0.9i'Procedure Call by Reference\l'|5.6i.'\0\055
.br
\h'|0.4i'4.11.\h'|0.9i'Date and Time\l'|5.6i.'\0\056
.br
\h'|0.4i'4.12.\h'|0.9i'Sending a Command to the Host OS\l'|5.6i.'\0\056
.sp
5.\h'|0.4i'\fBBit and Byte Primitives\fP\l'|5.6i.'\0\057
.br
\h'|0.4i'5.1.\h'|0.9i'Bitwise Boolean Primitives\l'|5.6i.'\0\057
.br
\h'|0.4i'5.2.\h'|0.9i'Bitfield Primitives\l'|5.6i.'\0\058
.br
\h'|0.4i'5.3.\h'|0.9i'Byte Primitives\l'|5.6i.'\0\058
.br
\h'|0.4i'5.4.\h'|0.9i'Vector Primitives\l'|5.6i.'\0\060
.br
\h'|0.4i'5.5.\h'|0.9i'MII Format Conversions\l'|5.6i.'\0\060
.br
\h'|0.4i'5.6.\h'|0.9i'Machine Constants for Mathematical Libraries\l'|5.6i.'\0\061
.sp
6.\h'|0.4i'\fBSystem Parameterization and Tuning\fP\l'|5.6i.'\0\062
.sp
7.\h'|0.4i'\fBOther Machine Dependencies\fP\l'|5.6i.'\0\062
.br
\h'|0.4i'7.1.\h'|0.9i'Machine Dependencies in the CL\l'|5.6i.'\0\063
.sp
8.\h'|0.4i'\fBSpecifications for the Kernel Procedures\fP\l'|5.6i.'\0\063
