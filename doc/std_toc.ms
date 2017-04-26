.RP
.ND
.TL
Contents
.PP
Hi there.
.bp
.ce
\fBContents\fR
.sp
.sp
1.\h'|0.4i'\fBIntroduction\fP\l'|5.6i.'\0\01
.br
\h'|0.4i'1.1.\h'|0.9i'Official Acceptance Procedure\l'|5.6i.'\0\01
.sp
2.\h'|0.4i'\fBSystem Standards\fP\l'|5.6i.'\0\02
.br
\h'|0.4i'2.1.\h'|0.9i'Standard Data Structures\l'|5.6i.'\0\02
.br
\h'|0.9i'2.1.1.\h'|1.5i'Text and Binary Files\l'|5.6i.'\0\02
.br
\h'|0.9i'2.1.2.\h'|1.5i'Parameter Files\l'|5.6i.'\0\02
.br
\h'|0.9i'2.1.3.\h'|1.5i'Imagefiles\l'|5.6i.'\0\03
.br
\h'|1.5i'2.1.3.1.\h'|2.2i'standard nomenclature for images\l'|5.6i.'\0\03
.br
\h'|1.5i'2.1.3.2.\h'|2.2i'definition of a pixel\l'|5.6i.'\0\03
.br
\h'|0.9i'2.1.4.\h'|1.5i'Datafiles\l'|5.6i.'\0\04
.br
\h'|0.9i'2.1.5.\h'|1.5i'List Files\l'|5.6i.'\0\04
.br
\h'|0.9i'2.1.6.\h'|1.5i'FITS\l'|5.6i.'\0\04
.br
\h'|0.4i'2.2.\h'|0.9i'Virtual File Names\l'|5.6i.'\0\04
.br
\h'|0.4i'2.3.\h'|0.9i'Standard Filename Extensions\l'|5.6i.'\0\05
.br
\h'|0.4i'2.4.\h'|0.9i'One Indexing\l'|5.6i.'\0\06
.br
\h'|0.4i'2.5.\h'|0.9i'The Procedure Naming Convention for the System Libraries\l'|5.6i.'\0\06
.br
\h'|0.9i'2.5.1.\h'|1.5i'Orthogonality\l'|5.6i.'\0\07
.br
\h'|0.9i'2.5.2.\h'|1.5i'Standard package prefixes\l'|5.6i.'\0\07
.br
\h'|0.9i'2.5.3.\h'|1.5i'Standard type suffixes\l'|5.6i.'\0\08
.br
\h'|0.4i'2.6.\h'|0.9i'Mapping of External Identifiers\l'|5.6i.'\0\08
.br
\h'|0.4i'2.7.\h'|0.9i'Conventions for Ordering Argument Lists\l'|5.6i.'\0\09
.sp
3.\h'|0.4i'\fBCoding Standards\fP\l'|5.6i.'\0\09
.br
\h'|0.4i'3.1.\h'|0.9i'General Guidelines\l'|5.6i.'\0\010
.br
\h'|0.9i'3.1.1.\h'|1.5i'Packages and Tasks\l'|5.6i.'\0\010
.br
\h'|0.9i'3.1.2.\h'|1.5i'Procedures\l'|5.6i.'\0\011
.br
\h'|0.4i'3.2.\h'|0.9i'Languages\l'|5.6i.'\0\013
.br
\h'|0.9i'3.2.1.\h'|1.5i'The SPP Language\l'|5.6i.'\0\013
.br
\h'|0.9i'3.2.2.\h'|1.5i'The Fortran Language\l'|5.6i.'\0\013
.br
\h'|0.4i'3.3.\h'|0.9i'Standard Interfaces\l'|5.6i.'\0\014
.br
\h'|0.4i'3.4.\h'|0.9i'Package Organization\l'|5.6i.'\0\015
.br
\h'|0.4i'3.5.\h'|0.9i'Tasks and Processes\l'|5.6i.'\0\015
.br
\h'|0.4i'3.6.\h'|0.9i'File Organization\l'|5.6i.'\0\016
.br
\h'|0.4i'3.7.\h'|0.9i'Header Files\l'|5.6i.'\0\016
.br
\h'|0.4i'3.8.\h'|0.9i'Comments\l'|5.6i.'\0\017
.br
\h'|0.4i'3.9.\h'|0.9i'Procedure Declarations\l'|5.6i.'\0\018
.br
\h'|0.4i'3.10.\h'|0.9i'Statements\l'|5.6i.'\0\019
.br
\h'|0.9i'3.10.1.\h'|1.5i'Statement Templates\l'|5.6i.'\0\019
.br
\h'|0.4i'3.11.\h'|0.9i'Expressions\l'|5.6i.'\0\021
.br
\h'|0.4i'3.12.\h'|0.9i'Constants\l'|5.6i.'\0\022
.br
\h'|0.4i'3.13.\h'|0.9i'Naming Conventions\l'|5.6i.'\0\022
.sp
4.\h'|0.4i'\fBPortability Considerations\fP\l'|5.6i.'\0\023
.br
\h'|0.4i'4.1.\h'|0.9i'keep it simple\l'|5.6i.'\0\023
.br
\h'|0.4i'4.2.\h'|0.9i'use the standard interfaces\l'|5.6i.'\0\023
.br
\h'|0.4i'4.3.\h'|0.9i'avoid machine dependent filenames\l'|5.6i.'\0\023
.br
\h'|0.4i'4.4.\h'|0.9i'isolate those portions of a program which perform i/o\l'|5.6i.'\0\024
.br
\h'|0.4i'4.5.\h'|0.9i'keep memory requirements to a reasonable level\l'|5.6i.'\0\024
.br
\h'|0.4i'4.6.\h'|0.9i'make sure argument and function datatypes match\l'|5.6i.'\0\024
.br
\h'|0.4i'4.7.\h'|0.9i'do not use output arguments as local variables\l'|5.6i.'\0\025
.br
\h'|0.4i'4.8.\h'|0.9i'avoid assumptions about the machine precision\l'|5.6i.'\0\025
.br
\h'|0.4i'4.9.\h'|0.9i'do not compare floating point numbers for equality\l'|5.6i.'\0\026
.br
\h'|0.4i'4.10.\h'|0.9i'use the standard predefined machine constants\l'|5.6i.'\0\026
.br
\h'|0.4i'4.11.\h'|0.9i'explicitly initialize variables\l'|5.6i.'\0\027
.br
\h'|0.4i'4.12.\h'|0.9i'beware of functions with side effects\l'|5.6i.'\0\027
.br
\h'|0.4i'4.13.\h'|0.9i'use of intrinsic functions\l'|5.6i.'\0\028
.br
\h'|0.4i'4.14.\h'|0.9i'explicitly align objects in global common\l'|5.6i.'\0\028
.sp
5.\h'|0.4i'\fBSoftware Documentation\fP\l'|5.6i.'\0\028
.br
\h'|0.4i'5.1.\h'|0.9i'User Documentation\l'|5.6i.'\0\029
.br
\h'|0.4i'5.2.\h'|0.9i'System Documentation\l'|5.6i.'\0\030
.br
\h'|0.4i'5.3.\h'|0.9i'Documentation Standards\l'|5.6i.'\0\031
.br
\h'|0.4i'5.4.\h'|0.9i'Technical Writing\l'|5.6i.'\0\031
.sp
\fBReferences\fR
.sp
\fBStandard Nomenclature\fR
