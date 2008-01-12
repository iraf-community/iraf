
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
.sp
.sp
1.\h'|0.4i'\fBIntroduction\fP\l'|5.6i.'\0\01
.sp
2.\h'|0.4i'\fBTerminology\fP\l'|5.6i.'\0\02
.br
\h'|0.4i'2.1.\h'|0.9i'Physical and Logical Tasks, Scripts\l'|5.6i.'\0\02
.br
\h'|0.4i'2.2.\h'|0.9i'Packages\l'|5.6i.'\0\02
.sp
3.\h'|0.4i'\fBParameter Files\fP\l'|5.6i.'\0\03
.br
\h'|0.4i'3.1.\h'|0.9i'Location and Name of Parameter Files\l'|5.6i.'\0\03
.br
\h'|0.4i'3.2.\h'|0.9i'Parameter File Format\l'|5.6i.'\0\03
.br
\h'|0.9i'3.2.1.\h'|1.5i'name\l'|5.6i.'\0\04
.br
\h'|0.9i'3.2.2.\h'|1.5i'type\l'|5.6i.'\0\04
.br
\h'|0.9i'3.2.3.\h'|1.5i'mode\l'|5.6i.'\0\04
.br
\h'|0.9i'3.2.4.\h'|1.5i'value\l'|5.6i.'\0\05
.br
\h'|0.9i'3.2.5.\h'|1.5i'minimum \fRand\fP maximum\l'|5.6i.'\0\06
.br
\h'|0.9i'3.2.6.\h'|1.5i'prompt\l'|5.6i.'\0\06
.sp
4.\h'|0.4i'\fBModes\fP\l'|5.6i.'\0\06
.br
\h'|0.4i'4.1.\h'|0.9i'Determining Modes\l'|5.6i.'\0\07
.br
\h'|0.4i'4.2.\h'|0.9i'Setting and Changing Modes\l'|5.6i.'\0\07
.br
\h'|0.4i'4.3.\h'|0.9i'Recommended Mode Settings\l'|5.6i.'\0\07
.sp
5.\h'|0.4i'\fBExpressions\fP\l'|5.6i.'\0\08
.br
\h'|0.4i'5.1.\h'|0.9i'Constants\l'|5.6i.'\0\08
.br
\h'|0.4i'5.2.\h'|0.9i'Parameter References\l'|5.6i.'\0\08
.br
\h'|0.4i'5.3.\h'|0.9i'Intrinsic Functions\l'|5.6i.'\0\09
.br
\h'|0.4i'5.4.\h'|0.9i'Operators\l'|5.6i.'\0\010
.sp
6.\h'|0.4i'\fBStatements\fP\l'|5.6i.'\0\010
.br
\h'|0.4i'6.1.\h'|0.9i'Assignment Statement\l'|5.6i.'\0\011
.br
\h'|0.4i'6.2.\h'|0.9i'Commands\l'|5.6i.'\0\011
.br
\h'|0.9i'6.2.1.\h'|1.5i'Command Arguments\l'|5.6i.'\0\011
.br
\h'|0.9i'6.2.2.\h'|1.5i'Pipes and Redirections\l'|5.6i.'\0\012
.br
\h'|0.4i'6.3.\h'|0.9i'Immediate Statement\l'|5.6i.'\0\012
.br
\h'|0.4i'6.4.\h'|0.9i'Flow Control\l'|5.6i.'\0\012
.br
\h'|0.4i'6.5.\h'|0.9i'Abbreviations\l'|5.6i.'\0\013
.sp
7.\h'|0.4i'\fBEnvironment\fP\l'|5.6i.'\0\013
.sp
8.\h'|0.4i'\fBLog File\fP\l'|5.6i.'\0\014
.sp
9.\h'|0.4i'\fBError Handling\fP\l'|5.6i.'\0\014
.sp
10.\h'|0.4i'\fBCL Initialization\fP\l'|5.6i.'\0\015
.sp
11.\h'|0.4i'\fBCL Directives\fP\l'|5.6i.'\0\015
.br
\h'|0.4i'11.1.\h'|0.9i'\fBbye\fR\l'|5.6i.'\0\015
.br
\h'|0.4i'11.2.\h'|0.9i'\fBcache \fIlt [, lt2, ...]\fR\l'|5.6i.'\0\015
.br
\h'|0.4i'11.3.\h'|0.9i'\fBcl\fR\l'|5.6i.'\0\015
.br
\h'|0.4i'11.4.\h'|0.9i'\fBkeep\fR\l'|5.6i.'\0\016
.br
\h'|0.4i'11.5.\h'|0.9i'\fBlparam\fP \fIlt [, lt2, ...]\fR\l'|5.6i.'\0\016
.br
\h'|0.4i'11.6.\h'|0.9i'\fBpackage\fP \fIpackname\fR\l'|5.6i.'\0\016
.br
\h'|0.4i'11.7.\h'|0.9i'\fBredefine\fP \fI[lt1, lt2, ...] lt = pt\fR\l'|5.6i.'\0\016
.br
\h'|0.4i'11.8.\h'|0.9i'\fBset\fP \fI[name = value]\fR\l'|5.6i.'\0\016
.br
\h'|0.4i'11.9.\h'|0.9i'\fBtask\fP \fI[lt1, lt2, ...] lt = pt\fR\l'|5.6i.'\0\016
.br
\h'|0.4i'11.10.\h'|0.9i'\fBupdate\fP \fIlt [, lt2, ...]\fR\l'|5.6i.'\0\017
.br
\h'|0.4i'11.11.\h'|0.9i'\fBversion\fR\l'|5.6i.'\0\017
.br
\h'|0.4i'11.12.\h'|0.9i'\fB?\fP and \fB??\fR\l'|5.6i.'\0\017
.sp
12.\h'|0.4i'\fBCL Parameters\fP\l'|5.6i.'\0\017
.sp
13.\h'|0.4i'\fBAn Example\fP\l'|5.6i.'\0\017
