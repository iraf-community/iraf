.LP
.hm 0.25i 
.nr HM 0.25i
.vs 10
.nr VS 10
.ll 9.0i
.nr LL 9.0i
.ps 9.0
.nr PS 9.0
.po 0.5i 
.nr PO 0.5i
.bp
.LP
\fBIRAF V2.5   Table of Selected Benchmark Results   May 1987\fR
.br
CPU and/or clock times are tabulated below for selected benchmark tests.
CPU times are given in seconds; clock times (in parentheses) are given
as (m:ss).  For the WBIN and RBIN benchmarks, the tabulated result is
the measured bandwidth in Kbytes/second.  For a description of the 
benchmark tests, see the document "A Set of Benchmarks for Measuring
IRAF System Performance", Doug Tody, May l987.
.sp
.TS
cB cB cB cB s cB cB s cB s cB s cB s cB s cB cB
cB cB cB cB s cB cB s cB s cB s cB s cB s cB cB
lB |n| n| n n| n| n n| n n| n n| n n| n n| n| n|.
 	CLSS	MKPKGV	MKHDB	PLOTS	IMADDS	IMADDR	IMSTATR	IMSHIFTR	IMTRAN	WBIN	RBIN
 	_	_	_	_	_	_	_	_	_	_	_

ISI	(0\&:03)	(0\&:25)	6\&.00	(0\&:17)	(0\&:10)	0\&.89	(0\&:05)	3\&.82	(0\&:10)	7\&.77	(0\&:10)	81\&.60	(1\&:29)	1\&.62	(0\&:06)	294.1	277.8

SUN3	(0\&:03)	(0\&:17)	5\&.26	(0\&:10)	(0\&:09)	0\&.62	(0\&:03)	3\&.34	(0\&:09)	8\&.38	(0\&:11)	83\&.44	(1\&:33)	1\&.47	(0\&:05)	625.0	454.5

SUN3+	(0\&:04)	(0\&:19)	5\&.28	(0\&:11)	(0\&:06)	0\&.63	(0\&:03)	0\&.86	(0\&:06)	5\&.1	(0\&:08)	31\&.1	(0\&:36)	1\&.5	(0\&:04)	714.3	454.5

U750	(0\&:17)	(0\&:39)	22\&.79	(0\&:40)	(0\&:29)	3\&.31	(0\&:10)	4\&.28	(0\&:17)	10\&.98	(0\&:15)	114\&.41	(2\&:13)	10\&.19	(0\&:17)	208.3	208.3

V750	(0\&:27)	(4\&:17)	46\&.54	(1\&:11)	(0\&:25)	5\&.90	(0\&:11)	6\&.48	(0\&:14)	10\&.65	(0\&:14)	69\&.62	(1\&:33)	14\&.85	(0\&:20)	238.1	384.6

UMVX	(0\&:09)	(0\&:37)	15\&.5	(0\&:38)	(0\&:20)	2\&.06	(0\&:09)	2\&.98	(0\&:17)	10\&.98	(0\&:16)	95\&.61	(1\&:49)	4\&.93	(0\&:16)	172.4	208.3

VMVX	n/a	n/a	n/a	n/a	(0\&:17)	3\&.44	(0\&:11)	4\&.31	(0\&:15)	9\&.32	(0\&:12)	74\&.72	(1\&:26)	10\&.83	(0\&:35)	192.3	294.1

VMVXM	(0\&:17)	(2\&:16)	27\&.58	(0\&:39)	(0\&:16)	3\&.51	(0\&:07)	4\&.31	(0\&:10)	9\&.31	(0\&:11)	74\&.54	(1\&:21)	10\&.81	(0\&:27)	312.5	500.0

V780	n/a	n/a	n/a	n/a	(0\&:16)	3\&.38	(0\&:08)	4\&.00	(0\&:11)	6\&.88	(0\&:08)	45\&.47	(0\&:53)	7\&.71	(0\&:12)	227.3	416.7

V780S	(0\&:15)	(2\&:09)	26\&.10	(0\&:31)	(0\&:19)	3\&.57	(0\&:10)	4\&.22	(0\&:17)	6\&.78	(0\&:10)	45\&.11	(0\&:57)	7\&.83	(0\&:14)	166.7	263.2

V8600	(0\&:08)	(1\&:05)	8\&.59	(0\&:17)	(0\&:09)	1\&.56	(0\&:05)	1\&.28	(0\&:07)	2\&.09	(0\&:04)	13\&.54	(0\&:32)	2\&.58	(0\&:06)	294.1	625.0

MV10	(0\&:14)	(0\&:29)	6\&.4	(0\&:25)	(0\&:09)	1\&.5	(0\&:06)	1\&.6	(0\&:08)	4\&.8	(0\&:07)	39\&.3	(0\&:47)	2\&.9	(0\&:06)	89.3	200.0

MV8	(0\&:28)	(2\&:17)	13.13	(0\&:57)	(0\&:16)	2\&.85	(0\&:12)	3\&.07	(0\&:17)	9\&.87	(0\&:13)	77\&.68	(1\&:31)	5\&.69	(0\&:12)	31\&.1	200\&.0
.TE
.sp
.LP
\fBKEY:\fR
.TS
lB lw(8.0i).
ISI	T{
Integrated Solutions with 16-Mhz 68020 and 16-Mhz 68881 fp_coprocessor; UNIX 
4.2BSD; 8Mb memory; Greenhills compiler
T}
SUN3	T{
SUN 3/160C with 68881 fp_chip; SUN UNIX 3.3; 8Mb memory; Eagle 
disk with 380Mb
T}
SUN3+	T{
SUN 3/180C with 68881 fp_chip + FPA; SUN UNIX 3.2; 8Mb memory; 380Mb Eagle disk
T}
U750	VAX 11/750+FPA; UNIX 4.3BSD; 8Mb memory; RA81 disk
V750	VAX 11/750+FPA; VMS V4.5; 7.25 Mb memory; RA81/clustered disks
UMVX	VAXSTATION II/GPX; ULTRIX 1.2; 5Mb memory; 150 Mb RD54 disk
VMVXM	T{
VAXSTATION II/GPX; MICROVMS V4.5; 5Mb memory; IRAF installed on 300MB
MAXSTOR disk, data files on this disk also
T}
VMVX	T{
VAXSTATION II/GPX; MICROVMS V4.5; 5Mb memory; IRAF on 300MB
MAXSTOR disk, data on 70Mb RD53 (84% full)
T}
V780	T{
VAX 11/780+FPA; VMS V4.5; 16Mb memory; IRAF installed on an RA81, data on an 
RM03 disk with 23 free Mb, Massbus
T}
V780S	T{
VAX 11/780+FPA; VMS V4.5; 16Mb memory; IRAF and data on an RA81 disk, Unibus
T}
V8600	VAX 8600; VMS V4.5; 28Mb memory; RA81/clustered disks
MV10	T{
MV 10000; AOSVS 7.54; 24Mb memory; 2-600 Mb ARGUS and 2-600 Mb KISMET disks
T}
MV8	T{
MV 8000 at La Serena; 5Mb memory, 2 large DG disks, 2 small Winchesters, 
disks nearly full and badly fragmented
T}
.TE
