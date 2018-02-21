# photcal - Photometric Calibration Package

This package has serious 64-bit problems in the original 2.16.1 distribution.

## fitparams - Compute the parameters of the transformation equations

This issue is created on base of [a forum post in 2013](http://iraf.net/forum/viewtopic.php?showtopic=1467834). Lets take the following minimal observation list:

File: `sobs.dat`
```
alpha 20. 0.
beta  16. 3.
gamma 15. 7.
delta 13. 6.
```
and this configuration file:

File: `config.dat`
```
observations

x 2
y 3

transformation

fit v1=0.0, v2=0.0
FIT : y = v1 + v2 * x
```
The catalog remains empty in this example (but has to be given as dummy).
Running this gives a segmentation violation on 64-bit platforms:

Test options: `decimals=4`
```
cl> noao
cl> digiphot
cl> photcal
cl> fitparams.interactive=no
cl> fitparams.logfile=""
cl> fitparams sobs.dat stds.dat config.dat fitpar.txt
cl> match "^\#" fitpar.txt stop=yes
begin	FIT
	status	0	(Solution converged)
	variance	2.98077
	stdeviation	1.726491
	avsqerror	1.
	averror		1.
	avsqscatter	0.
	avscatter	0.
	chisqr		2.98077
	msq		1.490385
	rms		1.220813
	reference	y
	fitting		v1+v2*x
	weights		uniform
	parameters	2
		v1	(fit)
		v2	(fit)
	derivatives	2
		0.1
		0.1
	values	2
		19.3846
		-0.9615375
	errors	2
		5.485904
		0.3385923

```
