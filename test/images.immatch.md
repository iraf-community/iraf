# immatch - Image matching and combining package

File: `rtran`
```
1.0000  1.0000 184.1445 -153.0376
512.0000 1.0000 684.0376 184.1445
512.0000 512.0000 346.8555 684.0376
1.0000 512.0000 -153.0380 346.8555  
```

## geomap - Compute geometric transforms using matched coordinate lists

Compute  the  linear  transformation between coordinate systems.

Test options: `decimals=5`
```
cl> geomap rtran rtran.db 1.0 512.0 1.0 512.0 intera- | match rms stop=yes

Coordinate list: rtran  Transform: rtran
    Results file: 
Coordinate mapping status
    X fit ok.  Y fit ok.
Coordinate mapping parameters
    Mean Xref and Yref: 256.5  256.5
    Mean Xin and Yin: 265.4999  265.5
    X and Y shift: 183.826  -154.6757  (xin  yin)
    X and Y scale: 1.18  1.18  (xin / xref  yin / yref)
    X and Y axis rotation: 326.00000  326.00000  (degrees  degrees)
cl> tail rtran.db nlines=-1 | match rms stop=yes
begin	rtran
	xrefmean	256.5
	yrefmean	256.5
	xmean		265.4999084472656
	ymean		265.5
	geometry	general
	function	polynomial
	xshift		183.826
	yshift		-154.6757
	xmag		1.18
	ymag		1.18
	xrotation	326.
	yrotation	326.
	surface1	11
			3.	3.
			2.	2.
			2.	2.
			0.	0.
			1.	1.
			512.	512.
			1.	1.
			512.	512.
			183.826	-154.6757
			0.9782647	0.6598474
			-0.6598479	0.9782643
	surface2	0
```

## geoxytran - Transform coordinate lists using the geomap transforms

Compute the transformation from the reference system to the output
system and then evaluate the transformation for both the input list
and the list of unknowns.

Test options: `decimals=2`
```
cl> geoxytran rtran STDOUT rtran.db rtran
184.1444 -153.038 184.1445 -153.0376
684.0377 184.1444 684.0376 184.1445
346.8554 684.0375 346.8555 684.0376
-153.038 346.8555 -153.0380 346.8555  
```

## gregister - Register 1-D or 2-D images using the geomap transforms

## imalign - Align and register 2-D images using a reference pixel list

## imcentroid - Compute and print relative shifts for a list of 2-D images

## imcombine - Combine images pixel-by-pixel using various algorithms

## linmatch - Match the linear intensity scales of 1-D or 2-D images

## psfmatch - Match the point-spread functions of 1-D or 2-D images

## skymap - Compute geometric transforms using the image celestial wcs

## skyxymtach - Generate matched pixel lists using the image celestial wcs

## sregister - Register 1-D or 2-D images using the image celestial wcs

## wcscopy - Copy the wcs from one image to another

## wcsmap - Compute geometric transforms using the image wcs

## wcsxymatch - Generate matched pixel lists using the image wcs

## wregister - Register 1-D or 2-D images using the image wcs 

## xregister - Register 1-D or 2-D images using x-correlation techniques

## xyxymatch - Match pixel coordinate lists

