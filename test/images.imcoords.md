# imcoords - Image coordinates package

These tests were collected from the "EXAMPLES" section in the help of
the commands in the "images" package.

## ccfind - Find catalog sources in an image

File: `wpix.coords`
```
13:29:47.297  47:13:37.52
13:29:37.406  47:09:09.18
13:29:38.700  47:13:36.23
13:29:55.424  47:10:05.15
13:30:01.816  47:12:58.79
```

```
cl> imcopy dev$wpix wpix
dev$wpix -> wpix
cl> hedit wpix equinox 1950.0 verify- add+
add wpix,equinox = 1950.
wpix updated
cl> ccfind wpix.coords wpix.match wpix usewcs+

Input File: wpix.coords  Output File: wpix.match
    Image: wpix  Wcs: 
Insystem: j2000  Coordinates: equatorial FK5
    Equinox: J2000.000 Epoch: J2000.00000000 MJD: 51544.50000
Refsystem: wpix.fits logical  Projection: TAN  Ra/Dec axes: 1/2
    Coordinates: equatorial FK4 Equinox: B1950.000
    Epoch: B1987.25767884 MJD: 46890.00000
Located 5 objects in image wpix

cl> type wpix.match

# Input File: wpix.coords  Output File: wpix.match
#     Image: wpix  Wcs: 
# Insystem: j2000  Coordinates: equatorial FK5
#     Equinox: J2000.000 Epoch: J2000.00000000 MJD: 51544.50000
# Refsystem: wpix.fits logical  Projection: TAN  Ra/Dec axes: 1/2
#     Coordinates: equatorial FK4 Equinox: B1950.000
#     Epoch: B1987.25767884 MJD: 46890.00000

13:29:47.297  47:13:37.52     327.504    410.379
13:29:37.406  47:09:09.18     465.503     62.101
13:29:38.700  47:13:36.23     442.013    409.654
13:29:55.424  47:10:05.15     224.351    131.200
13:30:01.816  47:12:58.79     134.373    356.327
```

## ccget - Extract objects from a text file catalog

```
cl> ccget noao$digiphot/photcal/catalogs/nlandolt.dat output 03:55:00.0 0:00:00 0.2 0.15 outsystem="j2000"

Catalog File: noao$digiphot/photcal/catalogs/nlandolt.dat  Output File: output
Field System: j2000  Coordinates: equatorial FK5
    Equinox: J2000.000 Epoch: J2000.00000000 MJD: 51544.50000
Catalog System: j2000  Coordinates: equatorial FK5
    Equinox: J2000.000 Epoch: J2000.00000000 MJD: 51544.50000
Output System: j2000  Coordinates: equatorial FK5
    Equinox: J2000.000 Epoch: J2000.00000000 MJD: 51544.50000
#
# Field Center:  3:55:00.0 0:00:00.0  Width: 0.2000 0.1500
# Field Limits: 3:54:36.0 3:55:24.0  -0:04:30.0 0:04:30.0
#
      95_62  03:55:00.0   -0:02:55  13.538  1.355  1.181  0.742  0.685  1.428  22  11  0.0030 0.0053 0.0136 0.0019 0.0019 0.0028
     95_137  03:55:04.0    0:03:33  14.440  1.457  1.136  0.893  0.845  1.737   1   1  0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
     95_139  03:55:05.0    0:03:13  12.196  0.923  0.677  0.562  0.476  1.039   3   2  0.0017 0.0046 0.0191 0.0023 0.0017 0.0035
     95_142  03:55:09.0    0:01:19  12.927  0.588  0.097  0.371  0.375  0.745  22  11  0.0030 0.0030 0.0036 0.0019 0.0017 0.0028
```

## ccmap - Compute image plate solutions using matched coordinate lists

File: `coords`
```
13:29:47.297  47:13:37.52  327.50  410.38
13:29:37.406  47:09:09.18  465.50   62.10
13:29:38.700  47:13:36.23  442.01  409.65
13:29:55.424  47:10:05.15  224.35  131.20
13:30:01.816  47:12:58.79  134.37  356.33
```

```
cl> imcopy dev$pix pix
dev$pix -> pix
cl> ccmap coords coords.db image=pix xcol=3 ycol=4 lngcol=1 latcol=2 inter- verb+
Refsystem: j2000  Coordinates: equatorial FK5
    Equinox: J2000.000 Epoch: J2000.00000000 MJD: 51544.50000
Insystem: j2000  Coordinates: equatorial FK5
    Equinox: J2000.000 Epoch: J2000.00000000 MJD: 51544.50000

Coords File: coords  Image: pix
    Database: coords.db  Solution: pix
Coordinate mapping status
    XI fit ok.  ETA fit ok.
    Ra/Dec or Long/Lat fit rms: 0.229  0.241   (arcsec  arcsec)
Coordinate mapping parameters
    Sky projection geometry: tan
    Reference point: 13:29:48.130  47:11:53.44  (hours  degrees)
    Reference point: 318.717  273.981  (pixels  pixels)
    X and Y scale: 0.764  0.767  (arcsec/pixel  arcsec/pixel)
    X and Y axis rotation: 179.110  358.958  (degrees  degrees)
Wcs mapping status
    Ra/Dec or Long/Lat wcs rms: 0.229  0.241   (arcsec  arcsec)
```


## ccsetwcs - Create an image celestial wcs from the ccmap plate solution

Test options: `decimals=5`
```
cl> ccsetwcs pix coords.db pix 
Image: pix  Database: coords.db  Solution: pix
Coordinate mapping parameters
    Sky projection geometry: tan
    Reference point: 13:29:48.130  47:11:53.44  (hours   degrees)
    Ra/Dec logical image axes: 1  2
    Reference point: 318.717  273.981  (pixels  pixels)
    X and Y scale: 0.764  0.767  (arcsec/pixel  arcsec/pixel)
    X and Y coordinate rotation: 179.110  358.958  (degrees  degrees)
Updating image header wcs
cl> imheader pix l+ | match ^CTYPE
CTYPE1  = 'RA---TAN'
CTYPE2  = 'DEC--TAN'
cl> imheader pix l+ | match ^CRVAL
CRVAL1  =     202.450540982385
CRVAL2  =     47.1981766432095
cl> imheader pix l+ | match ^CRPIX
CRPIX1  =     318.717314475376
CRPIX2  =     273.980754205693
cl> imheader pix l+ | match ^CD
CD1_1   =  -2.1224478467185E-4
CD1_2   =  -3.8721154990263E-6
CD2_1   =  -3.2966623625400E-6
CD2_2   =  2.12934727219336E-4
cl> imheader pix l+ | match ^LTM
LTM1_1  =                   1.
LTM2_2  =                   1.
cl> imheader pix l+ | match ^WAT
WAT0_001= 'system=image'
WAT1_001= 'wtype=tan axtype=ra'
WAT2_001= 'wtype=tan axtype=dec'
cl> imheader pix l+ | match ^WCSDIM
WCSDIM  =                    2
```

## ccstd - Transform to and from standard astrometric coordinates

Compute the standard coordinates in arcseconds per pixel given a list
of pixel and equatorial coordinates and the position of the reference
point in pixel and equatorial coordinates.

```
cl> ccstd coords STDOUT "" xref=256.5 yref=256.5 lngref=13:29:48.1 latref = 47:11:53.4 xcol=3 ycol=4 lngcol=1 latcol=2
  -8.180   104.120    71.000   153.880
-109.087  -164.189   209.000  -194.400
 -95.753   102.854   185.510   153.150
  74.688  -108.235   -32.150  -125.300
 139.745    65.441  -122.130    99.830
```

## cctran - Transform coordinate lists using the ccmap plate solution

```
cl> cctran coords STDOUT coords.db coords xcol=3 ycol=4 lngformat=%0.3h latformat=%0.2h
13:29:47.297  47:13:37.52  327.50  410.38
13:29:37.406  47:09:09.18  465.50   62.10
13:29:38.700  47:13:36.23  442.01  409.65
13:29:55.424  47:10:05.15  224.35  131.20
13:30:01.816  47:12:58.79  134.37  356.33
```

## hpctran - Convert between HEALPix row and spherical coordinate

Test options: `decimals=7`
```
cl> hpctran lng=50.12 lat=-33.45
2298092 50.12 -33.45000000000001
``` 

## imcctran - Transform image header from one celestial wcs to another

Test options: `decimals=5`
```
cl> imcctran pix j1975.0
INPUT IMAGE: pix
Insystem: pix logical  Projection: TAN  Ra/Dec axes: 1/2
    Coordinates: equatorial FK5 Equinox: J2000.000
    Epoch: J1987.25667351 MJD: 46890.00000
Outsystem: j1975.0  Coordinates: equatorial FK5
    Equinox: J1975.000 Epoch: J1975.00000000 MJD: 42413.25000
Crval1,2: 202:27:01.9, 47:11:53.4 -> 202:11:14.9, 47:19:37.0 dd:mm:ss.s
    Scaling: Xmag: 0.999996 Ymag: 1.000004 Xrot: 359.922 Yrot: 359.922 degrees
    Rms: X fit: 1.209002E-5 pixels  Y fit: 1.286608E-5 pixels

```

## mkcwcs - Make or update a simple celestial wcs

Test options: `decimals=5`
```
cl> mkcwcs new ra=1:20:23.1 dec=-12:11:13 scale=0.25
cl> imheader new l+ | match ^CTYPE
CTYPE1  = 'RA---TAN'
CTYPE2  = 'DEC--TAN'
cl> imheader pix l+ | match ^CRVAL
CRVAL1  =     202.187467189328
CRVAL2  =     47.3269441654267
cl> imheader pix l+ | match ^CRPIX
CRPIX1  =     318.717314475376
CRPIX2  =     273.980754205693
cl> imheader pix l+ | match ^CD
CD1_1   =  -2.1224907057248E-4
CD1_2   =  -3.5826140669847E-6
CD2_1   =  -3.0080993807270E-6
CD2_2   =  2.12939794761845E-4
cl> imheader pix l+ | match ^LTM
LTM1_1  =                   1.
LTM2_2  =                   1.
cl> imheader pix l+ | match ^WAT
WAT0_001= 'system=image'
WAT1_001= 'wtype=tan axtype=ra'
WAT2_001= 'wtype=tan axtype=dec'
cl> imheader pix l+ | match ^WCSDIM
WCSDIM  =                    2
```

## mkcwwcs - Make or update a simple celestial/wavelength 3D wcs

Test options: `decimals=5`
```
cl> mkcwwcs new3d ra=1:20:23.1 dec=-12:11:13 wave=5500.  scale=0.25 wscale=1.23
cl> imheader new3d l+ | match ^CTYPE
CTYPE1  = 'RA---TAN'
CTYPE2  = 'DEC--TAN'
CTYPE3  = 'LINEAR  '
cl> imheader pix l+ | match ^CRVAL
CRVAL1  =     202.187467189328
CRVAL2  =     47.3269441654267
cl> imheader pix l+ | match ^CRPIX
CRPIX1  =     318.717314475376
CRPIX2  =     273.980754205693
cl> imheader pix l+ | match ^CD
CD1_1   =  -2.1224907057248E-4
CD1_2   =  -3.5826140669847E-6
CD2_1   =  -3.0080993807270E-6
CD2_2   =  2.12939794761845E-4
cl> imheader pix l+ | match ^LTM
LTM1_1  =                   1.
LTM2_2  =                   1.
cl> imheader pix l+ | match ^WAT
WAT0_001= 'system=image'
WAT1_001= 'wtype=tan axtype=ra'
WAT2_001= 'wtype=tan axtype=dec'
cl> imheader pix l+ | match ^WCSDIM
WCSDIM  =                    2
```

## starfind - Automatically detect stellar objects in a list of images

```
cl> starfind dev$wpix default 2.5 500. wcs=world wxf="%12.2H" wyf="%12.1h" verb+

Image: dev$wpix.imh  Output: wpix.obj.1
Detection Parameters
    Hwhmpsf: 2.500 (pixels)  Threshold: 500. (ADU) Npixmin: 5
    Datamin: INDEF (ADU)  Datamax: INDEF (ADU)
    Fradius: 2.500 (HWHM)  Sepmin: 5.000 (HWHM)

     465.647    62.086  13:27:31.21   47:24:45.1  -10.72    110   1.61  0.011  177.9   0.644      1
     379.169    66.818  13:27:37.74   47:24:48.8  -11.27    115   1.57  0.039  102.3   0.628      2
     224.349   131.181  13:27:49.42   47:25:38.1  -10.95    114   1.60  0.072  164.0   0.638      3
     372.707   156.520  13:27:38.22   47:25:57.5  -10.61     99   2.35  0.165   70.2   0.938      4
     347.666   188.732  13:27:40.11   47:26:22.2  -13.11    121   1.58  0.019  127.7   0.633      5
      59.511   226.086  13:28:01.87   47:26:50.8   -9.95     84   1.59  0.087   28.3   0.636      6
     347.579   231.578  13:27:40.12   47:26:55.0   -9.86     71   1.38  0.071  129.3   0.550      7
     257.838   258.956  13:27:46.89   47:27:16.0  -12.72    120   2.43  0.073  125.3   0.972      8
     404.525   274.207  13:27:35.81   47:27:27.7  -11.28     97   1.54  0.074   71.8   0.618      9
     442.007   409.617  13:27:32.98   47:29:11.4  -11.04    119   1.60  0.021  133.2   0.639     10
     162.633   434.238  13:27:54.09   47:29:30.3  -10.34    112   2.07  0.044   61.5   0.829     11
     507.767   445.718  13:27:28.00   47:29:39.0   -9.81    113   1.63  0.043  154.9   0.651     12

Selection Parameters
    Maglo: INDEF  Maghi: INDEF
    Roundlo: 0.000  Roundhi: 0.200
    Sharplo: 0.500  Sharphi: 2.000
```

