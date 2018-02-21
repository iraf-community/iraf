# astutil - Astronomical utilities package

## airmass - Compute the airmass at a given elevation above the horizon

```
cl> noao
cl> astutil
cl> airmass 30
airmass 1.996 at an elevation of 30. degrees (0.5236 radians) above horizon
```

## astcalc - Astronomical calculator

File: `example2.dat`
```
# Define variables.
observatory = "kpno"
date = "05/04/87"
ut = 9:27:27
ra = 13:29:24
dec = 47:15:34
epoch = epoch (date, ut)
mst = mst (date, ut, obsdb (observatory, "longitude"))

# Print results of some expressions.
print ((1 + 2 + 3) / 2 - 2 * 2)       # Calculation with constants
print (epoch)                         # Print variable
print (mst)                           # Print variable
print (julday (date, ut))             # Print result of function
print (ra_precess (ra, dec, epoch, 1950))
print (dec_precess (ra, dec, epoch, 1950))
print (airmass (ra, dec, mst, obsdb (observatory, "latitude")))

# Formatted print with arguments.  Note newline.
printf ("Hello World: %s\n", precess (ra, dec, epoch, 1950))
```

Test options: `decimals=7`
```
cl> noao
cl> astutil
cl> astcalc commands=example2.dat
-1
1987.257752395672
14:53:39.81
2446890.894062519
13:27:49.84
47:27:05.72
1.07968417231416
Hello World: 13:27:49.84  47:27:05.7   1950.
```

### Precess coordinates given in a text file.

File: `example3.dat`
```
# Read table of RA, DEC, and optional EPOCH and precess to 2000.

epoch = 1900            # Default input epoch
epoch1 = 2000           # Precession epoch

# Scan table and precess coordinates.
if (fscan ("ra", "dec", "epoch") >= 2)
    ra1 = ra_precess (ra, dec, epoch, epoch1)
    dec1 = dec_precess (ra, dec, epoch, epoch1)
    printf ("%h %h %d -> %h %h %d\n", ra, dec, epoch, ra1, dec1, epoch1)
else
    printf ("Missing coordinates\n")
endif
```

File: `table.dat`
```
12:22:31        31:10:15        1950
13:52:44        10:21:32        1996.1
14:52:44        11:21:32
10:20:30
```

```
cl> noao
cl> astutil
cl> astcalc commands=example3.dat table=table.dat
12:22:31.0 31:10:15.0 1950 -> 12:25:00.56 30:53:38.13 2000
13:52:44.0 10:21:32.0 1996 -> 13:52:55.54 10:20:23.11 2000
14:52:44.0 11:21:32.0 1900 -> 14:57:33.16 10:57:24.74 2000
Missing coordinates
```

## asthedit - Astronomical header editor

## astradius - Find images within a circle on the sky

## asttimes - Compute UT, Julian day, epoch, and sidereal time

```
cl> noao
cl> astutil
cl> asttimes year=1987 month=10 day=28 time=15:30 obs=kpno
# ASTTIMES: Observatory parameters for Kitt Peak National Observatory
#       timezone = 7
#       longitude = 111:36.0
##YR MON   DAY          ZT         UT      EPOCH           JD       LMST
1987  10 28 WED 15:30:00.0 22:30:00.0 1987.82324 2447097.4375 17:30:31.8
cl> =asttimes.lmst
17.508823973881
```

## ccdtime - Compute time, magnitude, and signal-to-noise for CCDs

## galactic - Convert ra, dec to galactic coordinates

File: `galactic.coords`
```
12:30:10.12 10:18:27.5 1930.
12:30 10:18
12.5  10:18
```

Test options: `decimals=2`
```
cl> noao
cl> astutil
cl> galactic galactic.coords
  12:30:10.12   10:18:27.5  1930.00     288.4444   72.2931
  12:30:00.00   10:18:00.0  1950.00     287.4345   72.3248
  12:30:00.00   10:18:00.0  1950.00     287.4345   72.3248
```

## gratings - Compute and print grating parameters

## keywpars - Translate the image header keywords used in ASTUTIL package

## pdm - Find periods in light curves by Phase Dispersion Minimization

## precess - Precess a list of astronomical coordinates

File: `precess.coords`
```
12:30:10.12 10:18:27.5
12:30 10:18
12:30 -20 1900
12:30 -20 1900 2000
```

Test options: `decimals=2`
```
cl> noao
cl> astutil
cl> precess precess.coords 1950 1990
  12:32:11.79   10:05:13.09  1990.0
  12:32:01.68   10:04:45.51  1990.0
  12:34:42.90  -20:29:46.32  1990.0
  12:35:14.41  -20:33:04.42  2000.0
```

## rvcorrect - Compute radial velocity corrections

File: `rv.obs`
```
1987 10 21 11:00:24  3:36:15   0:22:04
1987 10 21 11:08:00  8:19:35  -0:51:35
1987 10 21 11:15:47  8:35:12   6:40:29
1987 10 21 12:12:10  9:13:20  61:28:49
1987 10 21 12:16:03  9:27:48   9:07:08
1987 10 21 12:20:43  9:50:45  -6:06:58
1979  3 25 11:22:59 16:07:28 -23:37:49 0 -67.5
```

```
cl> noao
cl> astutil
cl> rvcorrect f=rv.obs obs=kpno > rv.dat
cl> type rv.dat
# RVCORRECT: Observatory parameters for Kitt Peak National Observatory
#	latitude = 31:57.8
#	longitude = 111:36.0
#	altitude = 2120.
##   HJD          VOBS   VHELIO     VLSR   VDIURNAL   VLUNAR  VANNUAL   VSOLAR
2447089.96358     0.00    11.06    -2.74     -0.190    0.008   11.247  -13.808
2447089.96296     0.00    28.05    13.56      0.255    0.010   27.790  -14.498
2447089.96813     0.00    29.04    16.64      0.263    0.011   28.770  -12.401
2447090.00834     0.00    22.06    25.26      0.115    0.010   21.940    3.200
2447090.00884     0.00    27.70    18.55      0.251    0.009   27.438   -9.152
2447090.01129     0.00    23.99    13.50      0.276    0.007   23.704  -10.484
2443957.97715   -67.50   -41.37   -31.48      0.002    0.012   26.117    9.884
```

## setairmass - Compute effective airmass and middle UT for an exposure

```
cl> noao
cl> astutil
cl> setairmass dev$pix exposure=itime obs=kpno update-
#              Image    UT middle  effective  begin   middle     end   updated
	Coords not precessed for dev$pix.imh: check epoch
# SETAIRMASS: Observatory parameters for Kitt Peak National Observatory
#	latitude = 31:57.8
             dev$pix    9:32:27.0   1.0852   1.0797   1.0852   1.0910  no
```

## setjd - Compute and set Julian dates in images
