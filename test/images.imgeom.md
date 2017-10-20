# imgeom - Image geometric transformation package

## blkavg - Block average or sum a list of N-D images

```
cl> blkavg dev$pix avg 2 3
cl> imstat avg
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
                  avg     43776     107.8     122.7       28.    12997.
```

## blkrep - Block replicate a list of N-D images

```
cl> blkrep dev$pix rep 2 3
cl> imstat rep
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
                  rep   1572864     108.3     131.3       -1.    19936.
```

## imlintran - Linearly transform a list of 2-D images

```
cl> imlintran dev$pix lintran 45.0 45.0 0.50 0.50

Transforming image dev$pix to image lintran
    xshift: 75.13 yshift: 256.50 xmag: 0.50 ymag: 0.50 xrot: 45.00 yrot: 45.00
cl> imstat lintran
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
              lintran    262144     45.46     56.19        4.     4916.
```

## imshift - Shift a list of 1-D or 2-D images

```
cl> imshift dev$pix shift 3.2 -4.5 inter=poly5 bound=neare
cl> imstat shift
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
                shift    262144     107.8     130.9       21.    19756.
```

## imtranspose - Transpose a list of 2-D images

```
cl> imtranspose dev$pix tpix
cl> imstat tpix
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
                 tpix    262144     108.3     131.3       -1.    19936.
```

## im3dtran - Transpose a list of 3-D images

nothing yet

## magnify - Magnify a list of 1-D or 2-D images

```
vocl> magnify dev$pix mag 2.5 2.5

mag
  Magnify image dev$pix to image mag.
  Interpolation is linear.
  Boundary extension is nearest.
  Output coordinates in terms of input coordinates:
    x1 =         1., x2 =       512., dx =        0.4
    y1 =         1., y2 =       512., dy =        0.4
vocl> imstat mag
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
                  mag   1633284     16.86     20.46        1.     3176.
```

## rotate - Rotate and shift a list of 2-D images

```
cl> rotate dev$pix r30 30.0

Transforming image dev$pix to image r30
    xshift: -93.89 yshift: 162.61 xmag: 1.00 ymag: 1.00 xrot: 30.00 yrot: 30.00
cl> imstat r30
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
                  r30    262144     107.4      127.       14.    17175.
```

## shiftlines - Shift the lines of a list of N-D images

```
cl> shiftlines dev$pix shi 0.25
cl> imstat shi
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
                  shi    262144     107.9     129.1        5.    19834.
```

