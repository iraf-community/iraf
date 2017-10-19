# imfit - Image fitting package

## fit1d - Fit a function to image lines or columns

```
cl> fit1d dev$pix fitimage fit interactive-
cl> imstat fitimage
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
             fitimage    262144     108.3     65.53    -236.1     521.1
```

## imsurfit - Fit a surface to a 2-D image

```
cl> imsurfit dev$pix smooth 5 10 function=spline3
cl> imstat smooth
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
               smooth    262144     107.8     69.11      -39.      480.
```

## lineclean - Replace deviant pixels in image lines

```
cl> lineclean dev$pix clean interactive-
cl> imstat clean
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
                clean    262144     100.7     62.55        9.     1661.
```

