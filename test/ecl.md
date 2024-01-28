# Tests specific to ECL

Intrinsic mathematical functions specific for ECL

Test options: `decimals=7`
```
cl> =acos(0.67)
0.8365875393415376
cl> =asin(0.67)
0.7342087874533589
cl> =dacos(0.67)
47.932935197504825
cl> =dasin(0.67)
42.067064802495175
cl> =datan2(2.,3.)
33.690067525979785
cl> =dcos(12.)
0.9781476007338057
cl> =dsin(12.)
0.20791169081775934
cl> =dtan(12.)
0.21255656167002213
cl> =hypot(2.,3.)
3.605551275463989
cl> =deg(1.2)
68.75493541569878
cl> =rad(12)
0.20943951023931956
cl> =sign(-1.2)
-1
cl> =sign(1.2)
1
```

Intrinsic string functions specific for ECL


```
cl> =trim("--abcdefg---", "-")
abcdefg
cl> =triml("--abcdefg---", "-")
abcdefg---
cl> =trimr("--abcdefg---", "-")
--abcdefg
```
