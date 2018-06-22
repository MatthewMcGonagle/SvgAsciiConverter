# SvgAsciiConverter

The purpose of this library is to make a haskell library for converting a .svg vector graphics file to
an ASCII text picture. Since .svg graphics are scalable, the user is free to choose a scale to use for the
final ASCII picture.

The library currently supports the conversion of rectangles in the main layer of an .svg file. For example,
consider the following .svg picture of the Haskell logo as rectangles:

![HaskellLogo.svg](HasekellLogo.svg)

This can be turned into the following ASCII output:
```

      RRRRRRRR
      RRRRRRRR   ////////////
      RRRRRRRR   ////////////
      RRRRRRRR   ////////////
      RRRRRRRR   ////////////
       RRRRRRRR  ////////////
       RRRRRRRR  ////////////
       RRRRRRRR  ////////////
       RRRRRRRR
       RRRRRRRR       ////////////
                      ////////////
           RRRRRRRR   ////////////
           RRRRRRRR   ////////////
           RRRRRRRR   ////////////
           RRRRRRRR   ////////////
           RRRRRRRR   ////////////

              RRRRRRRR      ////////////
              RRRRRRRR      ////////////
              RRRRRRRR      ////////////
              RRRRRRRR      ////////////
              RRRRRRRR      ////////////
                            ////////////       *****  *****
                  RRRRRRRR  ////////////       *****  *****   *****   *****  *****   *****  *****
                  RRRRRRRR                     *****  *****   *****   *****  *****   *****  *****
                  RRRRRRRR                     *****  *****   *****   *****  *****   *****  *****
                  RRRRRRRR     ////////////    *****  *****   *****   *****  *****   *****  *****
                  RRRRRRRR     ////////////    *****  *****   *****   *****  *****   *****  *****
                               ////////////                   *****   *****  *****   *****  *****
                     RRRRRRRR  ////////////
                     RRRRRRRR  ////////////
                     RRRRRRRR  ////////////
                     RRRRRRRR  ////////////
                     RRRRRRRR                                                  *****  *****   *****
                                           ////////////  *****  *****   *****  *****  *****   *****
               RRRRRRRR   ////////////     ////////////  *****  *****   *****  *****  *****   *****
               RRRRRRRR   ////////////     ////////////  *****  *****   *****  *****  *****   *****
               RRRRRRRR   ////////////     ////////////  *****  *****   *****  *****  *****   *****
               RRRRRRRR   ////////////     ////////////  *****  *****   *****  *****  *****   *****
               RRRRRRRR   ////////////     ////////////  *****  *****   *****
                          ////////////     ////////////
         RRRRRRRR         ////////////
         RRRRRRRR
         RRRRRRRR  ////////////                  ////////////
         RRRRRRRR  ////////////                  ////////////
         RRRRRRRR  ////////////                  ////////////
                   ////////////                  ////////////
    RRRRRRRR       ////////////                  ////////////
    RRRRRRRR       ////////////                  ////////////
    RRRRRRRR       ////////////                  ////////////
    RRRRRRRR
    RRRRRRRR
              ////////////                            ////////////
RRRRRRRR      ////////////                            ////////////
RRRRRRRR      ////////////                            ////////////
RRRRRRRR      ////////////                            ////////////
RRRRRRRR      ////////////                            ////////////
RRRRRRRR      ////////////                            ////////////
              ////////////                            ////////////
```
