# GIFLIB - an OCaml GIF library

An OCaml library that supports reading and writing of GIF files. Differs from most other image libraries in that it exposes the palette based nature of GIF files rather than just working with an array of RGB pixels.

## Credits

This library is based on the work by [Jan Dudek](https://github.com/jdudek), and the reworked by [Michael Dales](https://github.com/mdales/) to update it to modern OCaml and rework the APIs to be more palette based.

Original README by Jan:

> This is my part of a project written in 2008 for Functional Programming course. It is implementation of GIF image
> format. The whole project was a converter beetwen GIF and BMP, with ability to apply some filters. The filters, color
> quantization and BMP implementation were not written by me, so I don't include them here.
>
> Licensed under MIT license.
