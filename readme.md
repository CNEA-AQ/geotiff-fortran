# GeoTIFF Fortran Module

> This is just a very minimalistic "home-made" GeoTIFF procesor module for Fortran. Sintaxis is inspired from the LIBTIFF library.


## Add the module to your code:

Just add to your code the `use` call:

```fortran
   use GeoTIFF
```


## List of functions available and it's sintax:

Open/close file:

[x] `TIFF_Open    ` 
[x] `TIFF_Close   `

Read commands:
[x] `TIFF_Get_Field` <!--TIFF_GetTagValue-->
[ ] `TIFF_Get_Image`

Write commands:
[ ] `TIFF_Set_Field`
[ ] `TIFF_Set_Image`

Structures/Types:
[ ] `TIFF_FILE`
[ ] `TIFF_IFD`
[ ] `TIFF_TAG` 


## To-do list:

[ ] Add `swap` function to support both endianess types
[ ]




