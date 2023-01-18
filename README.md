<!--
![sort](docs/images/sort.gif)
-->
# M_sort.f90 and associated files

## Name
   M_sort - Fortran modules for sorting

## Description
The `M_sort`(3fm) module is a collection of Fortran procedures that
do simple sorts.

## Download and Build
Just download the github repository, enter the src/ directory and run make(1):
```bash
     git clone https://github.com/urbanjost/M_sort.git
     cd M_sort/src
     # change Makefile if not using one of the listed compilers
     
     # for gfortran
     make clean
     make F90=gfortran gfortran
     
     # for ifort
     make clean
     make F90=ifort ifort

     # for nvfortran
     make clean
     make F90=nvfortran nvfortran

     # optionally
     make run  # run all the demo programs from the man-pages
     make help # see other developer options
```
This will compile the M_sort(3f) module and optionally build all the
example programs from the document pages in the example/ sub-directory
and run the unit tests.

## Supports fpm 
![fpm](docs/images/fpm_logo.gif)

#### (registered at the [fpm(1) registry](https://github.com/fortran-lang/fpm-registry) )

Alternatively, download the github repository and build it with 
fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )

```bash
     git clone https://github.com/urbanjost/M_sort.git
     cd M_sort
     fpm test  # run unit tests
```
or just list it as a dependency in your fpm.toml project file.
```toml
     [dependencies]
     M_sort        = { git = "https://github.com/urbanjost/M_sort.git" ,tag="v1.0.1"}
```
## Demo Programs 
![demos](docs/images/demo.gif)

There are demo programs extracted from the man pages in the [example/](example/) directory

## Documentation
![docs](docs/images/docs.gif)

### User 
 - A single page that uses javascript to combine all the HTML descriptions
   of the man-pages is at
   [BOOK_M_sort](https://urbanjost.github.io/M_sort/BOOK_M_sort.html).

 - An [index](https://urbanjost.github.io/M_sort/man3.html) to HTML versions
   of the man-pages 

in addition in the docs/ directory there is

 - ![man-pages](docs/images/manpages.gif)
    + [manpages.zip](https://urbanjost.github.io/M_sort/manpages.zip) 
    + [manpages.tgz](https://urbanjost.github.io/M_sort/manpages.tgz) 

 - [CHANGELOG](docs/CHANGELOG.md) provides a history of significant changes

### Developer 

 - [ford(1) output](https://urbanjost.github.io/M_sort/fpm-ford/index.html).
<!--
   - [doxygen(1) output](https://urbanjost.github.io/M_sort/doxygen_out/html/index.html).
-->
 - [github action status](docs/STATUS.md) 

## See Also

 * [Fortran Package Manager](https://github.com/fortran-lang/fpm)
 * [Fortran Wiki](http://fortranwiki.org)
 * [Fortran Discourse](https://fortran-lang.discourse.group)
 * [Google Fortran newsgroup](https://groups.google.com/forum/#!forum/comp.lang.fortran)
