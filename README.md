![string](docs/images/yarnball.gif)
# M_sort.f90 and associated files

## NAME
   M_sort - Fortran modules for sorting

## DESCRIPTION
The `M_sort`(3fm) module is a collection of Fortran procedures that
do simple sorts.

## DOWNLOAD AND BUILD
Just download the github repository, enter the src/ directory and run make(1):

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
     make test # run the unit tests
     make run  # run all the demo programs from the manpages
     make help # see other developer options

This will compile the M_sort(3f) module and optionally build all the
example programs from the document pages in the example/ sub-directory
and run the unit tests.

## SUPPORTS FPM 
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

## DEMO PROGRAMS

There are demo programs extracted from the man pages in the example/ directory


## USER DOCUMENTATION

 - The routines are summarized in the following section below.

in addition in the docs/ directory there is

 - manpages in 
    + [manpages.zip](https://urbanjost.github.io/M_sort/manpages.zip) 
    + [manpages.tgz](https://urbanjost.github.io/M_sort/manpages.tgz) 

 - An [index](https://urbanjost.github.io/M_sort/man3.html) to HTML versions
   of the manpages 

 - A single page that uses javascript to combine all the HTML descriptions
   of the manpages is at
   [BOOK_FORTRAN](https://urbanjost.github.io/M_sort/BOOK_M_sort.html).

## RESPONSES

Discussion is welcome here as well as at
 - [Fortran Discourse](https://fortran-lang.discourse.group)
 - [Fortran Wiki](http://fortranwiki.org)
 - [Google Fortran newsgroup](https://groups.google.com/forum/#!forum/comp.lang.fortran)
