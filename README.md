[![](docs/images/sort.gif)](https://urbanjost.github.io/M_sort/fpm-ford/index.html)
# [M_sort](https://urbanjost.github.io/M_sort/man3.html)

## Name
   M_sort - Fortran modules for sorting

## Description
The `M_sort`(3fm) module is a collection of Fortran procedures that
do simple sorts.

---
![gmake](docs/images/gnu.gif)
---
## Building the Module using make(1)
Just download the github repository, enter the src/ directory and run make(1):
```bash
     git clone https://github.com/urbanjost/M_sort.git
     cd M_sort/src
     # change Makefile if not using one of the listed compilers

     # for gfortran
     make clean
     make gfortran

     # for ifort
     make clean
     make ifort

     # for nvfortran
     make clean
     make nvfortran

     # optionally
     make run  # run all the demo programs from the man-pages
     make help # see other developer options
```
This will compile the M_sort(3f) module and optionally build all the
example programs from the document pages in the example/ sub-directory
and run the unit tests.

---
![-](docs/images/fpm_logo.gif)
---
## Build and Test with FPM

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
---
![-](docs/images/cmake.png)
---
## Build and Test with CMake
```bash
      git clone https://github.com/urbanjost/M_sort.git
      cd M_sort

      # Create a Build Directory:
      mkdir -p build

      cd build
      cmake -S ../src -B .

      # Configure the Build, specifying your preferred compiler (ifort, flang, etc.):
      cmake . -DCMAKE_Fortran_COMPILER=gfortran

      # Build the Project:
      cmake --build .
```
### Optional CMake section:
```bash
      # Verify build
      # On Linux this would create, for example:
      ls build/lib/libM_sort.a   # the static library
      ls build/include/*.mod     # module files
      ls build/test/*            # test executables
      ls build/example/*         # example executables

      #Optionally Run Tests and Examples:
      for name in ./test/* ./example/*
      do
         $name
      done

      #Install (Optional):
      # This installs the library and module files to the system
      # (e.g., /usr/local/lib/ and /usr/local/include/).
      cmake --install .

      # if you have insufficient permissions sudo(1) may be required
      # to perform the install
      #sudo cmake --install .

      # Verify installation
      ls /usr/local/lib/libM_sort.a
      ls /usr/local/include/*.mod

      # Cleaning Up: To clean artifacts, remove the build/ directory:
      rm -rf build
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
