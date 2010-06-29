[Kappa](http://kappalanguage.org/) - The Kappa language for Systems Biology
================================

Basic information
---------------------------------------

Compiling kappa creates two biraries, simplx and complx. simplx is for simulation, complx is for static analysis. 


What you need to compile Kappa
---------------------------------------

* OCaml 3.09.2
[http://caml.inria.fr/download.en.html](OCaml download page)
* TK/lablTK (for the graphical interface)
* graphviz
[http://www.graphviz.org/](graphviz.org)


How to compile Kappa
-----------------------------

In the main directory of the distribution (the one that this file is in), type
the following to make all versions of jQuery:

To compile the light version, which does not include the graphical interface (LablTK not required):

`make`

Or to include the graphical interface:

`make full`

Binaries are created in /bin.
To copy them into /usr/bin:

`make install`

If you do not have local root privileges, you may use the following: 
`make LOCAL_DIR="/home/bin" install_in_local`


