Prerequisites
-------------

* Install opam

* `opam init`
  
    I said y to the prompt but also added the line it displays into .bashrc:
  
    `. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true`

* `opam switch ocaml-base-compiler.4.07.1`

* `opam update`

* `opam install PACKAGES`
  where packages are whatever is listed in libraries in the dune files.

* It's quite likely you'll need to install OpenCL drivers before it runs. Below
  are instructions for OpenSuSE with an Intel GPU.

  + In Software Management add repo
    http://download.opensuse.org/repositories/home:/linnaea:/Intel-OpenCL/openSUSE_Leap_15.0/

  + install
    libigdfcl
    libigdrcl

    Make sure to not install beignet.

How to run
-----------

Most things can be run by going to the directory you want to run, and running

```
make
sudo su
./start.sh
firefox http://localhost/index-server.html
```

You should be able to use things that need microphone without microphone by
choosing the correct 'Monitor' interface under 'Recording' in pavucontrol.

