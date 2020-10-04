# Prerequisites

* Install opam

* `opam init --bare`
  
    I said y to the prompt but also added the line it displays into .bashrc:
  
    `. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true`

* `opam switch create ocaml-base-compiler.4.11.0`

* `opam install core async graphics async_shell js_of_ocaml-lwt js_of_ocaml-ppx angstrom`
  and whetever else is listed in libraries in the dune files.

* For poly/ you'll need to install OpenCL drivers. Below are
  instructions for OpenSuSE with an Intel GPU.

  + In Software Management add repo
    http://download.opensuse.org/repositories/home:/linnaea:/Intel-OpenCL/openSUSE_Leap_15.1/

  + install
    opencl-headers
    libOpenCL1
    libigdfcl1
    libigdrcl
    intel-opencl-neo

    Make sure to not install beignet - it exhibited some fascinating bugs like
    computing only some stripes of the data.

  + ln -s /usr/lib64/libOpenCL.so.1 /usr/lib64/libOpenCL.so

# How to run

Most things can be run by going to the directory you want to run, and running

```
make
sudo su
./start.sh
firefox http://localhost/index-server.html
```

You should be able to use things that need microphone without microphone by
choosing the correct 'Monitor' interface under 'Recording' in pavucontrol.

# Tasters

## spark/hex
[![Hex 1](https://img.youtube.com/vi/5e8FJhhpSPo/maxresdefault.jpg)](https://www.youtube.com/watch?v=5e8FJhhpSPo)

## spark/window

[![Window](https://img.youtube.com/vi/K_6M-9U1NGE/maxresdefault.jpg)](https://www.youtube.com/watch?v=K_6M-9U1NGE)

## spark/wall

[![Wall](https://img.youtube.com/vi/6cONcKzx4GY/maxresdefault.jpg)](https://www.youtube.com/watch?v=6cONcKzx4GY)

## spark/fred

[![Fred](https://img.youtube.com/vi/VhBzc3j_vPA/maxresdefault.jpg)](https://www.youtube.com/watch?v=VhBzc3j_vPA)
