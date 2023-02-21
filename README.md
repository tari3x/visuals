# Prerequisites

* `opam init`
  
    Added the line it displays into .bashrc:
    
    `eval $(opam env)`

* `opam install core async graphics async_shell js_of_ocaml-lwt js_of_ocaml-ppx angstrom tgls tsdl camlimages tgls tsdl`
  and whetever else is listed as libraries in the dune files.

* poly/ is bitrotten at the moment.

  For poly/ you'll need to install maxima and OpenCL drivers. Below are
  instructions for OpenSuSE 15.1 with an Intel GPU.

  + In Software Management add repo
    http://download.opensuse.org/repositories/home:/linnaea:/Intel-OpenCL/openSUSE_Leap_15.1/

  + install
    opencl-headers
    libOpenCL1
    libigdfcl1
    intel-opencl-neo
    maxima

    Make sure to not install beignet - it exhibited some fascinating bugs like
    computing only some stripes of the data.

    Do not install intel-opencl, it causes "error building program
    OUT_OF_HOST_MEMORY."

  + ln -s /usr/lib64/libOpenCL.so.1 /usr/lib64/libOpenCL.so

* For http-server:
  + install npm
  + `sudo npm install -g http-server`

# How to run

Most things can be run by going to the directory you want to run, and running

```
make
sudo su
./start.sh
google-chrome https://localhost/index-server.html
```

You should be able to use things that need microphone without microphone by
choosing the correct 'Monitor' interface under 'Recording' in pavucontrol.

We have to use https because chrome doesn't allow to use microphone otherwise,
but you have to click through the bad certificate warning.

# Tasters

## spark/hex
[![Hex 1](https://img.youtube.com/vi/5e8FJhhpSPo/maxresdefault.jpg)](https://www.youtube.com/watch?v=5e8FJhhpSPo)

## spark/window

[![Window](https://img.youtube.com/vi/K_6M-9U1NGE/maxresdefault.jpg)](https://www.youtube.com/watch?v=K_6M-9U1NGE)

## spark/wall

[![Wall](https://img.youtube.com/vi/6cONcKzx4GY/maxresdefault.jpg)](https://www.youtube.com/watch?v=6cONcKzx4GY)

## spark/fred

[![Fred](https://img.youtube.com/vi/VhBzc3j_vPA/maxresdefault.jpg)](https://www.youtube.com/watch?v=VhBzc3j_vPA)
