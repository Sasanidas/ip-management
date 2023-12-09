# I(nferior)P(process) management

IP-management is a library for controling background process, it defines an common interface that others libraries/systems can use
to manipulate standard procedures like input/output and signals.

It uses the CL library https://iolib.common-lisp.dev/ to interact with the underlying process, so the OS support is limited to Linux and BSD
(for more information look at the OS support  on the iolib webpage).


## Installation

#### Quicklisp
Clone the library on `~/quicklisp/local-projects` or `~/common-lisp/`.

Now it should be visible to Quicklisp

```lisp
    (ql:quickload :ip-management)
```


## Usage
The interface documentation can be read from the interface file `src/interface.lisp` which explains
the interface calls.

The library includes an implementation of a process management on `src/process.lisp`.
