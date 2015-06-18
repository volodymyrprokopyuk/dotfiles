#!/bin/bash
#
# introduce em command for emacs client mode
# chmod u+x em
#
exec /usr/bin/env emacsclient -c -a "" $*
