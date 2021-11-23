#!/bin/bash

f(){
  exec sml ~/Projects/smlc/top/sources.cm ~/Projects/smlc/top/finish.sml "$@" > /dev/null;
  unset -f f;
};

f $@
