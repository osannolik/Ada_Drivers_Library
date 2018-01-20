#!/bin/bash

gprbuild runtime_issue.gpr $1

st-util &

arm-eabi-gdb --se=obj/main --command=load_run.gdbinit
