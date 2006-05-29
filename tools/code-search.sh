#!/bin/bash

find . -name "*.scm" | xargs grep $@
