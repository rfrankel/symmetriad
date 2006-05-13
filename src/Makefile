### ----------------------------------------------------------------------
### Copyright 2005 Alexey Radul and Rebecca Frankel.
### ----------------------------------------------------------------------
### This file is part of The Symmetriad.
### 
### The Symmetriad is free software; you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2 of the License, or
### (at your option) any later version.
### 
### The Symmetriad is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
### 
### You should have received a copy of the GNU General Public License
### along with The Symmetriad; if not, write to the Free Software
### Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
### ----------------------------------------------------------------------

DIRLIST = constraint/ graphics/ group/ utils/ ./

COMFILES = $(wildcard $(patsubst %,%*.com,$(DIRLIST)))
BINFILES = $(wildcard $(patsubst %,%*.bin,$(DIRLIST)))
BCIFILES = $(wildcard $(patsubst %,%*.bci,$(DIRLIST)))
EXTFILES = $(wildcard $(patsubst %,%*.ext,$(DIRLIST)))

.PHONY: all clean clean-com clean-bin clean-bci clean-ext tar

all:
	@echo "Default command is empty to avoid unfortunate accidents."

clean: clean-com clean-bci clean-bin clean-ext

clean-com:
ifneq ($(strip $(COMFILES)),)
	rm $(COMFILES)
endif

clean-bin:
ifneq ($(strip $(BINFILES)),)
	rm $(BINFILES)
endif

clean-bci:
ifneq ($(strip $(BCIFILES)),)
	rm $(BCIFILES)
endif

clean-ext:
ifneq ($(strip $(EXTFILES)),)
	rm $(EXTFILES)
endif

# This is not build but release, and it no longer functions
# quite correctly (esp. the contents of root/tools)
# SCMFILES = $(wildcard $(patsubst %,%*.scm,$(DIRLIST)))
# PLFILES = $(wildcard $(patsubst %,%*.pl,$(DIRLIST)))
# TXTFILES = $(wildcard $(patsubst %,%*.txt,$(DIRLIST)))
# ONEOFFS = init.gcl init-conf.gcl Makefile
# SOURCES = $(SCMFILES) $(PLFILES) $(TXTFILES) $(ONEOFFS)

# TAREXCLUDES = ./todo.txt Makefile
# TARSRC = $(filter-out $(TAREXCLUDES),$(SOURCES))

# tar: symmetriad.tar

# symmetriad.tar: $(TARSRC)
# 	tar -cvvf symmetriad.tar $^
