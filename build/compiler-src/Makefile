ifeq ($(BBASE)x,x)
 $(error "You must define BBASE")
endif

ifeq ($(ARCH)x,x)
 X := $(shell uname -m)
 Y := $(shell uname -o)
 Y := $(subst /,-,$Y)
 ARCH := $X-$Y
 $(info Setting ARCH to $(ARCH))
endif

ifeq ($(ARCH),i686-Cygwin)
TARGET := meldCompiler.x86-win32
else
ifeq ($(ARCH),i686-GNU-Linux)
TARGET := meldCompiler.x86-linux
else
$(error "Makefile needs updating for this architecture: $(ARCH)")
endif
endif

build:	$(TARGET)

$(TARGET):	meldCompiler.tgz
	./build.$(ARCH)

install:	$(TARGET)
	cp $(TARGET) $(BBASE)/bin/arch-$(ARCH)/compiler.heap

clean:
	/bin/rm -rf compile util parse *.cm *.sml .cm*

reallyclean:
	/bin/rm -rf compile util parse *.cm *.sml $(TARGET)  .cm*


