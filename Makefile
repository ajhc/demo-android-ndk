SUBDIRS := native-activity cube
LIBDIR  := lib

all: libs demos

libs:
	$(MAKE) -C ${LIBDIR}

demos: libs
	@for i in $(SUBDIRS); do \
		$(MAKE) -C $$i; \
	done

clean:
	@for i in $(SUBDIRS); do \
		$(MAKE) -C $$i clean; \
	done
	$(MAKE) -C ${LIBDIR} clean

.PHONY: all clean $(SUBDIRS)
