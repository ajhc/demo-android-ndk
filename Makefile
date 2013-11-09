SUBDIRS := native-activity cube
LIBDIR  := lib

all: libs demos

libs:
	$(MAKE) -C ${LIBDIR}

demos: libs
	@for i in $(SUBDIRS); do \
		$(MAKE) -C $$i; \
	done

install: all
	@for i in $(SUBDIRS); do \
		$(MAKE) -C $$i install; \
	done

clean:
	@for i in $(SUBDIRS) ${LIBDIR}; do \
		$(MAKE) -C $$i clean; \
	done

.PHONY: all clean $(SUBDIRS)
