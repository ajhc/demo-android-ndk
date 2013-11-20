APPNAME = cube
ANDROID_TARGET = android-10

CDIR = jni
CSRC = $(wildcard $(CDIR)/*.c $(CDIR)/*.h)

HSBUILD = hs_build
HSDIR = hs_src
HSSRC = $(wildcard $(HSDIR)/*.hs $(HSDIR)/*/*.hs $(HSDIR)/*/*/*.hs $(HSDIR)/*/*/*/*.hs)
HSLIBDIR = ../lib

.PHONY: all release clean install
all: bin/$(APPNAME)-debug.apk
release: bin/$(APPNAME).apk

bin/$(APPNAME)-debug.apk: $(HSBUILD)/hs_main.c $(CSRC)
	android update project -p ./ -n $(APPNAME) -t $(ANDROID_TARGET)
	ndk-build NDK_DEBUG=1
	ant debug

bin/$(APPNAME).apk: $(HSBUILD)/hs_main.c $(CSRC)
	android update project -p ./ -n $(APPNAME) -t $(ANDROID_TARGET)
	ndk-build
	ant release

$(HSBUILD)/hs_main.c: $(HSSRC)
	mkdir -p $(HSBUILD)
	ajhc -fffi -fpthread -L ${HSLIBDIR} -p android-ndk --include=hs_src --tdir=$(HSBUILD) -C -o $@ $(HSDIR)/Main.hs

clean:
	rm -rf bin libs obj $(HSBUILD)
	rm -f build.xml local.properties proguard-project.txt project.properties

install: all
	sudo adb devices
	sudo adb install -r bin/$(APPNAME)-debug.apk
