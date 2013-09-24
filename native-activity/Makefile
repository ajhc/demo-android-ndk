APPNAME = native-activity
ANDROID_TARGET = android-10

CDIR = jni
CSRC = $(wildcard $(CDIR)/*.c $(CDIR)/*.h)

HSBUILD = hs_build
HSDIR = hs_src
HSSRC = $(wildcard $(HSDIR)/*.hs $(HSDIR)/*/*.hs $(HSDIR)/*/*/*.hs $(HSDIR)/*/*/*/*.hs)

.PHONY: all clean install
all: bin/$(APPNAME)-debug.apk

bin/$(APPNAME)-debug.apk: $(HSBUILD)/hs_main.c $(CSRC)
	android update project -p ./ -n $(APPNAME) -t $(ANDROID_TARGET)
	ndk-build NDK_DEBUG=1
	ant debug

$(HSBUILD)/hs_main.c: $(HSSRC)
	mkdir -p $(HSBUILD)
	ajhc -fffi -fpthread --include=hs_src --tdir=$(HSBUILD) -C -o $@ $(HSDIR)/Main.hs

clean:
	rm -rf bin libs obj $(HSBUILD)
	rm -f build.xml local.properties proguard-project.txt project.properties

install: all
	sudo adb devices
	sudo adb install -r bin/$(APPNAME)-debug.apk
