APPNAME = native-activity
ANDROID_TARGET = android-10

HSBUILD = hs_build
HSDIR = hs_src
HSSRC = $(wildcard $(HSDIR)/*.hs $(HSDIR)/*/*.hs $(HSDIR)/*/*/*.hs $(HSDIR)/*/*/*/*.hs)

.PHONY: all clean install
all: bin/$(APPNAME)-debug.apk

bin/$(APPNAME)-debug.apk: $(HSBUILD)/hs_main.c
	android update project -p ./ -n $(APPNAME) -t $(ANDROID_TARGET)
	ndk-build
	ant debug

$(HSBUILD)/hs_main.c: $(HSSRC)
	mkdir -p $(HSBUILD)
	ajhc -fffi -fpthread --include=hs_src --tdir=$(HSBUILD) -C -o $@ $<

clean:
	rm -rf bin libs obj $(HSBUILD)
	rm -f build.xml local.properties proguard-project.txt project.properties

install: all
	sudo adb devices
	sudo adb install -r bin/$(APPNAME)-debug.apk
