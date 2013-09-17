APPNAME = native-activity
ANDROID_TARGET = android-10

HSDIR = hs_build

.PHONY: all clean install
all: bin/$(APPNAME)-debug.apk

bin/$(APPNAME)-debug.apk: $(HSDIR)/hs_main.c
	android update project -p ./ -n $(APPNAME) -t $(ANDROID_TARGET)
	ndk-build
	ant debug

$(HSDIR)/hs_main.c: hs_src/Main.hs
	mkdir -p $(HSDIR)
	ajhc -fffi -fpthread --include=hs_src --tdir=$(HSDIR) -C -o $@ $<

clean:
	rm -rf bin libs obj $(HSDIR)
	rm -f build.xml local.properties proguard-project.txt project.properties

install: all
	sudo adb devices
	sudo adb install -r bin/$(APPNAME)-debug.apk
