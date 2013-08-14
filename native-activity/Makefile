APPNAME = native-activity

all: bin/$(APPNAME)-debug.apk

bin/$(APPNAME)-debug.apk:
	android update project -p ./ -n $(APPNAME) -t android-10
	ndk-build
	ant debug

clean:
	rm -rf bin libs obj
	rm -f build.xml local.properties proguard-project.txt project.properties
