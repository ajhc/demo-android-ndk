# Demo Ajhc apps on Android NDK [![Build Status](https://travis-ci.org/ajhc/demo-android-ndk.png)](https://travis-ci.org/ajhc/demo-android-ndk)

The demo needs Ajhc 0.8.0.8 or later.

## How to build

### Install JDK and Ant

    $ sudo apt-get install openjdk-7-jdk ant

### Setup android SDK and NDK

Download android SDK and NDK on following links.

* http://developer.android.com/sdk/index.html
* http://developer.android.com/tools/sdk/ndk/index.html

Unpack them and setting PATH for SDK/NDK.

    $ cd $HOME
    $ wget http://dl.google.com/android/android-sdk_r22-linux.tgz
    $ wget http://dl.google.com/android/ndk/android-ndk-r9-linux-x86_64.tar.bz2
    $ tar xf android-sdk_r22-linux.tgz
    $ tar xf android-ndk-r9-linux-x86_64.tar.bz2
    $ mv android-ndk-r9 android-ndk
    $ export PATH=$HOME/android-ndk:$HOME/android-sdk-linux/sdk/tools:$HOME/android-sdk-linux/sdk/platform-tools:$PATH

### Install Ajhc

    $ sudo apt-get install haskell-platform gcc make m4
    $ cabal install ajhc

### Build

Type "make".

    $ cd demo-android-ndk
    $ make
    $ file */bin/*debug.apk
    cube/bin/cube-debug.apk:                                 Java Jar file data (zip)
    native-activity/bin/native-activity-debug.apk:           Java Jar file data (zip)


## How to install

Connect your PC and Android Phone using USB cable.

    $ sudo adb devices
    List of devices attached 
    SHTBB081500     device

The adb command find your phone. Then install the apk file.

    $ sudo adb install -r cube/bin/cube-debug.apk
    956 KB/s (14555 bytes in 0.014s)
            pkg: /data/local/tmp/cube-debug.apk
    Success

And touch your Android Phone and run the application.
