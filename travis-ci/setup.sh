#!/usr/bin/env sh
sudo apt-get -qq update
sudo apt-get -qq -y install openjdk-7-jdk ant lib32z1-dev lib32stdc++6
wget http://files.metasepi.org/ajhc/android-sdk_r22.0.1-linux_4travis.tar.bz2
wget http://dl.google.com/android/ndk/android-ndk-r9-linux-x86_64.tar.bz2
tar xf android-sdk_r22.0.1-linux_4travis.tar.bz2
tar xf android-ndk-r9-linux-x86_64.tar.bz2
mv android-ndk-r9 android-ndk
