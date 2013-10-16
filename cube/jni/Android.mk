# Copyright (C) 2010 The Android Open Source Project
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE    := cube
LOCAL_SRC_FILES := main.c dummy4jhc.c \
		   ../hs_build/rts/rts_support.c ../hs_build/rts/jhc_rts.c \
		   ../hs_build/lib/lib_cbits.c ../hs_build/rts/gc_jgc.c \
		   ../hs_build/rts/stableptr.c ../hs_build/rts/conc.c ../hs_build/hs_main.c
LOCAL_C_INCLUDES := hs_build/cbits hs_build
LOCAL_CFLAGS    := -std=gnu99 -D_GNU_SOURCE -falign-functions=4 -ffast-math -fno-strict-aliasing \
		   -DNDEBUG -D_JHC_GC=_JHC_GC_JGC -D_JHC_CONC=_JHC_CONC_PTHREAD -D_JHC_USE_OWN_STDIO
LOCAL_LDLIBS    := -llog -landroid -lEGL -lGLESv1_CM
LOCAL_ARM_MODE  := arm
LOCAL_STATIC_LIBRARIES := android_native_app_glue

include $(BUILD_SHARED_LIBRARY)

$(call import-module,android/native_app_glue)
