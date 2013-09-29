/*
 * Copyright (C) 2010 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

//BEGIN_INCLUDE(all)
#include "c_extern.h"

// Should be called from Haskell world.
#define LOGI(...) ((void)__android_log_print(ANDROID_LOG_INFO, "native-activity", __VA_ARGS__))
#define LOGW(...) ((void)__android_log_print(ANDROID_LOG_WARN, "native-activity", __VA_ARGS__))

/**
 * Initialize an EGL context for the current display.
 */
extern int engineInitDisplay(struct engine* engine);
int engine_init_display(struct engine* engine) {
	return engineInitDisplay(engine);
}

/**
 * Just the current frame in the display.
 */
extern void engineDrawFrame(struct engine* engine);
void engine_draw_frame(struct engine* engine) {
	engineDrawFrame(engine);
}

/**
 * Tear down the EGL context currently associated with the display.
 */
extern void engineTermDisplay(struct engine* engine);
void engine_term_display(struct engine* engine) {
	engineTermDisplay(engine);
}

/**
 * Process the next input event.
 */
extern int32_t engineHandleInput(struct engine* engine, AInputEvent* event);
int32_t engine_handle_input(struct android_app* app, AInputEvent* event) {
    struct engine* engine = (struct engine*)app->userData;
    return engineHandleInput(engine, event);
}

/**
 * Process the next main command.
 */
extern void engineHandleCmd(struct engine* engine, int32_t cmd);
void engine_handle_cmd(struct android_app* app, int32_t cmd) {
    struct engine* engine = (struct engine*)app->userData;
    engineHandleCmd(engine, cmd);
}

/**
 * This is the main entry point of a native application that is using
 * android_native_app_glue.  It runs in its own thread, with its own
 * event loop for receiving input events and doing other things.
 */
extern void androidMain(struct android_app* state);
void android_main(struct android_app* state) {
	// Make sure glue isn't stripped.
	app_dummy();

	{ // Init Haskell code.
		int hsargc = 1;
		char *hsargv = "q";
		char **hsargvp = &hsargv;

		hs_init(&hsargc, &hsargvp);
		androidMain(state);
		hs_exit();
	}
}
//END_INCLUDE(all)
