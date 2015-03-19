# Requirements #

  * Visual Studio 2010 or later
  * Windows SDK 7 or later (If you are using Visual Studio 2010)
  * CMake 2.8.4 or later


# Steps #

  1. Run '`Visual Studio Command Prompt (2010)`' or similar terminal
  1. Run '`cmake-gui.exe`' on the same terminal
  1. Set source code directory and build directory
  1. Press '`Configure`' button
  1. On the dialog, select '`NMake Makefiles`' and press '`Finish`'
  1. If your environment doesn't have Boehm GC library then following steps are needed
    1. check '`enable_parallel_mark`'
    1. check '`enable_threads`'
  1. Uncheck '`DEBUG_VERSION`' (optional)
  1. Press '`Configure`' again
  1. Press '`Generate`'
  1. Go to the build directory you choose on the terminal
  1. Run '`nmake`'