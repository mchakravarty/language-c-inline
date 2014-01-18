This example application implements a simple GUI around interactive GHC sessions.

The GUI is done with Xcode (via a .xib). The corresponding Xcode project is in the subdirectory `HSApp-xcode-proj`. This, however, is not used to build the project. Instead, we have got a simple Makefile, that invokes GHC and clang to compile the various components, and finally, copies the binary into a pre-populated `.app` bundle.

Currently, if you change the `.xib` in the Xcode project, you need to manually copy it into the appropriate location in the `.app` bundle.
