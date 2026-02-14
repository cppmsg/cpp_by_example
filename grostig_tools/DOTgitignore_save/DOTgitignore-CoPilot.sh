# CoPilot ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Consolidated .gitignore for C++ projects
# Targets:
# - VS Code (CMake)
# - Visual Studio 26 (native solutions/projects and CMake)
# - Qt Creator (qmake and CMake)
# - CMake standalone, Make, Autotools
# - Vim / command-line builds
# - Linux and Windows
#
# Place at repository root. Adjust build folder names if you use custom names.

# -------------------------
# OS / general
# -------------------------
# Windows
Thumbs.db
desktop.ini
# macOS
.DS_Store
.AppleDouble
.LSOverride
# Linux
.Trash-*
# Temporary and editor backup files
*~
*.swp
*.swo
*.tmp
*.temp
*.bak
*.orig

# -------------------------
# Editors / IDEs
# -------------------------
# VS Code
.vscode/
*.code-workspace
.vscode-test/

# Visual Studio (including VS 26)
.vs/
*.suo
*.user
*.userosscache
*.sln.docstates
*.VC.db
*.VC.VC.opendb
*.opendb
*.opensdf
*.sdf
*.cachefile
*.psess
*.vsp
*.vspx
# Per-user project files
*.vcxproj.user
*.vcxproj.*.user
# Keep .sln and .vcxproj tracked if you intentionally commit them

# Qt Creator
*.pro.user
*.pro.user.*
*.qmake.stash
*.qmake.cache
*.creator.user
*.creator.user.*
*.autosave
# Qt Creator build dirs (common pattern)
build-*/
# Qt compiled QML
*.qmlc

# JetBrains (CLion etc.)
.idea/
*.iml

# Vim / command-line editor
Session.vim
tags

# Other editors / IDEs
*.sublime-workspace
*.sublime-project
.project
.metadata/
.kdev4/

# -------------------------
# CMake (common)
# -------------------------
# CMake cache and generated files
CMakeCache.txt
CMakeFiles/
cmake_install.cmake
CTestTestfile.cmake
CTestTestfile.cmake.user
Testing/
Makefile.cmake
install_manifest.txt
# Common out-of-source build dirs (adjust names if you use others)
cmake-build-*/
build/
build*/
out/
bin/
lib/
lib64/
obj/
# Ninja
build.ninja
.ninja_deps
.ninja_log
# Visual Studio CMake user settings
CMakeSettings.json.user

# -------------------------
# Make / Autotools / QMake / Meson
# -------------------------
# Autotools generated
config.log
config.status
configure
configure.ac
aclocal.m4
autom4te.cache/
Makefile.in
Makefile~
m4/
ltmain.sh
libtool
*.la

# Make artifacts and generic object files
*.o
*.lo
*.obj
*.a
*.so
*.so.*
*.dylib
*.dll
*.exe
*.out
*.elf

# QMake / Qt build artifacts
Makefile.Debug
Makefile.Release
*.moc
moc_*.cpp
qrc_*.cpp
ui_*.h

# Meson
meson-logs/
meson-private/
builddir/

# -------------------------
# Compiled objects, libs, executables, symbols
# -------------------------
# Object files and modules
*.o
*.ko
*.obj
*.lo
# Static libs
*.a
*.lib
# Shared libs / dynamic
*.so
*.so.*
*.dylib
*.dll
# Executables and bundles
*.exe
*.out
*.app
# Debug / symbol files
*.pdb
*.ilk
*.dSYM/
*.su
*.sym

# -------------------------
# Test, coverage, profiling
# -------------------------
coverage/
*.gcno
*.gcda
*.gcov
*.lcov
*.prof
*.profraw
*.profdata
test-results/
*.tap
*.trx

# -------------------------
# Package managers / caches
# -------------------------
# Conan
conanbuildinfo.*
conaninfo.txt
conan.lock
conan_data/
# vcpkg
vcpkg_installed/
vcpkg/
# Hunter / CPM
_hunter/
.cpm/
# ccache
.ccache/
*.ccache

# -------------------------
# Generated sources / codegen
# -------------------------
# Protobuf / gRPC
*.pb.h
*.pb.cc
*.grpc.pb.h
*.grpc.pb.cc
# Generated directories
generated/
gen/
# SWIG
*.i
*.wrap

# -------------------------
# Logs, runtime, and misc
# -------------------------
*.log
*.trace
*.pid
*.seed
*.stackdump
*.stacktrace
*.traceback

# Secrets / env
*.pem
*.key
*.crt
*.env
.env.local
.env.*.local

.editorconfig.local

# -------------------------
# Platform-specific build folders (common names)
# -------------------------
# Visual Studio build folders
Debug/
Release/
x64/Debug/
x64/Release/
# Common CI / packaging
dist/
pkg/
*.deb
*.rpm
*.tar.gz
*.zip
*.msi
*.msm
*.msp

# -------------------------
# Misc IDE / project files you usually don't track
# -------------------------
.classpath
.metadata/
*~   # Emacs backups (already listed above)
*.sublime-project
*.sublime-workspace

# -------------------------
# Keep these tracked if intentional (uncomment to enforce)
# -------------------------
# !CMakeLists.txt
# !*.sln
# !*.vcxproj
# !README
# !LICENSE

# -------------------------
# Notes
# -------------------------
# - If you use a specific build folder name (e.g., "out", "build-debug"), add it explicitly.
# - If you commit generated artifacts intentionally, remove or override the matching patterns.
# - This file focuses on ignoring user-specific, build, and generated files across Linux and Windows.
