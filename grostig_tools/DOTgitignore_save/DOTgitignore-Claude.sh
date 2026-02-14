# Claude ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ==============================================================================
# Comprehensive C++ .gitignore
# ==============================================================================
# Covers: CMake, Visual Studio, VS Code, Qt Creator (qmake/CMake),
#         Make, Autotools, Vim - Linux & Windows platforms
# ==============================================================================

# ------------------------------------------------------------------------------
# C++ Compiled Object Files and Libraries
# ------------------------------------------------------------------------------
*.o
*.obj
*.ko
*.elf
*.a
*.lib
*.so
*.so.*
*.dylib
*.dll
*.exe
*.out
*.app

# ------------------------------------------------------------------------------
# C++ Precompiled Headers
# ------------------------------------------------------------------------------
*.gch
*.pch

# ------------------------------------------------------------------------------
# Debug and Symbol Files
# ------------------------------------------------------------------------------
*.pdb
*.ilk
*.idb
*.iobj
*.ipdb
*.exp
*.dSYM/
*.su
*.stackdump
core
core.*

# ------------------------------------------------------------------------------
# CMake Build System
# ------------------------------------------------------------------------------
CMakeCache.txt
CMakeFiles/
CMakeScripts/
cmake_install.cmake
install_manifest.txt
compile_commands.json
CTestTestfile.cmake
_deps/
*.cmake
!CMakeLists.txt
!*Config.cmake
!*ConfigVersion.cmake
!*Targets.cmake

# CMake build directories (common patterns)
build/
Build/
builds/
cmake-build-*/
out/
Out/

# ------------------------------------------------------------------------------
# Visual Studio (Native Solutions and CMake)
# ------------------------------------------------------------------------------

# User-specific files
*.rsuser
*.suo
*.user
*.userosscache
*.sln.docstates

# User-specific directories
.vs/
.vscode/launch.json
.vscode/tasks.json

# Build results
[Dd]ebug/
[Dd]ebugPublic/
[Rr]elease/
[Rr]eleases/
x64/
x86/
[Ww][Ii][Nn]32/
[Aa][Rr][Mm]/
[Aa][Rr][Mm]64/
bld/
[Bb]in/
[Oo]bj/
[Ll]og/
[Ll]ogs/

# Visual Studio profiler
*.psess
*.vsp
*.vspx
*.sap

# Visual Studio Trace Files
*.e2e

# TFS 2012 Local Workspace
$tf/

# ReSharper
_ReSharper*/
*.[Rr]e[Ss]harper
*.DotSettings.user

# TeamCity
_TeamCity*

# NCrunch
_NCrunch_*
.*crunch*.local.xml
nCrunchTemp_*

# Visual Studio code coverage results
*.coverage
*.coveragexml

# MSTest test Results
[Tt]est[Rr]esult*/
[Bb]uild[Ll]og.*

# NUnit
*.VisualState.xml
TestResult.xml
nunit-*.xml

# Build Results of an ATL Project
[Dd]ebugPS/
[Rr]eleasePS/
dlldata.c

# Benchmark Results
BenchmarkDotNet.Artifacts/

# .NET Core
project.lock.json
project.fragment.lock.json
artifacts/

# ASP.NET Scaffolding
ScaffoldingReadMe.txt

# Files built by Visual Studio
*_i.c
*_p.c
*_h.h
*.meta
*.iobj
*.ipdb
*.pgc
*.pgd
*.rsp
*.sbr
*.tlb
*.tli
*.tlh
*.tmp
*.tmp_proj
*_wpftmp.csproj
*.log
*.tlog
*.vspscc
*.vssscc
.builds
*.pidb
*.svclog
*.scc

# Visual C++ cache files
ipch/
*.aps
*.ncb
*.opendb
*.opensdf
*.sdf
*.cachefile
*.VC.db
*.VC.VC.opendb

# Visual Studio profiler
*.psess
*.vsp
*.vspx
*.sap

# Visual Studio Trace Files
*.e2e

# Installshield output folder
[Ee]xpress/

# DocProject
DocProject/buildhelp/
DocProject/Help/*.HxT
DocProject/Help/*.HxC
DocProject/Help/*.hhc
DocProject/Help/*.hhk
DocProject/Help/*.hhp
DocProject/Help/Html2
DocProject/Help/html

# Click-Once directory
publish/

# Publish Web Output
*.[Pp]ublish.xml
*.azurePubxml
*.pubxml
*.publishproj

# Microsoft Azure Web App publish settings
PublishScripts/

# NuGet Packages
*.nupkg
*.snupkg
**/[Pp]ackages/*
!**/[Pp]ackages/build/
*.nuget.props
*.nuget.targets

# Windows Store app package directories and files
AppPackages/
BundleArtifacts/
Package.StoreAssociation.xml
_pkginfo.txt
*.appx
*.appxbundle
*.appxupload

# Visual Studio cache files
*.suo
*.user
*.sln.docstates

# Windows image file caches
Thumbs.db
ehthumbs.db

# Folder config file
Desktop.ini

# Recycle Bin
$RECYCLE.BIN/

# VS Code directories
.vscode/
!.vscode/settings.json
!.vscode/tasks.json
!.vscode/launch.json
!.vscode/extensions.json
!.vscode/*.code-snippets
*.code-workspace

# Local History for Visual Studio Code
.history/

# Built Visual Studio Code Extensions
*.vsix

# ------------------------------------------------------------------------------
# Qt Creator (qmake and CMake)
# ------------------------------------------------------------------------------
*.pro.user
*.pro.user.*
*.qbs.user
*.qbs.user.*
*.moc
moc_*.cpp
moc_*.h
qrc_*.cpp
ui_*.h
*.qmlc
*.jsc
Makefile*
*build-*
*.autosave
.qmake.stash

# Qt Creator local configuration
qtcreator-*.pro.user*

# Qt Creator CMake
CMakeLists.txt.user*

# QtCreator 4.8< compilation database
compile_commands.json

# QtCreator local machine specific files for imported projects
*creator.user*

*_qmlcache.qrc

# ------------------------------------------------------------------------------
# GNU Make
# ------------------------------------------------------------------------------
Makefile
!*/Makefile
!Makefile.am
!Makefile.in
*.make
.depend

# ------------------------------------------------------------------------------
# GNU Autotools
# ------------------------------------------------------------------------------
/autom4te.cache
/aclocal.m4
/compile
/configure
/depcomp
/install-sh
/missing
/stamp-h1
/config.guess
/config.sub
/config.log
/config.status
/libtool
/ltmain.sh
*.la
*.lo
.deps/
.libs/
Makefile.in
*.m4
!m4/

# ------------------------------------------------------------------------------
# Vim Editor
# ------------------------------------------------------------------------------
[._]*.s[a-v][a-z]
!*.svg
[._]*.sw[a-p]
[._]s[a-rt-v][a-z]
[._]ss[a-gi-z]
[._]sw[a-p]
Session.vim
Sessionx.vim
.netrwhist
*~
tags
[._]*.un~

# ------------------------------------------------------------------------------
# Emacs Editor
# ------------------------------------------------------------------------------
*~
\#*\#
/.emacs.desktop
/.emacs.desktop.lock
*.elc
auto-save-list
tramp
.\#*

# ------------------------------------------------------------------------------
# Common C++ Package Managers
# ------------------------------------------------------------------------------

# Conan
conan.lock
conanbuildinfo.*
conaninfo.txt
graph_info.json
.conan/

# vcpkg
vcpkg_installed/
vcpkg/
.vcpkg-root

# ------------------------------------------------------------------------------
# Documentation
# ------------------------------------------------------------------------------
/doc/html/
/doc/latex/
Doxyfile.bak
doxygen_sqlite3.db

# ------------------------------------------------------------------------------
# Testing Frameworks
# ------------------------------------------------------------------------------
# Google Test
googletest/
gtest/
gmock/

# Catch2
catch2/

# Test outputs
*.test
test_results/
test_output/
*.gcov
*.gcda
*.gcno

# Coverage
*.coverage
coverage/
lcov.info
*.profdata
*.profraw

# ------------------------------------------------------------------------------
# Sanitizer Outputs
# ------------------------------------------------------------------------------
*.dSYM
*.asan
*.msan
*.tsan
*.ubsan

# ------------------------------------------------------------------------------
# Static Analysis
# ------------------------------------------------------------------------------
.clang-tidy
.clang-format
compile_commands.json
.cache/
.ccls-cache/

# ------------------------------------------------------------------------------
# IDEs and Editors (Additional)
# ------------------------------------------------------------------------------
.idea/
*.iml
*.swp
*.swo
.DS_Store
*.sublime-project
*.sublime-workspace
.project
.cproject
.settings/

# ------------------------------------------------------------------------------
# OS-Specific
# ------------------------------------------------------------------------------

# Windows
*.exe
*.dll
*.pdb
*.ilk
*.exp
*.lib
Thumbs.db
ehthumbs.db
Desktop.ini

# Linux
*.so
*.a
*.out
.fuse_hidden*
.directory
.Trash-*
.nfs*

# macOS
.DS_Store
.AppleDouble
.LSOverride
._*
.DocumentRevisions-V100
.fseventsd
.Spotlight-V100
.TemporaryItems
.Trashes
.VolumeIcon.icns
.com.apple.timemachine.donotpresent
.AppleDB
.AppleDesktop

# ------------------------------------------------------------------------------
# Build Artifacts and Temporary Files
# ------------------------------------------------------------------------------
*.tmp
*.temp
*.bak
*.backup
*.orig
*.rej
*.swp
*~
.#*

# Dependency directories
third_party/
external/
!external/.gitkeep
vendor/
!vendor/.gitkeep

# ------------------------------------------------------------------------------
# Custom Build Directories (Common Patterns)
# ------------------------------------------------------------------------------
/dist/
/output/
/target/
/bin/
/lib/