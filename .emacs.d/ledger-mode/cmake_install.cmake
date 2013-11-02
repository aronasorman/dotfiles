# Install script for directory: /home/aronasorman/src/ledger/lisp

# Set the install prefix
IF(NOT DEFINED CMAKE_INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX "/usr/local")
ENDIF(NOT DEFINED CMAKE_INSTALL_PREFIX)
STRING(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
IF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  IF(BUILD_TYPE)
    STRING(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  ELSE(BUILD_TYPE)
    SET(CMAKE_INSTALL_CONFIG_NAME "")
  ENDIF(BUILD_TYPE)
  MESSAGE(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
ENDIF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)

# Set the component getting installed.
IF(NOT CMAKE_INSTALL_COMPONENT)
  IF(COMPONENT)
    MESSAGE(STATUS "Install component: \"${COMPONENT}\"")
    SET(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  ELSE(COMPONENT)
    SET(CMAKE_INSTALL_COMPONENT)
  ENDIF(COMPONENT)
ENDIF(NOT CMAKE_INSTALL_COMPONENT)

# Install shared libraries without execute permission?
IF(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  SET(CMAKE_INSTALL_SO_NO_EXE "1")
ENDIF(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp" TYPE FILE FILES
    "/home/aronasorman/src/ledger/lisp/ledger-commodities.el"
    "/home/aronasorman/src/ledger/lisp/ledger-complete.el"
    "/home/aronasorman/src/ledger/lisp/ledger-exec.el"
    "/home/aronasorman/src/ledger/lisp/ledger-fonts.el"
    "/home/aronasorman/src/ledger/lisp/ledger-init.el"
    "/home/aronasorman/src/ledger/lisp/ledger-mode.el"
    "/home/aronasorman/src/ledger/lisp/ledger-occur.el"
    "/home/aronasorman/src/ledger/lisp/ledger-post.el"
    "/home/aronasorman/src/ledger/lisp/ledger-reconcile.el"
    "/home/aronasorman/src/ledger/lisp/ledger-regex.el"
    "/home/aronasorman/src/ledger/lisp/ledger-report.el"
    "/home/aronasorman/src/ledger/lisp/ledger-schedule.el"
    "/home/aronasorman/src/ledger/lisp/ledger-sort.el"
    "/home/aronasorman/src/ledger/lisp/ledger-state.el"
    "/home/aronasorman/src/ledger/lisp/ledger-test.el"
    "/home/aronasorman/src/ledger/lisp/ledger-texi.el"
    "/home/aronasorman/src/ledger/lisp/ledger-xact.el"
    "/home/aronasorman/src/ledger/lisp/ledger-commodities.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-complete.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-exec.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-fonts.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-init.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-mode.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-occur.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-post.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-reconcile.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-regex.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-report.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-schedule.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-sort.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-state.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-test.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-texi.elc"
    "/home/aronasorman/src/ledger/lisp/ledger-xact.elc"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(CMAKE_INSTALL_COMPONENT)
  SET(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
ELSE(CMAKE_INSTALL_COMPONENT)
  SET(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
ENDIF(CMAKE_INSTALL_COMPONENT)

FILE(WRITE "/home/aronasorman/src/ledger/lisp/${CMAKE_INSTALL_MANIFEST}" "")
FOREACH(file ${CMAKE_INSTALL_MANIFEST_FILES})
  FILE(APPEND "/home/aronasorman/src/ledger/lisp/${CMAKE_INSTALL_MANIFEST}" "${file}\n")
ENDFOREACH(file)
