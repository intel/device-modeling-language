<!--
  © 2021-2022 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Introduction

The DML tool provides a programming language for modeling devices
in Simics. DML has been designed to make it easy to represent the
kind of things that are needed by a device model, and uses special syntactic
constructs to describe common elements such as memory mapped hardware registers
and connections to other Simics configuration objects.

The DML compiler is called `dmlc`. It translates a device model
description written in DML into C source code that can be compiled and
loaded as a Simics module.

This document describes the DML language, the standard libraries, and
the `dmlc` compiler, as of version 1.2 of DML. See
also *Simics Model Builder User's Guide* for an introduction
to DML.

## Requirements

The DML compiler and standard libraries, the documentation, and the
example devices, are provided by the *Model Builder* product,
which is bundled with the *Simics Base* package.

To run `dmlc`, and to build a device model generated from
DML, it is also necessary to have a license and a key for Simics Model Builder
and a working Simics build environment; see
*Simics Model Builder User's Guide* for details.

