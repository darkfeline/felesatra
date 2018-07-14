// Package enja implements reading and writing Enja documents.
//
// Enja is a file format for storing documents in files.  This module implements
// reading and writing the Enja file format.
//
// An Enja file is a text file that contains:
//
//   * the document header metadata formatted in YAML
//   * a line containing three (3) hyphen-minus characters (U+002D) terminated by a
//     newline character (U+000A).
//   * the document body
package enja
