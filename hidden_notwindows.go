//go:build !windows
// +build !windows
// Source https://github.com/ayoisaiah/f2/ - MIT License
package main

import (
    "strings"
)

const dotCharacter = 46
const pathSeperator = "/"

// isHidden checks if a file is hidden on Unix operating systems
// the error is returned to match the signature of the Windows
// version of the function.
func isHidden(filename, baseDir string) (bool, error) {
    names :=  strings.Split(filename, pathSeperator)
    for _, name := range(names) {
        if strings.HasPrefix(name, ".") {
            return true, nil
        }
    }
    return false, nil
}