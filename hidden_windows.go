//go:build windows
// +build windows
// Source https://github.com/ayoisaiah/f2/ - MIT License

package main

import (
    "path/filepath"
    "syscall"
)

const pathSeperator = `\`
const dotCharacter = 46

// isHidden checks if a file is hidden on Windows.
func isHidden(filename, baseDir string) (bool, error) {
    // dotfiles also count as hidden
    if filename[0] == dotCharacter {
        return true, nil
    }

    absPath, err := filepath.Abs(filepath.Join(baseDir, filename))
    if err != nil {
        return false, err
    }

    // Appending `\\?\` to the absolute path helps with
    // preventing 'Path Not Specified Error' when accessing
    // long paths and filenames
    // https://docs.microsoft.com/en-us/windows/win32/fileio/maximum-file-path-limitation?tabs=cmd
    pointer, err := syscall.UTF16PtrFromString(`\\?\` + absPath)
    if err != nil {
        return false, err
    }

    attributes, err := syscall.GetFileAttributes(pointer)
    if err != nil {
        return false, err
    }

    return attributes&syscall.FILE_ATTRIBUTE_HIDDEN != 0, nil
}