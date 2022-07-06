package main

import (
	"context"
	"os"
	"path/filepath"
	"time"
	"github.com/wailsapp/wails/v2/pkg/runtime"
)

// App struct
type App struct {
	ctx context.Context
}

// NewApp creates a new App application struct
func NewApp() *App {
	return &App{}
}

// startup is called at application startup
func (a *App) startup(ctx context.Context) {
	// Perform your setup here
	a.ctx = ctx
}

// domReady is called after front-end resources have been loaded
func (a App) domReady(ctx context.Context) {
	// Add your action here
}

// beforeClose is called when the application is about to quit,
// either by clicking the window close button or calling runtime.Quit.
// Returning true will cause the application to continue, false will continue shutdown as normal.
func (a *App) beforeClose(ctx context.Context) (prevent bool) {
	return false
}

// shutdown is called at application termination
func (a *App) shutdown(ctx context.Context) {
	// Perform your teardown here
}


type SerialisableError struct {
	errorMsg string
}

func NewError(message error) *SerialisableError {
	return &SerialisableError{message.Error()}
}


type FileInfo struct {
    Name    string
    Size    int64
    Mode    os.FileMode
    ModTime time.Time
    IsDir   bool
}



func (a *App) GetCurrentDirectory() (string, error) {
	return os.Getwd()
}


func (a *App) GetDirectoryContent(dirName string) ([]FileInfo, error) {
	dir, err := os.Open(dirName)

	if err != nil {
		return nil, err
	}

	defer dir.Close()

	fileList, err := dir.Readdir(0)

	if err != nil {
		return nil, err
	}

    result := []FileInfo{}

    for _, entry := range fileList {
    	isHidden, _ := isHidden(entry.Name(), dirName)
    	if isHidden {
    		continue
    	}

        f := FileInfo{
            Name:    entry.Name(),
            Size:    entry.Size(),
            Mode:    entry.Mode(),
            ModTime: entry.ModTime(),
            IsDir:   entry.IsDir(),
        }
        result = append(result, f)
    }

	return result, nil
}

func (a *App) GetDirectoryFiles(dirName string) ([]FileInfo, error) {
	dir, err := os.Open(dirName)

	if err != nil {
		return nil, err
	}

	defer dir.Close()

	fileList, err := dir.Readdir(0)

	if err != nil {
		return nil, err
	}

	parentDirFileInfo, err := getParentDirectory(dirName)
	if err != nil {
		return nil, err
	}
	result := []FileInfo{}
	result = append(result, parentDirFileInfo)

    for _, entry := range fileList {
    	isHidden, _ := isHidden(entry.Name(), dirName)
    	if isHidden || entry.IsDir() {
    		continue
    	}

        f := FileInfo{
            Name:    entry.Name(),
            Size:    entry.Size(),
            Mode:    entry.Mode(),
            ModTime: entry.ModTime(),
            IsDir:   entry.IsDir(),
        }
        result = append(result, f)
    }

	return result, nil
}

func (a *App) GetSubdirectoriesRecursively(dirName string) ([]FileInfo, error) {
	parentDirFileInfo, err := getParentDirectory(dirName)
	if err != nil {
		return nil, err
	}
	result := []FileInfo{}
	result = append(result, parentDirFileInfo)

	err = filepath.Walk(dirName,
	    func(path string, info os.FileInfo, err error) error {
		    if err != nil {
		        return err
		    }
		    relativePath, err := filepath.Rel(dirName, path)
			if err != nil {
		        return err
		    }
		    isHidden, _ := isHidden(relativePath, dirName)
	    	if info.IsDir() && ! isHidden {
		        f := FileInfo{
		            Name:    relativePath,
		            Size:    info.Size(),
		            Mode:    info.Mode(),
		            ModTime: info.ModTime(),
		            IsDir:   info.IsDir(),
		        }
		        result = append(result, f)
	    	}
		    return nil
		})
	return result, err
}

func getParentDirectory(absolutePath string) (FileInfo, error) {

	parentDir := filepath.Dir(absolutePath)
	info, err := os.Stat(parentDir)
	if err != nil {
		return FileInfo{}, err
	}

	f := FileInfo{
        Name:    "..",
        Size:    info.Size(),
        Mode:    info.Mode(),
        ModTime: info.ModTime(),
        IsDir:   true,
    }
    return f, nil
}

func (a *App) SelectDirectory(defaultDirectory string, title string) (string, error) {
	options := runtime.OpenDialogOptions{
		DefaultDirectory: defaultDirectory,
		Title: title,
	}
	return runtime.OpenDirectoryDialog(a.ctx, options)
}

func (a *App) Move(sourceFiles []string, destinationDirectory string) ([]FileInfo, error) {
	result := []FileInfo{}
	for _, sourcePath := range sourceFiles {
		filename := filepath.Base(sourcePath)
		newLocation := filepath.Join(destinationDirectory, filename)
		err := os.Rename(sourcePath, newLocation)
		if err != nil {
			return nil, err
		}
		info, err := os.Stat(newLocation)
		if err != nil {
			return nil, err
		}
		f := FileInfo{
            Name:    info.Name(),
            Size:    info.Size(),
            Mode:    info.Mode(),
            ModTime: info.ModTime(),
            IsDir:   info.IsDir(),
        }
        result = append(result, f)
	}
	return result, nil
}

func (a *App) Rename(oldName string, newName string) (FileInfo, error) {
	err := os.Rename(oldName, newName)
	if err != nil {
		return FileInfo{}, err
	}
	info, err := os.Stat(newName)
	if err != nil {
		return FileInfo{}, err
	}
	result := FileInfo{
        Name:    info.Name(),
        Size:    info.Size(),
        Mode:    info.Mode(),
        ModTime: info.ModTime(),
        IsDir:   info.IsDir(),
    }
	return result, nil
}
