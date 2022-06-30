package main

import (
	"context"
	"log"
	"os"
	"encoding/json"
	"time"
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


func (a *App) GetDirectoryContent(name string) string {
	dir, err := os.Open(name)
	if err != nil {
		log.Printf("Error: %s", err)
		data, _ := json.Marshal(NewError(err))
		return string(data)
	}
	defer dir.Close()

	fileList,_ := dir.Readdir(0)

    result := []FileInfo{}

    for _, entry := range fileList {
        f := FileInfo{
            Name:    entry.Name(),
            Size:    entry.Size(),
            Mode:    entry.Mode(),
            ModTime: entry.ModTime(),
            IsDir:   entry.IsDir(),
        }
        result = append(result, f)
    }

	data, err := json.Marshal(result)

	log.Printf("result %s", result)


	if err != nil {
		errorData, _ := json.Marshal(NewError(err))
		log.Printf("Error: %s", err)
		return string(errorData)
	}
	return string(data)

}