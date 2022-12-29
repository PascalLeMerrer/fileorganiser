package main
import (
    "testing"
    "fmt"
)

func TestGetCurrentDirectory(t *testing.T) {
    app:= NewApp()

    currentDir, err := app.GetCurrentDirectory()
    if err != nil {
        t.Log(err)
        t.Fail()
    }
    t.Log(fmt.Sprintf("currentDir %s", currentDir))
}