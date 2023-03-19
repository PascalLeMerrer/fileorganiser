dev:
	wails dev

macos:
	wails build -platform darwin -webview2 download -clean -o fileorganizer.app

windows:
	wails build -platform windows/amd64 -webview2 download -clean -o fileorganizer.exe

install:
	cd elm; npm install --save-dev elm-review

test:
	cd elm; elm-test-rs

test-watch:
	cd elm; elm-test-rs --watch

review:
	cd elm; elm-review --fix
