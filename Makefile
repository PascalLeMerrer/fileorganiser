dev:
	../bin/wails dev

macos:
	../bin/wails build -platform darwin -webview2 download -clean -o fileorganizer.app

windows:
	../bin/wails build -platform windows/amd64 -webview2 download -clean -o fileorganizer.exe

install:
	cd elm; npm install --save-dev elm-review

test:
	cd elm; elm-test

review:
	cd elm; node_modules/elm-review/bin/elm-review --fix
