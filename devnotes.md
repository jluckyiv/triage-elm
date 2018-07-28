# Development notes
## Branch yak-shaving
### json-server
After getting the basic app up and running, I followed directions from [Elm Tutorial](https://www.elm-tutorial.org/en/) on [setting up a fake backend](https://www.elm-tutorial.org/en/04-starting/02-backend.html) with [json-server](https://github.com/typicode/json-server). The tutorial pinned v0.9.5, but I used the latest, which is 0.14.0. For some reason, I can't get the root screen of json-server to show anymore, whether or not I use the old version.

### Bootstrap
I know Bootstrap is tired, but I know it, so I'm using [elm-bootstrap](http://elm-bootstrap.info/). The library uses only the css, but `bootstrap-css` has individual css files that I don't want to pick through. I pulled down the regular Bootstrap package.

One concern I have is that the [dropdowns](http://elm-bootstrap.info/dropdown) require some coding and state management.

### Font Awesome
I'll probably want some icons, so I added Font Awesome.