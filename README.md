### What 
This are some experiments on cellular automata as to try autoreplicating automata as presented in:

https://www.johndcook.com/blog/2021/05/03/self-reproducing-cellular-automata/

This are experiments on how to implement automata in cljs using core.async. 
The actual implementation ideas (use canvas for performance, etc.) are based on:

https://github.com/undo76/zoomata

Live demo of that project can be found at:

https://zoomata.undo76.net


### Development mode
```
npm install
npx shadow-cljs watch app
```
start a ClojureScript REPL
```
npx shadow-cljs browser-repl
```
### Building for production

```
npx shadow-cljs release app
```
