# For downloading IndexedDb of KC3Kai

## storer

For retrieving data from exporting logic.

```
cd storer
stack build --fast --pedantic && stack exec -- storer
```

## Exporting from KC3Kai

Save lodash as `src/assets/js/lodash-4.17.5`.

Edit `src/pages/strategy/strategy.html` to include it:

```html
<script type="text/javascript" src="../../assets/js/lodash-4.17.5.js"></script>
```

Now go to strategy room, open console. Paste content from `test.js` to it, wait until `all done` message is printed.
