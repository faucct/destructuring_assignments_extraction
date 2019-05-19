# JavaScript Destructuring assignments extraction

[![Build Status](https://travis-ci.org/faucct/destructuring_assignments_extraction.svg?branch=master)](https://travis-ci.org/faucct/destructuring_assignments_extraction)

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment

Before:

```javascript
function ping(arr) {
  var a = arr[0];
  var b = arr[1];
}
```

After:
```javascript
function ping(arr) {
  var [a, b] = arr;
}
```

## Running

```
sbt run src/test/resources/ping.js
```
