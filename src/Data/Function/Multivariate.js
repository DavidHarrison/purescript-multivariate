/* global exports */
"use strict";

// module Data.Function.Multivariate

exports.getConst = function(f) {
  return f();
}

// NOTE: we do not slice on `arguments` as it prevents optimizations.
//       See https://github.com/petkaantonov/bluebird/wiki/Optimization-killers#3-managing-arguments.
exports.fnCurry = function(fn) {
  return function(x) {
    return function() {
      len = arguments.length;
      args = new Array(len + 1);
      args[0] = x;
      for (var i = 0; i < len; i++) {
        args[i + 1] = arguments[i];
      }
      return fn.apply(null, args);
    }
  }
}

exports.fnUncurry = function(f) {
  return function() {
    len = arguments.length;
    args = new Array(len - 1);
    for (var i = 1; i < len; i++) {
      args[i - 1] = arguments[i];
    }
    return f(arguments[0]).apply(null, args);
  }
}

exports.fnMap = function(f) {
  return function(fn) {
    return function() {
      return f(fn.apply(null, arguments));
    }
  }
}

exports.fnApply = function(f) {
  return function(g) {
    return function() {
      return f.apply(null, arguments)(g.apply(null, arguments));
    }
  }
}

exports.fnPure = function(x) {
  return function() {
    return x;
  }
}

exports.fnBind = function(fn) {
  return function(f) {
    return function() {
      return f(fn.apply(null, argumnets)).apply(null, arguments);
    }
  }
}
