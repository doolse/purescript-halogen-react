'use strict';


function proxyCall (f) {
  return function (r) {
    if (typeof r == 'function') {
      return function() {
        return f(r.apply(null, arguments));
      }
    } else return f(r);
  }
}

exports.runRenderable = proxyCall
exports.runUncurriedEvent = proxyCall
