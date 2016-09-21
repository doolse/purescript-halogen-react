'use strict';
var RD = require('react-dom');

exports.renderElementDOM = function (re) {
  return function (e) {
    return function() {
      RD.render(re, e);
    }
  }
}
