'use strict';

var React = require('react');

function createElement(class_) {
  return function(props){
    return function(children){
      return React.createElement.apply(React, [class_, props].concat(children));
    };
  };
}

exports.createElementOneChild = createElement

exports.getRefImpl = function(refs, n) {
  return refs[n];
}
