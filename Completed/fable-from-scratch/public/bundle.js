!function(e){var t={};function r(n){if(t[n])return t[n].exports;var i=t[n]={i:n,l:!1,exports:{}};return e[n].call(i.exports,i,i.exports,r),i.l=!0,i.exports}r.m=e,r.c=t,r.d=function(e,t,n){r.o(e,t)||Object.defineProperty(e,t,{configurable:!1,enumerable:!0,get:n})},r.r=function(e){Object.defineProperty(e,"__esModule",{value:!0})},r.n=function(e){var t=e&&e.__esModule?function(){return e.default}:function(){return e};return r.d(t,"a",t),t},r.o=function(e,t){return Object.prototype.hasOwnProperty.call(e,t)},r.p="",r(r.s=0)}([function(e,t,r){"use strict";r.r(t);function n(e,t){for(var r=e.toString(10);r.length<t;)r="0"+r;return r}function i(e){var t=e<0,r=(e=Math.abs(e))%36e5/6e4;return(t?"-":"+")+n(~~(e/36e5),2)+":"+n(r,2)}function o(e,t){var r=e.toISOString();return"first"===t?r.substring(0,r.indexOf("T")):r.substring(r.indexOf("T")+1,r.length-1)}function a(e,t,r){return t.replace(/(\w)\1*/g,function(t){var n=t;switch(t.substring(0,1)){case"y":var i=r?e.getUTCFullYear():e.getFullYear();n=t.length<4?i%100:i;break;case"M":n=(r?e.getUTCMonth():e.getMonth())+1;break;case"d":n=r?e.getUTCDate():e.getDate();break;case"H":n=r?e.getUTCHours():e.getHours();break;case"h":var o=r?e.getUTCHours():e.getHours();n=o>12?o%12:o;break;case"m":n=r?e.getUTCMinutes():e.getMinutes();break;case"s":n=r?e.getUTCSeconds():e.getSeconds()}return n!==t&&n<10&&t.length>1&&(n="0"+n),n})}function u(e,t){var r,n,u,f=new Date(e.getTime()+e.offset);if(!t)return f.toISOString().replace(/\.\d+/,"").replace(/[A-Z]|\.\d+/g," ")+i(e.offset);if(1!==t.length)return a(f,t,!0);switch(t){case"D":case"d":return o(f,"first");case"T":case"t":return o(f,"second");case"O":case"o":return r=f,n=e.offset,(u=r.toISOString()).substring(0,u.length-1)+i(n);default:throw new Error("Unrecognized Date print format")}}function f(e,t){var r=1===e.kind;if(!t)return r?e.toUTCString():e.toLocaleString();if(1!==t.length)return a(e,t,r);switch(t){case"D":case"d":return r?o(e,"first"):e.toLocaleDateString();case"T":case"t":return r?o(e,"second"):e.toLocaleTimeString();case"O":case"o":return function(e,t){if(t)return e.toISOString();var r=null==e.kind||2===e.kind;return n(e.getFullYear(),4)+"-"+n(e.getMonth()+1,2)+"-"+n(e.getDate(),2)+"T"+n(e.getHours(),2)+":"+n(e.getMinutes(),2)+":"+n(e.getSeconds(),2)+"."+n(e.getMilliseconds(),3)+(r?i(-6e4*e.getTimezoneOffset()):"")}(e,r);default:throw new Error("Unrecognized Date print format")}}function c(e,t){return null!=e.offset?u(e,t):f(e,t)}new Map;var s={reflection:Symbol("reflection")},l="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(e){return typeof e}:function(e){return e&&"function"==typeof Symbol&&e.constructor===Symbol&&e!==Symbol.prototype?"symbol":typeof e},g=function(){function e(e,t){for(var r=0;r<t.length;r++){var n=t[r];n.enumerable=n.enumerable||!1,n.configurable=!0,"value"in n&&(n.writable=!0),Object.defineProperty(e,n.key,n)}}return function(t,r,n){return r&&e(t.prototype,r),n&&e(t,n),t}}();function y(e,t){if(!(e instanceof t))throw new TypeError("Cannot call a class as a function")}var d=function(){function e(t,r,n){y(this,e),this.kind=t,this.definition=r,this.generics=n}return g(e,[{key:"Equals",value:function(e){return this.kind===e.kind&&this.definition===e.definition&&("object"===l(this.generics)?function(e,t){if(e===t)return!0;var r=b(e),n=!0,i=!1,o=void 0;try{for(var a,u=r[Symbol.iterator]();!(n=(a=u.next()).done);n=!0){var f=a.value;if(!v(e[f],t[f]))return!1}}catch(e){i=!0,o=e}finally{try{!n&&u.return&&u.return()}finally{if(i)throw o}}return!0}(this.generics,e.generics):this.generics===e.generics)}}]),e}();new d("Any"),new d("Unit");function b(e){if(null==e)return[];var t="function"==typeof e[s.reflection]?e[s.reflection]().properties||[]:e;return Object.getOwnPropertyNames(t)}function p(e){var t=arguments.length>1&&void 0!==arguments[1]&&arguments[1];if(null==e||"number"==typeof e)return String(e);if("string"==typeof e)return t?JSON.stringify(e):e;if(e instanceof Date)return c(e);if("function"==typeof e.ToString)return e.ToString();if(function(e,t){if("System.Collections.Generic.IEnumerable"===t)return"function"==typeof e[Symbol.iterator];if("function"==typeof e[s.reflection]){var r=e[s.reflection]().interfaces;return Array.isArray(r)&&r.indexOf(t)>-1}return!1}(e,"FSharpUnion")){var r=e[s.reflection]().cases[e.tag];switch(r.length){case 1:return r[0];case 2:return r[0]+" ("+p(e.data,!0)+")";default:return r[0]+" ("+e.data.map(function(e){return p(e,!0)}).join(",")+")"}}try{return JSON.stringify(e,function(e,t){return!t||!t[Symbol.iterator]||Array.isArray(t)||(null===(r=t)||"object"!==(void 0===r?"undefined":l(r))||r instanceof Number||r instanceof String||r instanceof Boolean)?t&&"function"==typeof t.ToString?p(t):t:Array.from(t);var r})}catch(t){return"{"+Object.getOwnPropertyNames(e).map(function(t){return t+": "+String(e[t])}).join(", ")+"}"}}var h=function(){function e(){y(this,e)}return g(e,null,[{key:"id",value:function(t){return e.idMap.has(t)||e.idMap.set(t,++e.count),e.idMap.get(t)}}]),e}();function v(e,t){if(e===t)return!0;if(null==e)return null==t;if(null==t)return!1;if("object"!==(void 0===e?"undefined":l(e))||"object"!==(void 0===t?"undefined":l(t)))return e===t;if("function"==typeof e.Equals)return e.Equals(t);if("function"==typeof t.Equals)return t.Equals(e);if(Object.getPrototypeOf(e)!==Object.getPrototypeOf(t))return!1;if(Array.isArray(e)){if(e.length!==t.length)return!1;for(var r=0;r<e.length;r++)if(!v(e[r],t[r]))return!1;return!0}if(ArrayBuffer.isView(e)){if(e.byteLength!==t.byteLength)return!1;for(var n=new DataView(e.buffer),i=new DataView(t.buffer),o=0;o<e.byteLength;o++)if(n.getUint8(o)!==i.getUint8(o))return!1;return!0}return e instanceof Date&&e.getTime()===t.getTime()}h.idMap=new WeakMap,h.count=0;var S=/(^|[^%])%([0+ ]*)(-?\d+)?(?:\.(\d+))?(\w)/;function m(e){return e<0?"ff"+(16777215-(Math.abs(e)-1)).toString(16):e.toString(16)}function w(e,t){return e.replace(S,function(e,r,n,i,o,a){switch(a){case"f":case"F":t=t.toFixed(o||6);break;case"g":case"G":t=t.toPrecision(o);break;case"e":case"E":t=t.toExponential(o);break;case"O":t=p(t);break;case"A":t=p(t,!0);break;case"x":t=m(Number(t));break;case"X":t=m(Number(t)).toUpperCase()}var u=n.indexOf("+")>=0&&parseInt(t,10)>=0;if(i=parseInt(i,10),!isNaN(i)){var f=i>=0&&n.indexOf("0")>=0?"0":" ";t=T(t,Math.abs(i)-(u?1:0),f,i<0)}return(r+(u?"+"+t:t)).replace(/%/g,"%%")})}var O,k;function T(e,t,r,n){r=r||" ",t-=(e=String(e)).length;for(var i=0;i<t;i++)e=n?e+r:r+e;return e}({input:O="Hello Fable 🐉",cont:(k=O,function(e){return S.test(k)?function e(t,r){var n=function(){for(var n=arguments.length,i=Array(n),o=0;o<n;o++)i[o]=arguments[o];var a=t,u=!0,f=!1,c=void 0;try{for(var s,l=i[Symbol.iterator]();!(u=(s=l.next()).done);u=!0){var g=s.value;a=w(a,g)}}catch(e){f=!0,c=e}finally{try{!u&&l.return&&l.return()}finally{if(f)throw c}}return S.test(a)?e(a,r):r(a.replace(/%%/g,"%"))};return n.curried=!0,n}(k,e):e(k)})}).cont(console.log)}]);
//# sourceMappingURL=bundle.js.map