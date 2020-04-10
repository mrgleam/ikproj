'use strict';

require('./styles.scss');

const { Elm } = require('./Main');
var app = Elm.Main.init({ flags: 6 });

app.ports.sendTokenToStorage.subscribe(token => {
  console.log('GOT TOKEN', token)
  localStorage.setItem('ikweb-access-token', token)
})
// app.ports.toJs.subscribe((data) => {
//   console.log(data);
// });
// Use ES2015 syntax and let Babel compile it for you
// var testFn = (inp) => {
//   let a = inp + 1;
//   return a;
// };
