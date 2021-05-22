'use strict';

// Require index.html so it gets copied to dist
require('./index.html');

var app = require('./Main.elm').Elm.Main;
app.init({ node: document.querySelector('main') });
