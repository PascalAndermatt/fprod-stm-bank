var path = require("path"),
    webpack = require('webpack');

module.exports = {
  entry: {
    app: [
      './src/index.js'
    ]
  },

  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: 'app.js',
  },

  module: {
    loaders: [
      {
        test:    /\.html$/,
        include: /src/,
        loader:  'file?name=[name].[ext]',
      },
      {
        test:    /\.elm$/,
        include: /src/,
        loader:  'elm-hot!elm-webpack',
      }
    ],
    noParse: /\.elm$/,
    rules: [
      {
        test: /\.css$/i,
        use: ['style-loader', 'css-loader']
      }
      // {
      //   test: /\.(png|svg|jpg|jpeg|gif)$/i,
      //   type: 'asset/resource'
      // }
    ]
  },

  devServer: {
    inline: true,
    stats: 'errors-only',
    historyApiFallback: true,
    headers: {
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, PATCH, OPTIONS",
      "Access-Control-Allow-Headers": "X-Requested-With, content-type, Authorization"
   }
  },
};

