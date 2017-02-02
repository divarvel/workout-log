'use strict';

module.exports = {
  debug: true,

  devtool: 'eval-source-map',

  devServer: {
    contentBase: '.',
    port: 4009,
    stats: 'errors-only'
  },

  entry: './src/Main.purs',

  output: {
    path: __dirname,
    pathinfo: true,
    filename: 'bundle.js'
  },

  module: {
    loaders: [
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        query: {
          src: [ 'bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs' ],
          bundle: true,
          pscIde: false,
          psc: 'psa'
        }
      },
    ]
  },

  resolve: {
    modulesDirectories: [ 'node_modules', 'bower_components' ],
    extensions: [ '', '.purs', '.js']
  }
};
