const path = require("path");
const HTMLWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: './src/index.js',
  output: {
    filename: 'bundle.js',
    path: __dirname + '/public',
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],

        use: [
          {loader: 'elm-hot-webpack-loader'},
          {
            loader: 'elm-webpack-loader',
            options: {
              cwd: __dirname,
            },
          },
        ],
      },
    ],
  },

  plugins: [
    new HTMLWebpackPlugin({
      title: 'app',
      // Use this template to get basic responsive meta tags
            template: 'src/index.html',
      // inject details of output file at end of body
            inject: 'body',
    }),
  ],
  resolve: {
    modules: [path.join(__dirname, 'src'), 'node_modules'],
    extensions: ['.js', '.elm', '.scss', '.png'],
  },
};
