const path = require('path');
const webpack = require('webpack')
const WebpackNotifierPlugin = require('webpack-notifier')

const prod = process.env.NODE_ENV === 'production'
const dev = !prod

module.exports = {
  mode: prod ? 'production' : 'development',
  entry: './src/index.tsx',
  module: {
    rules: [
      {
        test: /\.(ts|tsx)$/,
        enforce: 'pre',
        use: [
          {
            options: {
              eslintPath: require.resolve('eslint'),

            },
            loader: require.resolve('eslint-loader'),
          },
        ],
        exclude: /node_modules|..\/common/ //exclude common for now because eslint blows up on it. TODO fix
      },
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
    ],
  },
  resolve: {
    alias: {
      '@pim/common': path.join(__dirname, '../common')
    },
    extensions: [ '.tsx', '.ts', '.js' ],
  },
  output: {
    path: path.join(__dirname, 'build'),
    filename: 'app.js'
  },
  devtool: dev ? 'eval-source-map' : 'source-map',
  devServer: dev ? {
    contentBase: 'public',
    historyApiFallback: true
  } : undefined,
  plugins: [
    // new webpack.OldWatchingPlugin(),
    new WebpackNotifierPlugin(),
    prod && new webpack.DefinePlugin({
      'process.env': {
        'NODE_ENV': JSON.stringify('production')
      }
    })
  ].filter(Boolean)
};
