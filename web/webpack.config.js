const path = require('path');
const webpack = require('webpack')
const WebpackNotifierPlugin = require('webpack-notifier')
const ESLintPlugin = require('eslint-webpack-plugin');
const {BundleAnalyzerPlugin} = require('webpack-bundle-analyzer');

const prod = process.env.NODE_ENV === 'production'
const dev = !prod

module.exports = {
  mode: prod ? 'production' : 'development',
  entry: './src/index.tsx',
  module: {
    rules: [     
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
    extensions: ['.tsx', '.ts', '.js'],
  },
  output: {
    path: path.join(__dirname, 'build'),
    filename: 'app.js'
  },
  devtool: dev ? 'eval-source-map' : 'source-map',
  devServer: dev ? {
    // contentBase: 'public',
    historyApiFallback: true,
    proxy: {
      '/api': 'http://localhost:4321'
    }
  } : undefined,
  plugins: [
    new ESLintPlugin({
      extensions: ["js", "jsx", "ts", "tsx"],
    }),
    dev && new BundleAnalyzerPlugin(),
    dev && new WebpackNotifierPlugin(),
    prod && new webpack.DefinePlugin({
      'process.env': {
        'NODE_ENV': JSON.stringify('production')
      }
    })
  ].filter(Boolean)
};
