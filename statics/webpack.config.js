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
        exclude: /node_modules/,
      },
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
    ],
  },
  resolve: {
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
  // module: {
    // rules: [
    //   {
    //     test: /\.tsx?$/,
    //     use: 'ts-loader',
    //     exclude: /node_modules/,
    //   },
    // ]
    // preLoaders: [
    //   {
    //     test: /\.js$/,
    //     exclude: /node_modules/,
    //     loader: 'eslint'
    //   }
    // ],
    // loaders: [
    //   {
    //     test: /\.js$/,
    //     exclude: /node_modules/,
    //     loader: 'babel'
    //   },
    //   {
    //     test: /\.css$/,
    //     exclude: /node_modules/,
    //     loaders: ['style', 'css?modules&sourceMap&importLoaders=1&localIdentName=[name]__[local]___[hash:base64:5]', 'postcss']
    //   },
    //   {
    //     test: /\.css$/,
    //     exclude: /src/,
    //     loaders: ['style', 'css']
    //   },
    //   {
    //     test: /\.(jpg|png|ttf|eot|woff|woff2|svg)$/,
    //     exclude: /node_modules/,
    //     loader: 'url?limit=100000'
    //   }
    // ]
  // },
  // plugins: [
  //   new webpack.OldWatchingPlugin(),
  //   new WebpackNotifierPlugin()
  // ]

};
