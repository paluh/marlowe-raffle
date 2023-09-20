import path from 'path';
import { fileURLToPath } from 'url';
import { BundleAnalyzerPlugin } from 'webpack-bundle-analyzer';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

import webpack from 'webpack';

export default function(_env, argv) {
  return {
    mode: 'production',
    optimization: {
      usedExports: true,
    },
    externals: {
      react: 'react',
      "react-dom": 'react-dom',
      // "@dcspark/cardano-multiplatform-lib-browser": "@dcspark/cardano-multiplatform-lib-browser",
    },
    experiments: {
      asyncWebAssembly: true,
      outputModule: true,
    },
    entry: {
       lib: './src/lib.js',
    },
    devtool: 'inline-source-map',
    devServer: {
      static: './public',
      hot: true,
      port: 8080
    },
    plugins: [
      // new webpack.NormalModuleReplacementPlugin(
      //   '@dcspark/cardano-multiplatform-lib-browser'
      // ),
      new BundleAnalyzerPlugin(),
    ],
    output: {
      filename: 'marlowe-raffle.js',
      path: path.resolve(__dirname, 'dist'),
      // libraryTarget: 'module',
      libraryTarget: "module",
      // globalObject: 'this',
    },
    resolve: {
      modules: ['node_modules'],
    },
    module: {
      rules: [
        // {
        //   test: /\.wasm$/,
        //   type: "asset/inline",
        // },
        {
          test: /\.js$/,
          exclude: /node_modules/,
          use: {
            loader: 'babel-loader'
          }
        },
        {
          test: /\.md$/,
          use: [
            {
              loader: "html-loader",
            },
          ],
        },
        {
          test: /\.(scss|css)$/,
          use: [
          {
            // inject CSS to page
            loader: 'style-loader'
          }, {
            // translates CSS into CommonJS modules
            loader: 'css-loader'
          }, {
            // Run postcss actions
            loader: 'postcss-loader',
            options: {
              // `postcssOptions` is needed for postcss 8.x;
              // if you use postcss 7.x skip the key
              postcssOptions: {
                // postcss plugins, can be exported to postcss.config.js
                plugins: function () {
                  return [
                    require('autoprefixer')
                  ];
                }
              }
            }
          }, {
            // compiles Sass to CSS
            loader: 'sass-loader'
          }]
        }
      ]
    }
  };
};
