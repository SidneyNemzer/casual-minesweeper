const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const CopyWebpackPlugin = require('copy-webpack-plugin')
const CleanWebpackPlugin = require('clean-webpack-plugin')

module.exports = (env, args) => {
  return {
    mode: args.mode || 'development',

    devtool: args.mode !== 'production' && 'cheap-eval-source-map',

    context: path.resolve(__dirname),
    output: {
      path: path.resolve(__dirname, './build')
    },

    module: {
      strictExportPresence: true,
      noParse: /\.elm$/,
      rules: [
        {
          oneOf: [
            // "url" loader works just like "file" loader but it also embeds
            // assets smaller than specified size as data URLs to avoid requests.
            {
              test: [/\.bmp$/, /\.gif$/, /\.jpe?g$/, /\.png$/],
              loader: 'url-loader',
              options: {
                limit: 10000,
                name: 'media/[name].[ext]'
              }
            },
            // Compile Elm files
            {
              test: /\.elm$/,
              exclude: [/elm-stuff/, /node_modules/],
              loader: 'elm-webpack-loader',
              options: {
                debug: args.mode !== 'production',
                optimize: args.mode === 'production'
              }
            },
            // "css" loader resolves paths in CSS and adds assets as dependencies.
            // "style" loader turns CSS into JS modules that inject <style> tags.
            // In production, we use a plugin to extract that CSS to a file, but
            // in development "style" loader enables hot editing of CSS.
            {
              test: /\.css/,
              use: [
                'style-loader',
                'css-loader'
              ]
            },
            // "file" loader makes sure those assets get served by WebpackDevServer.
            // When you `import` an asset, you get its (virtual) filename.
            // In production, they would get copied to the `build` folder.
            // This loader doesn't use a "test" so it will catch all modules
            // that fall through the other loaders.
            {
              // Exclude `js` files to keep "css" loader working as it injects
              // its runtime that would otherwise processed through "file" loader.
              // Also exclude `html`, `json`, and 'ejs' extensions so they get
              // processed by Webpack's internal loaders.
              exclude: [/\.js$/, /\.html$/, /\.json$/, /\.ejs$/],
              loader: require.resolve('file-loader'),
              options: {
                name: 'media/[name].[ext]'
              }
            }
          ]
        }
        // ** STOP ** Are you adding a new loader?
        // Make sure to add the new loader(s) before the "file" loader.
      ]
    },

    plugins: [
      new CleanWebpackPlugin(),
      new HtmlWebpackPlugin({
        inject: true,
        filename: 'index.html',
        template: path.resolve(__dirname, './src/template.html')
      }),
      new CopyWebpackPlugin([ { from: 'public', to: './' } ])
    ]
  }
}
