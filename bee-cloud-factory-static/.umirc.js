const path = require('path')
const _env = process.env.BUILD_ENV || 'dev'
export default {
  history: 'hash',
  plugins: [
    [
      'umi-plugin-react',
      {
        dva: {
          immer: true
        },
        antd: true,
        routes: {
          exclude: [
            /model\.(j|t)sx?$/,
            /service\.(j|t)sx?$/,
            /models\//,
            /components\//,
            /services\//
          ]
        }
      }
    ]
  ],
  alias: {
    components: path.resolve(__dirname, '../src/components/'),
    // layouts: path.resolve(__dirname, 'src/layouts'),
    utils: path.resolve(__dirname, '../src/utils/'),
    assets: path.resolve(__dirname, '../src/assets/')
  },
  define: {
    'process.env.BUILD_ENV': _env
  },
  base: '/factory-web/',
  publicPath: '/factory-web/'
}