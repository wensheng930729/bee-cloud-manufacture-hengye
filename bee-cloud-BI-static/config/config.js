const path = require('path');
import pageRoutes from './allRoutes';
const _env = process.env.BUILD_ENV || 'dev';
console.log('当前打包环境：' + _env);

export default {
  plugins: [
    [
      'umi-plugin-react',
      {
        dva: {
          immer: true,
        },
        antd: true,
        library: 'react',
        dynamicImport: true,
        fastClick: true,
      },
    ],
  ],
  targets: {
    ie: 7,
    chrome: 49,
    firefox: 45,
    safari: 10,
    edge: 13,
    ios: 10,
  },
  hash: true,
  history: 'hash',
  extraBabelPlugins: [
    ['import', { libraryName: 'antd', libraryDirectory: 'es', style: true }],
    // ['import', { libraryName: '@babel/polyfill' ,libraryDirectory: 'es',}]
  ],
  alias: {
    components: path.resolve(__dirname, 'src/components/'),
    layouts: path.resolve(__dirname, 'src/layouts/'),
    utils: path.resolve(__dirname, 'src/utils/'),
    assets: path.resolve(__dirname, 'src/assets/'),
    services: path.resolve(__dirname, 'src/services/'),
  },
  routes: pageRoutes,
  base: '/bi-web/',
  publicPath: '/bi-web/',
  proxy: {
    '/api': {
      target: 'http://jsonplaceholder.typicode.com/',
      changeOrigin: true,
      pathRewrite: { '^/api': '' },
    },
  },
  disableCSSModules: false,
  define: {
    'process.env.BUILD_ENV': _env,
  },
};
