const _env = process.env.BUILD_ENV;

// 根域名
const domain =
  _env === 'pro'
    ? 'https://sibeecloudmanufacture.beesrv.com'
    : _env === 'qa'
    ? 'http://192.168.3.181:9140'
    : _env === 'qa1'
    ? 'http://182.139.182.247:8811'
    : 'http://192.168.3.199:89';

// user服务前缀
const api_user_prefix = '/platform-cloudmanufactureuser';

// 工厂服务前缀
const api_factory_prefix = '/platform-sibeecloudmanufacture';

export { domain, api_user_prefix, api_factory_prefix };
