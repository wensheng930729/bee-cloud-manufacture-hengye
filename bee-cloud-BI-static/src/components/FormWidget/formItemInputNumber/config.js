const config = {
  cn: {
    pattern: /^[\u4e00-\u9fa5_]+$/g,
    message: '只能输入中文/下划线',
  },
  en: {
    pattern: /^[a-zA-Z_]+$/g,
    message: '只能输入英文/下划线',
  },
  http: {
    pattern: /(http|https):\/\/([\w.]+\/?)\S*/,
    message: '不符合URL规范,检查格式是否正确',
  },
  // 备注  简介  描述
  remark: {
    max: 100,
    required: false,
    patternMessage: '不超过100字符',
  },
  // 知识图谱--标题,菜单管理——子菜单名,子级用户组，客户名称
  title: {
    min: 2,
    max: 20,
    pattern: '^(?!_)(?!.*?_$)[a-zA-Z0-9_/u4e00-/u9fa5]+$',
    patternMessage: '2-20字符,包含中文,字母,数字,下划线，不能以下划线开头或结尾',
  },
  // 菜单管理——参数
  param: {
    pattern: "((^http)|(^https)|(^ftp)):'/'/(\\w)+",
    patternMessage: '请输入正确的URL地址',
  },
  // 用户名
  userName: {
    min: 2,
    max: 20,
    patternMessage: '2-20字符',
  },
  // 密码
  password: {
    min: 2,
    max: 20,
    pattern: '^[A-Za-z0-9]+$',
    patternMessage: '2-20字符,只含英文数字',
  },
  // IP地址
  ip: {
    pattern: /(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\.(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\.(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\.(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])$/,
    patternMessage: 'ip格式,如：127.0.0.1',
  },
};

export default config;
