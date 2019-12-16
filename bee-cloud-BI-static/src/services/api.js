import { api_user_prefix, api_factory_prefix } from '@/constants/prefix';
export default {
  login: {
    api: () => `${api_user_prefix}/authPlatformUser/web/login`,
    type: 'POST',
  },
  logout: {
    api: () => `${api_user_prefix}/authPlatformUser/logout`,
    type: 'POST',
  },
  getSelfInfo: {
    //获取当前用户个人信息
    api: () => `${api_user_prefix}/platform/auth/getSelfInfo`,
    type: 'GET',
  },
  getSelfResource: {
    //登录角色的权限列表
    api: () => `${api_user_prefix}/auth/resource/user`,
    type: 'GET',
  },
  //根据产品类型查询产品列表
  //(产品类别 0 全部 1 主料 2 辅料 3 成品 4 用户定义的类别 5 包含(主料 辅料) 6 包含(主料 成品) 7包含(辅料 成品) 8包含(主料 辅料 成品)
  getProductListByCategory: {
    api: category =>
      `${api_factory_prefix}/configProduct/getProductListByCategory?category=${category}`,
    type: 'GET',
  },
  //根据类别查询设备列表
  //(设备类型（0 矿热炉 1 环保设备 2 特种设备 3 其他设备） 空为查全部){
  //   "types": [
  //     0
  //   ]
  // }
  getDeviceListByType: {
    api: () => `${api_factory_prefix}/configDevice/getDeviceListByType`,
    type: 'POST',
  },
  //根据版本号查询升级描述
  getVersionMessage: {
    api: () => `${api_factory_prefix}/versionUpgrade/getByVersionNum`,
    type: 'GET',
  },
};
