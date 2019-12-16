import { api_user_prefix } from '@/constants/prefix';
export default {
  login: {
    api: () => `${api_user_prefix}/authPlatformUser/web/login`,
    type: 'POST'
  },
  getSelfResource: { //登录角色的权限列表
    api: () => `${api_user_prefix}/auth/resource/user`,
    type: "GET"
  }
}
