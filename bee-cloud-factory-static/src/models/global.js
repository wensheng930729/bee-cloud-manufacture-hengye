export default {
  namespace: 'global',
  state: {
    login: false,
    user: {}, //用户的基础信息
    menus: [], //当前账户所选权限的菜单
  },

  effects: {},
  reducers: {
    setUserInfo(state, action) {
      return {
        ...state,
        login: true,
        user: action.payload
      }
    },
    setMenus(state, action) {
      return {
        ...state,
        menus: action.payload
      }
    }
  }
}