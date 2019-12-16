import router from 'umi/router';
import {
  getEnableReportFormsList,
  // eslint-disable-next-line import/named
  accountLogin,
  logout,
  getSelfResource,
  getSelfInfo,
  getVersionMessage,
} from '../services/index';
// eslint-disable-next-line import/extensions
import { setRoutes } from '../utils/utils.js';
// 初始菜单
// eslint-disable-next-line no-underscore-dangle
const _menus = [];

const initState = {
  login: false,
  user: {}, // 用户的基础信息
  menus: _menus, // 当前账户所选权限的菜单
  versionMessage: [], // 版本信息
};

export default {
  state: {
    ...initState,
  },
  effects: {
    *accountLogin({ payload, callback }, { call, put }) {
      const response = yield call(accountLogin, payload);

      // 清空之前页面缓存
      localStorage.clear();
      sessionStorage.clear();
      if (response.code === 1 && response.object) {
        localStorage.setItem('sysToken', response.object.sysToken);
        yield put({
          type: 'setLoginInfo',
          payload: response.object,
        });
      }
      // eslint-disable-next-line no-unused-expressions
      callback && callback();
    },
    *getSelfInfo({ payload, callback }, { call, put }) {
      const response = yield call(getSelfInfo, payload);
      if (response.code === 1) {
        yield put({
          type: 'setLoginInfo',
          payload: response.object,
        });
        // eslint-disable-next-line no-unused-expressions
        callback && callback();
      } else {
        sessionStorage.clear();
        localStorage.clear();
        // message.info('登录信息验证失败，请先登录！')
        setTimeout(() => {
          router.push('/login');
        }, 1000);
      }
    },
    *logout({ payload, callback }, { call, put }) {
      const response = yield call(logout, payload);
      if (response.code === 1) {
        router.push('/login');
        sessionStorage.clear();
        localStorage.clear();
        yield put({
          type: 'logoutReducer',
          payload: response.object,
        });
      } else {
        // eslint-disable-next-line no-unused-expressions
        callback && callback();
      }
    },
    *getMenus({ payload, callback }, { call, put }) {
      const response = yield call(getSelfResource, payload);
      if (response.code === 1) {
        yield put({
          type: 'setMenus',
          payload: response.object || [],
        });
      }
      // eslint-disable-next-line no-unused-expressions
      callback && callback(response.object);
    },
    *getEnableReportFormsListEffect({ payload, callback }, { call, put }) {
      const response = yield call(getEnableReportFormsList, payload);
      if (response.code === 1) {
        yield put({
          type: 'getEnableReportFormsListReduce',
          payload: response.object,
        });
      } else {
        // eslint-disable-next-line no-unused-expressions
        callback && callback(response.message);
      }
    },
    // 版本信息action
    *getVersionMessage({ payload, callback }, { call, put }) {
      const response = yield call(getVersionMessage, payload);
      if (response.code === 1) {
        yield put({
          type: 'getVersionMessageList',
          payload: response.object,
        });
      }
      callback && callback(response.message);
    },
  },
  reducers: {
    setLoginInfo(state, action) {
      // 设置用户当前选中的权限与角色
      return {
        ...state,
        user: action.payload,
        login: true,
      };
    },
    setMenus(state, action) {
      const datas = action.payload.concat(_menus);

      return {
        ...state,
        menus: setRoutes(datas),
      };
    },
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    logoutReducer(state, action) {
      return { ...initState, login: true };
    },
    getEnableReportFormsListReduce(state, { payload }) {
      return { ...state, tabPages: payload };
    },
    getVersionMessageList(state, { payload }) {
      return { ...state, versionMessage: payload };
    },
  },
};
