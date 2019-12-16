
import { getSettleBuyPopupWindow, saveSettleBuyPopupWindow } from '../services/services';
import { message } from 'antd'

export default {
  namespace: 'settleModel',
  state: {
    modalInfo: { settles: [] },//弹出层数据
  },
  effects: {
    // 弹窗数据
    *getSettleBuyPopupWindow({ payload, callback }, { call, all, put }) {
      const response = yield call(getSettleBuyPopupWindow, payload);
      if (response && response.code === 1) {
        yield put({
          type: 'setModalInfo',
          payload: { ...response.object },
        });
        callback();
      } else {
        message.error(response.message)
      }
    },
    // 弹窗数据
    *saveSettleBuyPopupWindow({ payload, callback }, { call, all, put }) {
      const response = yield call(saveSettleBuyPopupWindow, payload);
      if (response && response.code === 1) {
        message.success(response.message)
        callback();
      } else {
        message.error(response.message)
      }
    }
  },
  reducers: {
    setModalInfo(state, { payload }) {
      return {
        ...state,
        modalInfo: payload
      };
    },
  },
};
