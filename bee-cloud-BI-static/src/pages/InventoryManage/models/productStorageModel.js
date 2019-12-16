import { getFurnaces, listConfigCodeByType, getRepositoryListByType } from '@/services';

import { saveFinishedProduct } from '../services/productStorageService';

export default {
  namespace: 'productStorageModel',
  state: {
    furnaceOptions: [],
    shiftCodeOptions: [],
    storageOptions: [],
  },
  effects: {
    // 获取炉号列表,班次，仓库列表
    *fetchInitOptionsEffect({ payload, callback }, { call, all, put }) {
      const [furnaceRes, shiftCodeRes, storageRes] = yield all([
        call(getFurnaces, { rq: { types: [] } }),
        call(listConfigCodeByType, { type: 'shifts' }),
        call(getRepositoryListByType, { rq: { types: [] } }),
      ]);

      if (furnaceRes && shiftCodeRes && storageRes) {
        if (furnaceRes.code === 1 && shiftCodeRes.code === 1 && storageRes.code === 1) {
          const allItem = { value: '0', label: '全部' };
          const furnaceOptions = furnaceRes.object.map(item => ({
            value: `${item.furnacesId}`,
            label: item.furnacesName,
            origin: item,
          }));
          furnaceOptions.unshift(allItem);

          const shiftCodeOptions = shiftCodeRes.object.map(item => ({
            value: `${item.code}`,
            label: item.value,
            origin: item,
          }));
          shiftCodeOptions.unshift(allItem);

          const storageOptions = storageRes.object.map(item => ({
            value: `${item.id}`,
            label: item.name,
            origin: item,
          }));

          const result = {
            furnaceOptions,
            shiftCodeOptions,
            storageOptions,
          };
          yield put({
            type: 'fetchInitOptionsReduce',
            payload: { ...result },
          });

          callback(result, 'ok', '');
        } else {
          const error1 =
            furnaceRes.message || shiftCodeRes.message || storageRes.message || 'fetchInit-error';
          callback('', 'error', error1);
        }
      } else {
        callback('', 'error', 'fetchInit-error');
      }
    },
    // 批量入库
    *saveProductEffect({ payload, callback }, { call, put, select }) {
      const result = yield call(saveFinishedProduct, payload);

      if (result && result.code === 1) {
        callback('', 'ok', '入库成功');
      } else if (callback) {
        callback('', 'error', result.message);
      }
    },
  },
  reducers: {
    fetchInitOptionsReduce(state, { payload }) {
      return {
        ...state,
        furnaceOptions: payload.furnaceOptions,
        shiftCodeOptions: payload.shiftCodeOptions,
        storageOptions: payload.storageOptions,
      };
    },
  },
};
