import {
  getStorageTreeService,
  getStockByStorageIdService,
  getStockByStorageTypeService,
} from '../services/existingQuantityQueryService';

export default {
  namespace: 'existingQuantityQueryModel',
  state: {
    storageTreeData: [],
    stockData: { list: [], pagination: false },
  },
  effects: {
    //
    *getStorageTreeEffect({ payload, callback }, { call, put, select }) {
      const result = yield call(getStorageTreeService, payload);
      const storageTreeData = [{ key: 'all', title: '全部', children: [] }];

      if (result && result.code === 1) {
        if (result.object.length > 0) {
          const datas = result.object.map(item => {
            const first = {
              key: `${item.storageTypeId}`,
              title: item.storageTypeName,
            };
            if (item.storages) {
              first.children = item.storages.map(nextItem => ({
                key: `${item.storageTypeId}-${nextItem.key}`,
                title: nextItem.value,
              }));
            }
            return first;
          });
          storageTreeData[0].children = datas;
        }

        yield put({
          type: 'getStorageTreeReduce',
          payload: storageTreeData,
        });
      } else if (callback) {
        callback('', 'error', result.message);
      }
    },
    // 根据产品id 获取产品
    *getStockByStorageIdEffect({ payload, callback }, { call, put }) {
      const result = yield call(getStockByStorageIdService, payload);

      if (result && result.code === 1) {
        yield put({
          type: 'getStockByStorageIdReduce',
          payload: result,
        });
      } else if (callback) {
        callback('', 'error', result.message);
      }
    },
    // 根据产品类型 获取产品
    *getStockByStorageTypeEffect({ payload, callback }, { call, put }) {
      const result = yield call(getStockByStorageTypeService, payload);

      if (result && result.code === 1) {
        yield put({
          type: 'getStockByStorageIdReduce',
          payload: result,
        });
      } else if (callback) {
        callback('', 'error', result.message);
      }
    },
  },
  reducers: {
    getStorageTreeReduce(state, { payload }) {
      return {
        ...state,
        storageTreeData: payload,
      };
    },
    getStockByStorageIdReduce(state, { payload }) {
      const list = payload.object.map(item => ({
        key: `${item.productSpecName}-${item.productName}-${item.storageName}`,
        ...item,
      }));

      const pagination = {
        current: payload.page ? payload.page.currentPage : 1,
        pageSize: payload.page ? payload.page.pageSize : 10,
        total: payload.page ? payload.page.totalRecords : 0,
      };

      return {
        ...state,
        stockData: { list, pagination },
      };
    },
  },
};
