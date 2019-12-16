import { getProducts, getRepositoryListByType, getProductSpecByProductId } from '@/services';

import {
  saveFreeStorageService,
  getFreeStorageService,
  getDetailService,
  saveProductService,
} from '../services/purchaseStorageService';

export default {
  namespace: 'purchaseWarehousingModel',
  state: {
    saveFreeStorageParams: {},

    storageOptions: [],
    productsOptions: [],
    productSpecOptions: [],

    // 展示已新增的入库详情信息
    detailDataWithAdd: [],
    // 详情
    detailData: {},
  },
  effects: {
    *cacheSaveFreeStorageEffect({ payload, callback }, { call, put }) {
      const { productId } = payload;
      if (productId) {
        yield put({
          type: 'getProductSpecByProductIdEffect',
          payload: productId,
          callback,
        });
        yield put({
          type: 'cacheSaveFreeStorage',
          payload: { ...payload, productSpecId: '' },
        });
      } else {
        yield put({
          type: 'cacheSaveFreeStorage',
          payload,
        });
      }
    },
    // 待入库-列表头部的  "新增入库"
    *saveFreeStorageEffect({ payload, callback }, { call, put, select }) {
      const { saveFreeStorageParams, productSpecOptions } = yield select(
        state => state.purchaseWarehousingModel,
      );
      const params = {
        ...saveFreeStorageParams,
        productSpecName: productSpecOptions.filter(
          item => item.value === saveFreeStorageParams.productSpecId,
        )[0].label,
      };

      const result = yield call(saveFreeStorageService, params);
      if (result && result.code === 1) {
        callback('', 'ok', '新增成功');
        yield put({
          type: 'cacheSaveFreeStorageEffect',
          payload: { storageId: '', productId: '', productSpecId: '', productNumber: '' },
        });
      } else {
        callback('', 'error', result.message);
      }
    },
    // 已入库-列表头部的  "查看新增的入库详情"
    *getFreeStorageEffect({ payload, callback }, { call, put }) {
      const result = yield call(getFreeStorageService, payload);

      if (result && result.code === 1) {
        yield put({
          type: 'getFreeStorageReduce',
          payload: result.object,
        });
        callback('', 'ok', '');
      } else {
        callback('', 'error', result.message);
      }
    },
    // 查看详情
    *getDetailEffect({ payload, callback }, { call, put, select }) {
      const { storageOptions } = yield select(state => state.purchaseWarehousingModel);
      if (storageOptions.length === 0) {
        yield put({
          type: 'fetchInitOptionsEffect',
          payload: {},
          callback,
        });
      }

      const result = yield call(getDetailService, payload);

      if (result && result.code === 1) {
        yield put({
          type: 'getDetailReduce',
          payload: result.object,
        });
      } else {
        callback('', 'error', result.message);
      }
    },
    // 详情界面- 提交按钮
    *saveProductEffect({ payload, callback }, { call, put, select }) {
      const result = yield call(saveProductService, payload);
      if (result && result.code === 1) {
        callback('', 'ok', '提交成功');
      } else {
        callback('', 'error', result.message);
      }
    },
    // 获取产品列表, 仓库列表
    *fetchInitOptionsEffect({ payload, callback }, { call, all, put }) {
      const [storageRes, productRes] = yield all([
        call(getRepositoryListByType, { rq: { types: [] } }),
        call(getProducts, {}),
      ]);

      if (storageRes && productRes) {
        if (storageRes.code === 1 && productRes.code === 1) {
          const allItem = { value: '0', label: '全部' };
          const storageOptions = storageRes.object.map(item => ({
            value: `${item.id}`,
            label: item.name,
            origin: item,
          }));

          const productsOptions = productRes.object.map(item => ({
            value: `${item.productId}`,
            label: item.productName,
            origin: item,
          }));

          const result = {
            storageOptions,
            productsOptions,
          };
          yield put({
            type: 'fetchInitOptionsReduce',
            payload: { ...result },
          });

          callback(result, 'ok', '');
        } else {
          const error1 = storageRes.message || productRes.message || 'fetchInit-error';
          callback('', 'error', error1);
        }
      } else {
        callback('', 'error', 'fetchInit-error');
      }
    },
    // 根据产品id 获取规格列表
    *getProductSpecByProductIdEffect({ payload, callback }, { call, put, select }) {
      const result = yield call(getProductSpecByProductId, payload);
      let productSpecOptions = [];
      if (result && result.code === 1) {
        if (result.object.length > 0) {
          productSpecOptions = result.object.map(item => ({
            value: `${item.id}`,
            label: item.specName,
            origin: item,
          }));

          yield put({
            type: 'getProductSpecByProductIdReduce',
            payload: productSpecOptions,
          });
        }
      } else if (callback) {
        callback('', 'error', result.message);
      }
    },
  },
  reducers: {
    // 待入库-列表头部的  "新增入库"-缓存字段
    cacheSaveFreeStorage(state, { payload }) {
      return {
        ...state,
        saveFreeStorageParams: { ...state.saveFreeStorageParams, ...payload },
      };
    },
    getFreeStorageReduce(state, { payload }) {
      return {
        ...state,
        detailDataWithAdd: payload,
      };
    },
    getDetailReduce(state, { payload }) {
      return {
        ...state,
        detailData: payload,
      };
    },
    fetchInitOptionsReduce(state, { payload }) {
      return {
        ...state,
        storageOptions: payload.storageOptions,
        productsOptions: payload.productsOptions,
      };
    },
    getProductSpecByProductIdReduce(state, { payload }) {
      return {
        ...state,
        productSpecOptions: payload,
      };
    },
  },
};
