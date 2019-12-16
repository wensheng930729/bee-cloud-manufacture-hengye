import moment from 'moment';
import { cloneDeep } from 'lodash';
import contasts from '../constants';
import * as handlers from '../handlers';
import { buyContractBasic, getProducts } from '@/services';
import { getBuyContractDetail, getCarList } from '../services/services';

const initState = {
  orderInfo: {},
  tableDatas: { PRODUCT: [], RECEIVE: {}, PAY: [] }, // 用户的基础信息
  menus: {}, // 当前账户所选权限的菜单
  parentTabs: [],
  listSearchParams: {
    currentPage: 1,
    orderStage: '',
    pageSize: 10,
    productId: '',
    searchCount: true,
    dateRange: {
      startTime: moment()
        .subtract(30, 'days')
        .format('YYYY-MM-DD'),
      endTime: moment().format('YYYY-MM-DD'),
    },
    contractNum: '',
  },
  dataObj: {},
  productsOptions: [],
};

export default {
  namespace: 'purchase',
  state: {
    ...initState,
  },
  effects: {
    *getBuyContractDetail({ payload, callback }, { call, put }) {
      const response = yield call(getBuyContractDetail, payload);
      if (response.code === 1 && response.object) {
        yield put({
          type: 'setOrderInfo',
          data: response.object,
        });
        yield put({
          type: 'setTableData',
          key: contasts.PRODUCT.key,
          data: handlers.productData(response.object.contract || {}),
        });
        yield put({
          type: 'setTableData',
          key: contasts.PAY.key,
          data: response.object.pay && response.object.pay.data ? response.object.pay.data : [],
        });
      }
      callback && callback();
    },
    *getCarList({ payload, callback }, { call, put }) {
      const response = yield call(getCarList, payload);
      if (response.code === 1 && response.object) {
        yield put({
          type: 'setTableData',
          key: contasts.RECEIVE.key,
          data: { data: response.object, page: response.page },
        });
      }
      callback && callback();
    },
    // 获取列表
    *getListEffect({ payload, callback }, { call, put }) {
      const result = yield call(buyContractBasic, payload);
      // 表格数据
      if (result && result.code === 1) {
        const {
          page: { currentPage, pageSize, totalRecords },
          object,
        } = result;

        const list = object.data.map(item => ({
          key: item.contractBusinessId,
          ...item,
        }));

        // eslint-disable-next-line max-len
        const dataSource = {
          list,
          count: object.count,
          pagination: { total: totalRecords, current: currentPage, pageSize },
        };

        yield put({
          type: 'getListReduce',
          payload: {
            listSearchParams: { ...payload },
            dataObj: dataSource,
          },
        });
      } else if (callback) {
        const error1 = (result && result.message) || 'fetchInit-error';
        callback('', 'error', error1);
      }
    },

    *getProductsEffect({ callback }, { call, put }) {
      const result = yield call(getProducts);

      if (result && result.code === 1) {
        const datas = result.object.map(item => ({
          value: `${item.productId}`,
          label: item.productName,
        }));
        datas.unshift({ value: '', label: '全部' });
        yield put({
          type: 'getProductsReduce',
          payload: datas,
        });
        callback(datas, 'ok');
      } else if (callback) {
        const error1 = (result && result.message) || 'getProductsReduce-error';
        callback('', 'error', error1);
      }
    },
  },
  reducers: {
    setTableData(state, { data, key }) {
      const tableDatas = { ...state.tableDatas };
      tableDatas[key] = data;
      // 设置用户当前选中的权限与角色
      return { ...state, tableDatas };
    },
    setOrderInfo(state, { data }) {
      return { ...state, orderInfo: data };
    },
    cacheSearchParamsReduce(state, { payload }) {
      return {
        ...state,
        listSearchParams: { ...state.listSearchParams, ...payload },
      };
    },
    cacheDataListReduce(state, { payload }) {
      return { ...state, dataObj: payload };
    },
    getProducts(state, { payload }) {
      return { ...state, products: payload };
    },
    getListReduce(
      state,
      {
        payload: { listSearchParams, dataObj },
      },
    ) {
      return { ...state, listSearchParams, dataObj };
    },
    getProductsReduce(state, { payload }) {
      return { ...state, productsOptions: payload };
    },
  },
};
