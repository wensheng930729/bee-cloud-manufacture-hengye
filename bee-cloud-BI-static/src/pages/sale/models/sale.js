import moment from 'moment';
import contasts from '../constants';
import * as handlers from '../handlers';
import { saleContractBasic, getProducts } from '@/services';
import { getSaleContractDetail, getCarList } from '../services/services';

const initState = {
  orderInfo: {},
  tableDatas: { PRODUCT: [], RECEIVE: {}, PAY: [], SALE_RECEIVE: [] }, // 用户的基础信息
  parentTabs: [],
  productsOptions: [],
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
};

export default {
  namespace: 'sale',
  state: {
    ...initState,
  },
  effects: {
    *getSaleContractDetail({ payload, callback }, { call, put }) {
      const response = yield call(getSaleContractDetail, payload);
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
          data:
            response.object.receive && response.object.receive.data
              ? response.object.receive.data
              : [],
        });
      }
      callback && callback();
    },
    *getCarList({ payload, callback }, { call, put }) {
      const response = yield call(getCarList, payload);
      if (response.code === 1 && response.object) {
        yield put({
          type: 'setTableData',
          key: contasts.SALE_RECEIVE.key,
          data: { data: response.object, page: response.page },
        });
      }
      callback && callback();
    },
    // 获取列表
    *getListEffect({ payload, callback }, { call, put }) {
      const result = yield call(saleContractBasic, payload);
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
        const error1 = (result && result.message) || 'fetchInit-error';
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
