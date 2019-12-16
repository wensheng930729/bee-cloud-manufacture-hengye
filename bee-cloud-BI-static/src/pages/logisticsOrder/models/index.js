import moment from 'moment';
import { getProducts, getProductCategories, getAllLogisticsContractList } from '@/services';

export default {
  namespace: 'LogisticsOrder',
  state: {
    listSearchParams: {
      currentPage: 1,
      pageSize: 10,
      searchCount: true,
      productId: '',
      dateRange: {
        startTime: moment()
          .subtract(30, 'days')
          .format('YYYY-MM-DD'),
        endTime: moment().format('YYYY-MM-DD'),
      },
      contractNum: '', // 合同编号
    },
    dataObj: {},
    productsOptions: [],
  },
  effects: {
    // 获取
    *getListEffect({ payload, callback }, { call, put }) {
      const newPayload = { ...payload };
      const result = yield call(getAllLogisticsContractList, newPayload);
      // 表格数据
      if (result && result.code === 1) {
        const {
          page: { currentPage, pageSize, totalRecords },
          object,
        } = result;

        const list = object.map(item => ({
          key: item.contractBusinessId,
          ...item,
        }));

        // eslint-disable-next-line max-len
        const dataSource = {
          list,
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
