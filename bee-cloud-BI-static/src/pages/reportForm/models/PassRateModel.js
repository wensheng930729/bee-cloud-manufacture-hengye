import { reportType as REPORT_TYPE, tabPages } from '@/consts/reportForm';
import { getPassRateReportForm } from '../services';
import { getEnableReportFormsList } from '@/services/index';
import { deaProducelListData, dealListParams } from '../utils';

export default {
  namespace: 'PassRateModel',
  state: {
    parentTabs: [],

    listSearchParams: {
      reportType: REPORT_TYPE.passRate.key,
      currentPage: 1,
      orderStage: '',
      pageSize: 10,
      searchCount: true,

      productId: '',
      year: '2019',
    },
    dataList: { list: [], pagination: {} },
    columns: [],
  },
  effects: {
    // 获取tab页
    *getTabPagesEffect({ payload, callback }, { call, put }) {
      const { type } = payload;
      const result = yield call(getEnableReportFormsList, { type });
      if (result && result.code === 1 && Array.isArray(result.object) && result.object.length > 0) {
        const parentTabs = [];

        result.object.forEach(item => {
          if (tabPages[Number(item.code)]) {
            parentTabs.push({
              ...tabPages[Number(item.code)],
              containerL: null,
            });
          }
        });

        yield put({
          type: 'getTabPagesReduce',
          payload: parentTabs,
        });

        callback('', 'ok', '');
      } else {
        callback('', 'error', result.message);
      }
    },

    // 获取列名和表格数据
    *getListEffect({ payload, callback }, { call, put, select }) {
      const { listSearchParams } = yield select(state => state.PassRateModel);

      const dealListParamsObj = {
        preParams: listSearchParams,
        curParams: { ...payload },
      };

      const { requestParams, listSearchParams: newlistSearchParams } = dealListParams(
        dealListParamsObj,
      );

      const result = yield call(getPassRateReportForm, requestParams);

      const dealListDataParams = {
        originData: result,
      };
      const newData = deaProducelListData(dealListDataParams);

      if (typeof newData !== 'string') {
        const { data, columns } = newData;

        yield put({
          type: 'cacheDataListReduce',
          payload: { dataList: data, columns },
        });

        yield put({
          type: 'cacheSearchParamsReduce',
          payload: newlistSearchParams,
        });
      } else if (!newData.includes('该产品没有合格线')) {
        callback('', 'error', newData);
        yield put({
          type: 'cacheDataListReduce',
          payload: { dataList: [], columns: [] },
        });

        yield put({
          type: 'cacheSearchParamsReduce',
          payload: newlistSearchParams,
        });
      } else {
        yield put({
          type: 'cacheDataListReduce',
          payload: { dataList: [], columns: [] },
        });

        yield put({
          type: 'cacheSearchParamsReduce',
          payload: newlistSearchParams,
        });
      }
    },
  },
  reducers: {
    getTabPagesReduce(state, { payload }) {
      return { ...state, parentTabs: payload };
    },
    cacheSearchParamsReduce(state, { payload }) {
      return {
        ...state,
        listSearchParams: { ...state, ...payload },
      };
    },
    cacheDataListReduce(state, { payload }) {
      return { ...state, ...payload };
    },
  },
};
