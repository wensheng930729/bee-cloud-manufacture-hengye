import { isFunction } from 'lodash';
import moment from 'moment';

const commonModel = ({ namespace, getListService, listServiceHandle }) => ({
  namespace,
  state: {
    listSearchParams: {
      currentPage: 1,
      orderStage: '',
      pageSize: 10,
      searchCount: true,

      supplierid: '0',

      dateRange: {
        startTime: moment()
          .subtract(30, 'days')
          .format('YYYY-MM-DD'),
        endTime: moment().format('YYYY-MM-DD'),
      },
    },
    data: { list: [], pagination: {} },

    supplierOptions: [],
    repositoryOptions: [],
  },
  effects: {
    //
    *getListEffect({ payload, callback }, { call, put, select }) {
      const {
        listSearchParams,
        listSearchParams: {
          dateRange: { startTime, endTime },
          ...rest
        },
      } = yield select(state => state[namespace]);

      const newPayload = payload.dateRange ? {} : payload;
      const requestParams = {
        startTime: payload.dateRange ? payload.dateRange.startTime : startTime,
        endTime: payload.dateRange ? payload.dateRange.endTime : endTime,
        ...rest,
        ...newPayload,
      };

      const newlistSearchParams = { ...listSearchParams, ...payload };

      yield put({
        type: 'cacheSearchParamsReduce',
        payload: newlistSearchParams,
      });

      const result = yield call(getListService, requestParams);

      if (result && result.code === 1) {
        let data = { list: [], pagination: { pageSize: 10, current: 1, total: 0 } };

        if (result.object.length > 0) {
          if (isFunction(listServiceHandle)) {
            data = listServiceHandle(result);
          } else {
            const { currentPage, pageSize, totalRecords } = result.page;
            const pagination = {
              pageSize,
              current: currentPage,
              total: totalRecords,
            };

            data.list = result.object;
            data.pagination = pagination;
          }
        }

        yield put({
          type: 'getListReduce',
          payload: data,
        });

        callback('', 'ok', '');
      } else {
        callback('', 'error', result.message);
      }
    },
  },
  reducers: {
    getListReduce(state, { payload }) {
      return { ...state, data: payload };
    },
    cacheSearchParamsReduce(state, { payload }) {
      return {
        ...state,
        listSearchParams: { ...state.listSearchParams, ...payload },
      };
    },
  },
});

export default commonModel;
