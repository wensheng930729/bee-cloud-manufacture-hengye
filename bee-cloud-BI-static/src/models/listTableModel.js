import { isFunction } from 'lodash';

export default {
  namespace: 'listTableModel',
  state: {
    data: { list: [], pagination: {} },
  },
  effects: {
    // service-请求列表  handler-列表数据处理方法  paramsHandler-列表请求入参处理方法
    *getListEffect({ payload, listService, dataHandler, callback }, { call, put }) {
      const result = yield call(listService, payload);

      if (result && result.code === 1) {
        let data = { list: [], pagination: { pageSize: 10, current: 1, total: 0 } };

        // 处理数据
        if (result.object.length > 0) {
          //
          if (isFunction(dataHandler)) {
            data = dataHandler(result);
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

        //
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
  },
};
