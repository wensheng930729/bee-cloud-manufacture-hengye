import { getRelationContract, machineBindContract, ignoreRecoveryMachine } from '../services';

const initState = {
  orderInfo: {},
  tableDatas: { PRODUCT: [], RECEIVE: {}, PAY: [] }, // 用户的基础信息
  parentTabs: [],
  listSearchParams: {
    currentPage: 1,
    orderStage: '',
    pageSize: 10,
    searchCount: true,
  },
  dataObj: {},
};

export default {
  namespace: 'contractRelated',
  state: {
    ...initState,
  },
  effects: {
    // 获取列表
    *getListEffect({ payload, callback }, { call, put }) {
      const result = yield call(getRelationContract, payload);
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
    // 磅单关联合同提交
    *machineBindContract({ payload, callback }, { call }) {
      const result = yield call(machineBindContract, payload);
      if (result && result.code === 1) {
        callback('', 'ok', '提交成功');
      } else {
        callback('', 'error', result.message);
      }
    },
    // 磅单忽略
    *ignoreRecoveryMachine({ payload, callback }, { call }) {
      const result = yield call(ignoreRecoveryMachine, payload);
      if (result && result.code === 1) {
        callback('', 'ok', '提交成功');
      } else {
        callback('', 'error', result.message);
      }
    },
  },
  reducers: {
    getListReduce(
      state,
      {
        payload: { listSearchParams, dataObj },
      },
    ) {
      return { ...state, listSearchParams, dataObj };
    },
  },
};
