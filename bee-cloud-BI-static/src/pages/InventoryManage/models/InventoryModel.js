

import {
  saveInventoryInfo,
  getInventoryTypeService,
  getinventoryTypeDescService,
  createInventoryOrder
} from '../services/inventoryService';
import { message } from 'antd';

const initStore = {
  types: [],
  typeDesc: [],
  step1Data: { categoryId: [], inventoryTypeCode: undefined },
  step2Data: {}//待盘点数据
}

export default {
  namespace: 'InventoryModel',
  state: {
    ...initStore
  },
  effects: {
    // 获取炉号列表,班次，仓库列表
    *getInventoryType({ payload, callback }, { call, all, put }) {
      const response = yield call(getInventoryTypeService, {})
      if (response.code === 1 && response.object && response.object.length) {
        const result = response.object.map(item => ({
          value: `${item.inventoryTypeCode}`,
          label: item.inventoryTypeDesc
        }));
        yield put({
          type: 'setTypes',
          payload: [...result],
        });
        //根据盘点分类获得下拉列表详细
        yield put({
          type: 'getinventoryTypeDescService',
          payload: { inventoryTypeCode: result[0].value },
          callback: (allChecks) => callback({ inventoryTypeCode: result[0].value, categoryId: allChecks })
        });
      } else {

      }
    },
    // 根据盘点分类获得下拉列表详细
    * getinventoryTypeDescService({ payload, callback }, { call, all, put }) {
      const response = yield call(getinventoryTypeDescService, payload)
      if (response.code === 1 && response.object && response.object.length) {
        const resultData = treeDataHandler(response.object);
        yield put({
          type: 'setTypesDesc',
          payload: [...resultData.list],
        });
        callback(resultData.allChecks);
      } else {
        yield put({
          type: 'setTypesDesc',
          payload: [],
        });
        callback(undefined)
      }
    },
    // 创建库存盘点单，并返回待盘点数据
    * createInventoryOrder({ payload, callback }, { call, all, put }) {
      const response = yield call(createInventoryOrder, payload)
      if (response.code === 1 && response.object) {
        yield put({
          type: 'clearStep2Data',
          payload: {}
        });
        yield put({
          type: 'setStep2Data',
          payload: { ...response.object },
        });
        callback();
      } else {
        yield put({
          type: 'clearStep2Data',
          payload: {}
        });
        message.error(res.message)
      }
    },
    //第一步参数变更
    * step1DataChange({ payload, callback }, { call, all, put }) {
      yield put({
        type: 'setStep1Data',
        payload,
      });
    },
    // 根据盘点分类获得下拉列表详细
    * step2DataChange({ payload, callback }, { call, all, put }) {
      yield put({
        type: 'setStep2Data',
        payload: payload,
      });
    },
    // 保存盘点单
    * saveInventoryInfo({ payload, callback }, { call, all, put }) {
      const bodyParams = savaBodyHandler(payload);
      const response = yield call(saveInventoryInfo, bodyParams)
      if (response.code === 1) {
        yield put({
          type: 'initData',
          payload: {},
        })
      }
      callback && callback(response);
    }
  },
  reducers: {
    setTypes(state, { payload }) {
      return {
        ...state,
        types: payload
      };
    },
    setTypesDesc(state, { payload }) {
      return {
        ...state,
        typeDesc: payload
      };
    },
    setStep1Data(state, { payload }) {
      return {
        ...state,
        step1Data: payload
      }
    },
    clearStep2Data(state, { payload }) {
      return {
        ...state,
        step2Data: {}
      };
    },
    setStep2Data(state, { payload }) {
      return {
        ...state,
        step2Data: { ...state.step2Data, ...payload }
      };
    },
    initData(state, { payload }) {
      return {
        ...initStore
      };
    }
  },
};

//树结构处理
const treeDataHandler = (data) => {
  let list = [{ title: '全选', value: 'all', key: 'all' }]
  let allChecks = []
  list[0].children = data.map(item => {
    allChecks.push(item.categoryId);
    return { title: item.categoryName, value: item.categoryId, key: item.categoryId }
  });
  return { list, allChecks }
}

//保存盘点单 参数处理
const savaBodyHandler = (_body) => {
  const { inventoryName, inventoryOrderId, list, remarks } = _body;
  return { inventoryDetail: list, inventoryName, inventoryOrderId, remarks };
}