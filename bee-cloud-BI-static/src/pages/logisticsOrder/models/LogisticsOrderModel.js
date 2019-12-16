import { cloneDeep, random } from 'lodash';
import { chineseToNum, numToChinese } from '@/utils/numToChinese';
import {
  getLogisticsBatchSectionInfo,
  getLocationList,
  getTransportSectionAllDetail,
  saveLogisticsSectionInfo,
} from '../services';

export default {
  namespace: 'LogisticsOrderModel',
  state: {
    requestType: 'buyLogisticsBatch',

    // 位置下拉框数据
    locationOptions: [],
    // 详情信息
    contractInfoDTO: {},

    // 批次列表数据
    logisticsBatchDTOS: [],
    // 手风琴当前展开的项---根据批次batchId来设置
    collapseActivityKey: '',

    // 阶段列表数据
    transportSectionOBJ: {},
    // 阶段列表 原始数据
    transportSectionOBJOrigin: {},
  },
  effects: {
    // 获取物流详情
    *getLogisticsBatchInfoEffect({ payload, callback }, { call, all, put, select }) {
      const [logisticsBatchInfoRes, locationListRes] = yield all([
        call(getLogisticsBatchSectionInfo, payload),
        call(getLocationList, {}),
      ]);

      if (locationListRes && logisticsBatchInfoRes) {
        if (locationListRes.code === 1 && logisticsBatchInfoRes.code === 1) {
          const locationOptions = locationListRes.object.map(item => ({
            value: `${item.id}`,
            label: item.name,
            origin: item,
          }));

          yield put({
            type: 'getLogisticsBatchInfoReduce',
            payload: {
              locationOptions,
              logisticsBatchInfo: logisticsBatchInfoRes.object,
              requestType: payload.requestType,
            },
          });
        } else {
          const error1 =
            locationListRes.message ||
            logisticsBatchInfoRes.message ||
            'getLogisticsBatchInfoEffect-error';
          callback('', 'error', error1);
        }
      } else if (callback) {
        callback('', 'error', 'getLogisticsBatchInfoEffect-error');
      }
    },
    // 新增阶段
    *addSectionListEffect({ payload }, { put, select }) {
      const { transportSectionOBJ } = yield select(state => state.LogisticsOrderModel);

      const newData = cloneDeep(transportSectionOBJ);

      const currenttransportSectionList = newData[payload.batchId] || [];
      const transportSectionId = `${random(0, 9999)}_section`;
      currenttransportSectionList.push({
        transportSectionId,
        key: transportSectionId,
        transportSectionName: `第${numToChinese(currenttransportSectionList.length + 1)}阶段`,
        transportMode: '1',
        startingPlaceId: '1',
        arrivalPlaceId: '1',
        toFactory: '0',
        editable: true,
      });

      newData[payload.batchId] = currenttransportSectionList;

      yield put({
        type: 'cacheSectionListEffect',
        payload: newData,
      });
    },
    // 删除阶段
    *deleteSectionListEffect({ payload }, { put, select }) {
      const { collapseActivityKey: batchId, transportSectionId } = payload;

      const { transportSectionOBJ } = yield select(state => state.LogisticsOrderModel);

      const newData = cloneDeep(transportSectionOBJ);

      const currenttransportSectionList = newData[batchId];

      const newList = currenttransportSectionList.filter(
        item => item.transportSectionId !== transportSectionId,
      );

      newData[batchId] = newList;

      yield put({
        type: 'cacheSectionListEffect',
        payload: newData,
      });
    },

    // 存储改变后的----阶段列表到store中
    *cacheSectionListEffect({ payload }, { put, select }) {
      let result = payload;
      if (!result) {
        const { transportSectionOBJOrigin } = yield select(state => state.LogisticsOrderModel);
        result = cloneDeep(transportSectionOBJOrigin);
      }
      yield put({
        type: 'cacheSectionListReduce',
        payload: result,
      });
    },
    *cacheCollapseActivityKeyEffect({ payload }, { put }) {
      yield put({
        type: 'cacheCollapseActivityKeyReduce',
        payload,
      });
    },

    // 新增批次
    *addBatchListEffect({ payload }, { put, select }) {
      const { logisticsBatchDTOS } = yield select(state => state.LogisticsOrderModel);

      const newData = cloneDeep(logisticsBatchDTOS);

      newData.push({
        batchId: `${random(999, 9999)}_batch`,
        batchName: `第${numToChinese(newData.length + 1)}批次`,
        arrivalVolume: 0,
        contractBusinessId: '',
        freightVolume: '',
      });

      yield put({
        type: 'addBatchListReduce',
        payload: newData,
      });
    },

    // 确认保存 ----阶段列表(注:只保存新增的)
    *saveSectionListEffect({ payload, callback }, { select, put, call }) {
      const {
        contractInfoDTO: { contractBusinessId },
        transportSectionOBJ,
        collapseActivityKey,
        requestType,
      } = yield select(state => state.LogisticsOrderModel);

      const currentData = transportSectionOBJ[collapseActivityKey];
      if (!currentData || currentData.length === 0) {
        callback('', 'error', '暂无数据可保存');
      } else {
        const listDisabled = currentData.filter(item => item.disabled);
        const list = currentData.filter(item => !item.disabled);
        if (list.length === 0) {
          callback('', 'error', '暂无数据可保存');
          return;
        }
        const newList = list.map((item, index) => {
          const { editable, transportSectionId, key, ...rest } = item;
          const transportSection = listDisabled.length + index;
          return {
            ...rest,
            transportSection,
          };
        });
        const params = {
          batchId: collapseActivityKey.includes('batch') ? '' : collapseActivityKey,
          contractBusinessId,
          transportSectionDTOS: newList,
          requestType,
        };

        const result = yield call(saveLogisticsSectionInfo, params);

        if (result && result.code === 1) {
          callback('', 'ok', '保存批次及阶段成功');
        } else if (callback) {
          const error1 = (result && result.message) || 'saveLogisticsSectionInfo-error';
          callback('', 'error', error1);
        }
      }
    },
    /*
     *
     *
     *
     *
     */
    // 获取阶段下面的详情 {transportSectionId}
    *getTransportSectionAllDetailEffect({ payload, callback }, { call, put, select }) {
      const result = yield call(getTransportSectionAllDetail, payload);

      if (result && result.code === 1) {
      } else if (callback) {
        const error1 = (result && result.message) || 'getTransportSectionAllDetail-error';
        callback('', 'error', error1);
      }
    },

    /*
     *
     *
     *
     *
     */

    // 保存批次及阶段信息
    *saveLogisticsSectionInfoEffect({ payload, callback }, { call, put, select }) {
      const result = yield call(saveLogisticsSectionInfo, payload);

      if (result && result.code === 1) {
        callback('', 'ok', '保存成功');
      } else if (callback) {
        const error1 = (result && result.message) || 'saveLogisticsSectionInfo-error';
        callback('', 'error', error1);
      }
    },
  },
  reducers: {
    getLogisticsBatchInfoReduce(state, { payload }) {
      const {
        locationOptions,
        requestType,
        logisticsBatchInfo: { contractInfoDTO, logisticsBatchDTOS: oldlogisticsBatchDTOS },
      } = payload;

      const transportSectionOBJ = {};
      const logisticsBatchDTOS = [];

      let collapseActivityKey = '';

      if (Array.isArray(oldlogisticsBatchDTOS) && oldlogisticsBatchDTOS.length > 0) {
        oldlogisticsBatchDTOS.forEach(item => {
          const { transportSectionDTOS, ...rest } = item;
          logisticsBatchDTOS.push(rest);
          transportSectionOBJ[item.batchId] = item.transportSectionDTOS.map(nextItem => ({
            ...nextItem,
            key: nextItem.transportSectionId,
            editable: true,
            disabled: true,
          }));
        });
      }

      if (logisticsBatchDTOS.length > 0) {
        collapseActivityKey = `${logisticsBatchDTOS[0].batchId}`;
      }

      return {
        ...state,
        requestType,
        // 位置下拉框数据
        locationOptions,
        // 详情信息
        contractInfoDTO,

        // 批次列表数据
        logisticsBatchDTOS,
        collapseActivityKey,
        // 阶段列表数据
        transportSectionOBJ,
        // 阶段列表 原始数据
        transportSectionOBJOrigin: transportSectionOBJ,
      };
    },
    cacheSectionListReduce(state, { payload }) {
      return { ...state, transportSectionOBJ: payload };
    },
    cacheCollapseActivityKeyReduce(state, { payload }) {
      return { ...state, collapseActivityKey: payload };
    },
    addBatchListReduce(state, { payload }) {
      return { ...state, logisticsBatchDTOS: payload };
    },
  },
};
