import moment from 'moment';
import { cloneDeep, random } from 'lodash';
import {
  getTransportSectionAllDetail,
  saveTransportSection,
  getCarrierInfoList,
} from '../services';

export default {
  namespace: 'TransportSectionDetailModel',
  state: {
    carrayNameOptions: [],
    requestType: 'buyTransportSection',
    // 详情信息
    transportSectionDetail: {
      transportModeName: '',
      toFactory: 0,
      startingPlace: '',
      arrivalPlace: '',
    },

    // 承运商数据
    carrierTransportDTOS: [],
    // 手风琴当前展开的项---根据承运方id来设置
    collapseActivityKey: '',

    // 运载单位列表数据
    carrierTransportDetailOBJ: {},
    // 运载单位列表 原始数据
    carrierTransportDetailOBJOrigin: {},
  },
  effects: {
    // 获取承运商详情
    *getTransportSectionAllDetailEffect({ payload, callback }, { call, all, put }) {
      const [TransportSectionAllDetailRes, carrayNameListRes] = yield all([
        call(getTransportSectionAllDetail, payload),
        call(getCarrierInfoList, {}),
      ]);

      if (TransportSectionAllDetailRes && carrayNameListRes) {
        if (TransportSectionAllDetailRes.code === 1 && carrayNameListRes.code === 1) {
          yield put({
            type: 'getTransportSectionAllDetailReduce',
            payload: {
              ...TransportSectionAllDetailRes.object,
              requestType: payload.requestType,
              carrayNameList: carrayNameListRes.object,
            },
          });
        } else {
          const error1 =
            TransportSectionAllDetailRes.message ||
            carrayNameListRes.message ||
            'getTransportSectionAllDetail-error';
          callback('', 'error', error1);
        }
      } else if (callback) {
        callback('', 'error', 'getTransportSectionAllDetail-error');
      }
    },
    // 新增运载单位
    *addTransportDetailListEffect({ payload }, { put, select }) {
      const { id } = payload;
      const { carrierTransportDetailOBJ } = yield select(
        state => state.TransportSectionDetailModel,
      );

      const newData = cloneDeep(carrierTransportDetailOBJ);

      const currentCarrayList = newData[id] || [];
      const key = `${random(0, 9999)}_id`;
      currentCarrayList.push({
        key,
        id: key,
        // 运载单位编号
        trainNumber: '',
        // 司机
        driver: '',
        // 手机号
        contact: '',
        // 吨位
        cargoWeight: '',
        editable: true,
      });

      newData[id] = currentCarrayList;

      yield put({
        type: 'cacheTransportDetailListEffect',
        payload: newData,
      });
    },
    // 删除某一条运载单位
    *deleteTransportDetailListEffect({ payload }, { put, select }) {
      const { collapseActivityKey: id, id: transID } = payload;

      const { carrierTransportDetailOBJ } = yield select(
        state => state.TransportSectionDetailModel,
      );

      const newData = cloneDeep(carrierTransportDetailOBJ);

      const currentCarrayList = newData[id];

      const newList = currentCarrayList.filter(item => item.id !== transID);

      newData[id] = newList;

      yield put({
        type: 'cacheTransportDetailListEffect',
        payload: newData,
      });
    },

    // 存储改变后的----运载单位列表到store中
    *cacheTransportDetailListEffect({ payload }, { put, select }) {
      let result = payload;
      if (!result) {
        const { carrierTransportDetailOBJOrigin } = yield select(
          state => state.TransportSectionDetailModel,
        );
        result = cloneDeep(carrierTransportDetailOBJOrigin);
      }
      yield put({
        type: 'cacheTransportDetailListReduce',
        payload: result,
      });
    },
    *cacheCollapseActivityKeyEffect({ payload }, { put }) {
      yield put({
        type: 'cacheCollapseActivityKeyReduce',
        payload,
      });
    },

    // 新增承运商
    *addCarrayListEffect({ payload }, { put, select }) {
      const { carrierTransportDTOS, carrayNameOptions } = yield select(
        state => state.TransportSectionDetailModel,
      );

      const newData = cloneDeep(carrierTransportDTOS);

      newData.push({
        id: `${random(999, 9999)}_id`,
        carrierId: carrayNameOptions[0].value,

        // 单价
        unitPrice: '',
        // 运费
        carriage: '',
        // 出发时间
        departureTime: moment().format('YYYY-MM-DD HH:mm:ss'),
        // 预计到达时间
        estimateArrivalTime: moment().format('YYYY-MM-DD HH:mm:ss'),
      });

      yield put({
        type: 'cacheCarrayListReduce',
        payload: newData,
      });
    },

    // 确认保存 ----承运商列表(注:只保存新增的)
    *saveCarrayListEffect({ payload, callback }, { select, put, call }) {
      const {
        transportSectionDetail,
        carrierTransportDTOS,
        carrierTransportDetailOBJ,
        collapseActivityKey,
        requestType,
      } = yield select(state => state.TransportSectionDetailModel);

      const currentData = carrierTransportDetailOBJ[collapseActivityKey];
      if (!currentData || currentData.length === 0) {
        callback('', 'error', '暂无数据可保存');
      } else {
        const list = currentData.filter(item => !item.disabled);
        if (list.length === 0) {
          callback('', 'error', '暂无数据可保存');
          return;
        }
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        const newList = list.map((item, index) => {
          const { editable, key, id, ...rest } = item;
          return {
            ...rest,
          };
        });

        const currentcarrierTransportDTOS = carrierTransportDTOS.filter(
          item => `${item.id}` === `${collapseActivityKey}`,
        );

        const isNoData = newList.filter(item => !item.trainNumber || !item.cargoWeight);
        if (isNoData.length > 0) {
          callback('', 'error', '运载单位编号与吨位必填');
        } else {
          const params = {
            ...transportSectionDetail,
            carrierTransportDTOS: [
              {
                ...currentcarrierTransportDTOS[0],
                id: `${currentcarrierTransportDTOS[0].id}`.includes('id')
                  ? ''
                  : currentcarrierTransportDTOS[0].id,
                detailDTOS: newList,
              },
            ],

            requestType,
          };

          const result = yield call(saveTransportSection, params);

          if (result && result.code === 1) {
            callback('', 'ok', '保存承运商及运载单位成功');
          } else if (callback) {
            const error1 = (result && result.message) || 'saveTransportSection-error';
            callback('', 'error', error1);
          }
        }
      }
    },
  },
  reducers: {
    getTransportSectionAllDetailReduce(state, { payload }) {
      const {
        carrierTransportDTOS: oldcarrierTransportDTOS,
        requestType,
        carrayNameList,
        ...payloadRest
      } = payload;

      const newcarrayNameList = carrayNameList.map(item => ({
        value: `${item.carrierId}`,
        label: item.carrierName,
      }));

      // 详情信息
      const transportSectionDetail = { ...payloadRest };

      // 承运商数据
      const carrierTransportDTOS = [];
      // 手风琴当前展开的项---根据承运方id来设置
      let collapseActivityKey = '';

      // 运载单位列表数据
      const carrierTransportDetailOBJ = {};

      oldcarrierTransportDTOS.forEach(item => {
        const { detailDTOS, ...rest } = item;
        carrierTransportDTOS.push(rest);

        carrierTransportDetailOBJ[item.id] = item.detailDTOS.map(nextItem => ({
          ...nextItem,
          key: nextItem.id,
          editable: true,
          disabled: true,
        }));
      });

      if (carrierTransportDTOS.length > 0) {
        collapseActivityKey = `${carrierTransportDTOS[0].id}`;
      }

      return {
        ...state,
        carrayNameOptions: newcarrayNameList,
        requestType,
        // 详情信息
        transportSectionDetail,

        // 列表数据
        carrierTransportDTOS,
        collapseActivityKey,
        // 阶段列表数据
        carrierTransportDetailOBJ,
        // 阶段列表 原始数据
        carrierTransportDetailOBJOrigin: carrierTransportDetailOBJ,
      };
    },
    cacheTransportDetailListReduce(state, { payload }) {
      return { ...state, carrierTransportDetailOBJ: payload };
    },
    cacheCollapseActivityKeyReduce(state, { payload }) {
      return { ...state, collapseActivityKey: payload };
    },
    cacheCarrayListReduce(state, { payload }) {
      return { ...state, carrierTransportDTOS: payload };
    },
  },
};
