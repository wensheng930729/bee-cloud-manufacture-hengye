import moment from 'moment';
import { cloneDeep } from 'lodash';
import { reportType as REPORT_TYPE, tabPages } from '@/consts/reportForm';
import { getEnableReportFormsList } from '@/services/index';
import {
  getExistingDeatilsReportForm,
  getBuySendStorageReportForm,
  getRawMaterialReportForm,
  getProductWarehouseReportForm,
  getProductOutStorageReportForm,
  getQualityTestReportForm,
  getReportFormFields,
  getProductCategories,
  getProducts,
  getRepositoryListByType,
  getFurnaces,
  listConfigCodeByType,
  getSaleLogisticsReportForm,
  getBuyLogisticsReportForm,
  getBuyReportForm,
  getSaleReportForm,
  getPassRateReportForm,
} from '../services';

import { dealListData, dealListParams } from '../utils';

const reportTypeInit = REPORT_TYPE.existingDeatil.key;
const businessTypeInit = REPORT_TYPE.existingDeatil.businessType;

export default {
  namespace: 'ReportFormCommonModel',
  state: {
    parentTabs: [],

    // 产品
    productsOptions: [],
    // 仓库
    storageOptions: [],
    // 产品类别
    productsCateOptions: [],
    // 炉号
    furnaceOptions: [],
    // 班次
    shiftCodeOptions: [],

    // 列名接口参数
    columnsParams: {
      reportType: reportTypeInit,
      businessType: businessTypeInit,
      // 产品id
      productId: '',
    },
    // 根据reportType名称生成 列名对象
    columnsObj: {
      1: [],
      2: [],
      3: [],
      4: [],
      5: [],
      6: [],
      7: [],
      8: [],
      9: [],
      10: [],
      11: [],
      12: [],
      13: [],
      14: [],
      15: [],
    },

    listSearchParams: {
      reportType: reportTypeInit,
      currentPage: 1,
      orderStage: '',
      pageSize: 10,
      searchCount: true,
      dateRange: {
        startTime: moment()
          .subtract(30, 'days')
          .format('YYYY-MM-DD'),
        endTime: moment().format('YYYY-MM-DD'),
      },
      dayTime: moment().format('YYYY-MM-DD'),
      // 产品id
      productId: '',
      // 仓库id
      storageId: '',
      // 样品编号
      sampleCode: '',
      // 产品类别，仅进出厂有
      categoryId: '0',
      year: '2019',
    },

    // 根据reportType生成 表格数据 对象  {list:[],pagination:{}}
    dataObj: {},
  },
  effects: {
    // 获取tab页
    *getTabPagesEffect({ payload, callback }, { call, put }) {
      const { type, columnsParams } = payload;
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
        // 获取查询表单中的 下拉框数据
        yield put({
          type: 'fetchInitOptionsEffect',
          payload: { columnsParams },
          callback,
        });
      } else {
        callback('', 'error', result.message);
      }
    },
    // 获取产品列表,产品类别列表,仓库列表
    *fetchInitOptionsEffect({ payload, callback }, { call, all, put }) {
      const [storageRes, productRes, productCategoriesRes] = yield all([
        call(getRepositoryListByType, { rq: { types: [] } }),
        call(getProducts, {}),
        call(getProductCategories, {}),
      ]);

      if (storageRes && productRes && productCategoriesRes) {
        if (storageRes.code === 1 && productRes.code === 1 && productCategoriesRes.code === 1) {
          const allItem = { value: '0', label: '全部' };
          const storageOptions = storageRes.object.map(item => ({
            value: `${item.id}`,
            label: item.name,
            origin: item,
          }));
          storageOptions.unshift(allItem);

          const productsOptions = productRes.object.map(item => ({
            value: `${item.productId}`,
            label: item.productName,
            origin: item,
          }));

          const productsCateOptions = productCategoriesRes.object.map(item => ({
            value: `${item.categoryId}`,
            label: item.categoryName,
            origin: item,
          }));
          productsCateOptions.unshift(allItem);

          const result = {
            storageOptions,
            productsOptions,
            productsCateOptions,
          };
          yield put({
            type: 'fetchInitOptionsReduce',
            payload: { ...result },
          });
          // 获取列名

          if (payload && payload.columnsParams) {
            const productId = productsOptions[0].value;
            const storageId = storageOptions[0].value;
            const categoryId = productsCateOptions[0].value;
            // furnaceId: furnaceOptions[0].value,
            // shiftCode: shiftCodeOptions[0].value

            const newParams = { ...payload.columnsParams, productId };
            yield put({
              type: 'fetchColumnsNameEffect',
              payload: {
                columnsParams: newParams,
                listSearchParams: {
                  productId,
                  storageId,
                  categoryId,
                  reportType: newParams.reportType,
                },
              },
              callback,
            });
          }

          callback(result, 'ok', '');
        } else {
          const error1 =
            storageRes.message ||
            productRes.message ||
            productCategoriesRes.message ||
            'fetchInit-error';
          callback('', 'error', error1);
        }
      } else {
        callback('', 'error', 'fetchInit-error');
      }
    },

    // 获取列名
    *fetchColumnsNameEffect({ payload, callback }, { call, put, select }) {
      if (payload && JSON.stringify(payload) !== '{}') {
        const { columnsParams = {}, listSearchParams = {} } = payload;

        const {
          columnsObj,
          columnsParams: oldcolumnsParams,
          listSearchParams: oldlistSearchParams,
        } = yield select(state => state.ReportFormCommonModel);

        const newColumnsParams = { ...oldcolumnsParams, ...columnsParams };

        const result = yield call(getReportFormFields, newColumnsParams);
        /*
      defaultSortOrder: 'descend',
      sorter: (a, b) => a.age - b.age,
                  // if (item.fields === "signDate") {
            //   return {
            //     title: item.fieldsName,
            //     dataIndex: item.fields,
            //     defaultSortOrder: "descend",
            //     // sorter: (a, b) => a.signDate - b.signDate
            //   };
            // }
  */
        if (result && result.code === 1) {
          const fieldsFixedColumns = result.object.fieldsFixed.map(item => ({
            title: item.title,
            dataIndex: item.dataIndex || item.title,
          }));
          if (Array.isArray(result.object.assayItem) && result.object.assayItem.length > 0) {
            const assayItemColumns = result.object.assayItem.map(item => ({
              title: item.item,
              dataIndex: item.item,
            }));
            fieldsFixedColumns.push(...assayItemColumns);
          }

          columnsObj[newColumnsParams.reportType] = fieldsFixedColumns;

          yield put({
            type: 'fetchColumnsNameReduce',
            payload: {
              columnsParams: newColumnsParams,
              columnsObj,
            },
          });

          const newlistSearchParams = {
            ...oldlistSearchParams,
            ...listSearchParams,
          };

          yield put({
            type: 'getListEffect',
            payload: { ...newlistSearchParams },
            callback,
          });

          if (callback) {
            callback('', 'ok', '');
          }
        } else if (callback) {
          const error1 = (result && result.message) || 'fetchInit-error';
          callback('', 'error', error1);
        }
      }
    },

    // 获取表格数据
    *getListEffect({ payload, callback }, { call, put, select }) {
      let serviceAPI = null;
      switch (payload.reportType) {
        // 获取 现存明细表数据
        case REPORT_TYPE.existingDeatil.key:
          serviceAPI = getExistingDeatilsReportForm;
          break;
        // 获取 采购入库表数据
        case REPORT_TYPE.buySendStorage.key:
          serviceAPI = getBuySendStorageReportForm;
          break;
        // 获取 原料日报表数据
        case REPORT_TYPE.productOutStorage.key:
          serviceAPI = getProductOutStorageReportForm;
          break;
        // 获取 产成品入库表数据
        case REPORT_TYPE.rawMaterial.key:
          serviceAPI = getRawMaterialReportForm;
          break;
        // 获取 成品出库表数据
        case REPORT_TYPE.productWarehouse.key:
          serviceAPI = getProductWarehouseReportForm;
          break;

        // 获取 质检表数据
        case REPORT_TYPE.productQualiInspect.key:
        case REPORT_TYPE.entryExitQualiInspect.key:
          serviceAPI = getQualityTestReportForm;
          break;

        // 获取 采购运输台账
        case REPORT_TYPE.logisticsBuy.key:
          serviceAPI = getBuyLogisticsReportForm;
          break;
        // 获取 销售运输台账
        case REPORT_TYPE.logisticsSale.key:
          serviceAPI = getSaleLogisticsReportForm;
          break;
        // 获取 采购报表
        case REPORT_TYPE.purchase.key:
          serviceAPI = getBuyReportForm;
          break;
        // 获取 销售报表
        case REPORT_TYPE.sales.key:
          serviceAPI = getSaleReportForm;
          break;
        // 获取 合格率报表
        case REPORT_TYPE.passRate.key:
          serviceAPI = getPassRateReportForm;
          break;
        default:
          break;
      }

      const { listSearchParams: oldlistSearchParams } = yield select(
        state => state.ReportFormCommonModel,
      );

      const { requestParams, listSearchParams: newlistSearchParams } = dealListParams({
        preParams: { ...oldlistSearchParams },
        curParams: payload,
      });

      if (serviceAPI) {
        const result = yield call(serviceAPI, requestParams);
        // 表格数据
        const { dataObj } = yield select(state => state.ReportFormCommonModel);
        // 表格列名
        const { columnsObj } = yield select(state => state.ReportFormCommonModel);

        const dealListDataParams = {
          originData: result,
          columns: columnsObj[payload.reportType],
        };
        const newData = dealListData(dealListDataParams);
        const newdataObj = cloneDeep(dataObj);

        newdataObj[payload.reportType] = newData;

        if (typeof newData !== 'string') {
          yield put({
            type: 'getListReduce',
            payload: {
              listSearchParams: { ...newlistSearchParams },
              dataObj: newdataObj,
            },
          });
        } else {
          callback('', 'error', newData);
          newdataObj[payload.reportType] = { list: [], pagination: {} };
          yield put({
            type: 'getListReduce',
            payload: {
              listSearchParams: { ...newlistSearchParams },
              dataObj: newdataObj,
            },
          });
        }
      }
    },
    // 获取炉号与班次 产品
    *fetchFurnacesShiftCodeEffect({ payload, callback }, { call, put, all }) {
      const [furnaceRes, shiftCodeRes, productRes] = yield all([
        call(getFurnaces, {}),
        call(listConfigCodeByType, { type: 'shifts' }),
        call(getProducts, {}),
      ]);

      if (furnaceRes && shiftCodeRes && productRes) {
        if (furnaceRes.code === 1 && shiftCodeRes.code === 1 && productRes.code === 1) {
          const allItem = { value: '0', label: '全部' };

          const furnaceOptions = furnaceRes.object.map(item => ({
            value: `${item.furnacesId}`,
            label: item.furnacesName,
            origin: item,
          }));
          furnaceOptions.unshift(allItem);

          const shiftCodeOptions = shiftCodeRes.object.map(item => ({
            value: `${item.code}`,
            label: item.value,
            origin: item,
          }));
          shiftCodeOptions.unshift(allItem);

          const productsOptions = productRes.object.map(item => ({
            value: `${item.productId}`,
            label: item.productName,
            origin: item,
          }));

          const result = {
            furnaceOptions,
            shiftCodeOptions,
            productsOptions,
          };
          yield put({
            type: 'fetchFurnacesShiftCodeReduce',
            payload: { ...result },
          });

          if (payload && payload.columnsParams) {
            const productId = productsOptions[0].value;
            const newParams = { ...payload.columnsParams, productId };
            yield put({
              type: 'fetchColumnsNameEffect',
              payload: newParams,
              callback,
            });
          }

          callback(result, 'ok', '');
        } else {
          const error1 =
            furnaceRes.message || shiftCodeRes.message || 'fetchFurnacesShiftCodeEffect-error';
          callback('', 'error', error1);
        }
      } else {
        callback('', 'error', 'fetchFurnacesShiftCodeEffect-error');
      }
    },
    *cacheSearchParamsEffect({ payload }, { put }) {
      yield put({
        type: 'cacheSearchParamsReduce',
        payload,
      });
    },
  },

  reducers: {
    getTabPagesReduce(state, { payload }) {
      return { ...state, parentTabs: payload };
    },
    fetchInitOptionsReduce(
      state,
      {
        payload: { storageOptions, productsOptions, productsCateOptions },
      },
    ) {
      return { ...state, storageOptions, productsOptions, productsCateOptions };
    },
    fetchColumnsNameReduce(
      state,
      {
        payload: { columnsParams, columnsObj },
      },
    ) {
      return { ...state, columnsParams, columnsObj };
    },
    getListReduce(
      state,
      {
        payload: { listSearchParams, dataObj },
      },
    ) {
      return { ...state, listSearchParams, dataObj };
    },

    cacheSearchParamsReduce(state, { payload }) {
      return {
        ...state,
        listSearchParams: { ...state, ...payload },
      };
    },
    fetchFurnacesShiftCodeReduce(
      state,
      {
        payload: { furnaceOptions, productsOptions, shiftCodeOptions },
      },
    ) {
      return { ...state, furnaceOptions, shiftCodeOptions, productsOptions };
    },
  },
};
