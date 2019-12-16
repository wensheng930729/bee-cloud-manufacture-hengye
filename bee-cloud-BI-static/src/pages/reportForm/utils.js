import { uniqBy } from 'lodash';
import { reportType as REPORT_TYPE } from '@/consts/reportForm';

// 处理报表接口 参数
export const dealListParams = obj => {
  const { preParams, curParams } = obj;

  const newParams = { ...preParams, ...curParams };

  const {
    dateRange,
    dayTime,
    storageId,
    productId,
    categoryId,

    reportType,

    currentPage,
    orderStage,
    pageSize,
    searchCount,
    ...rest
  } = newParams;

  const startTime = dateRange && dateRange.startTime ? dateRange.startTime : null;
  const endTime = dateRange && dateRange.endTime ? dateRange.endTime : null;

  if (!reportType) {
    throw new Error('reportType不存在');
  }

  const commonParams = {
    pageSize,
    currentPage,
    orderStage,
    searchCount,
  };

  let result = {
    startTime,
    endTime,
    storageId,
    productId,
  };

  switch (reportType) {
    case REPORT_TYPE.existingDeatil.key:
      break;
    case REPORT_TYPE.buySendStorage.key:
      break;
    case REPORT_TYPE.rawMaterial.key:
      result = { productId, dayTime };
      break;
    case REPORT_TYPE.productWarehouse.key:
      break;
    case REPORT_TYPE.productOutStorage.key:
      break;

    case REPORT_TYPE.productQualiInspect.key:
      result = {
        startTime,
        endTime,
        productId,
        sampleCode: newParams.sampleCode,
        type: REPORT_TYPE.productQualiInspect.listType,
      };
      break;
    case REPORT_TYPE.entryExitQualiInspect.key:
      result = {
        startTime,
        endTime,
        productId,
        sampleCode: newParams.sampleCode,
        categoryId: newParams.categoryId,
        type: REPORT_TYPE.entryExitQualiInspect.listType,
      };
      break;

    case REPORT_TYPE.purchase.key:
      result = {
        startTime,
        endTime,
        productId,
        // 合同编号
        contractNum: newParams.contractNum,
        // 采购商
        supplierName: newParams.supplierName,
      };
      break;
    case REPORT_TYPE.sales.key:
      result = {
        startTime,
        endTime,
        productId,
        // 合同编号
        contractNum: newParams.contractNum,
        // 采购商
        customerName: newParams.customerName,
      };
      break;
    case REPORT_TYPE.yieldAnalysis.key:
      result = {
        startTime,
        endTime,
        productId: newParams.productId,
        // 生产编号
        productionNo: newParams.productionNo,
        // 炉号
        furnaceId: newParams.furnaceId,
        // 班次id
        shiftCode: newParams.shiftCode,
      };
      break;
    case REPORT_TYPE.passRate.key:
      result = {
        productId,
        year: newParams.year,
      };
      break;
    case REPORT_TYPE.productionStatistics.key:
      result = {
        startTime,
        endTime,
        productId,
        furnaceId: newParams.furnaceId,
        type: REPORT_TYPE.productionStatistics.listType,
      };
      break;
    case REPORT_TYPE.consumptionAnalysis.key:
      result = {
        startTime,
        endTime,
        productId,
        furnaceId: newParams.furnaceId,
        type: REPORT_TYPE.consumptionAnalysis.listType,
      };
      break;
    case REPORT_TYPE.logisticsBuy.key:
      result = {
        startTime,
        endTime,
        // 客户名
        supplierName: newParams.supplierName,
        // 产品名
        productName: newParams.productName,
      };
      break;
    case REPORT_TYPE.logisticsSale.key:
      result = {
        startTime,
        endTime,
        // 客户名
        customerName: newParams.customerName,
        productName: newParams.productName,
      };

      break;

    default:
      break;
  }
  return { requestParams: { ...result, ...commonParams }, listSearchParams: { ...newParams } };
};

export const dealReaptList = data => {
  const newList = data.map((item, index) => {
    const newItem = { ...item };
    newItem.key = item.id || `${index}`;
    if (Array.isArray(item.items) && item.items.length > 0) {
      item.items.forEach(childItem => {
        Object.keys(childItem).forEach(childItemObjItem => {
          newItem[childItemObjItem] = childItem[childItemObjItem];
        });
      });
    }
    return newItem;
  });

  const result = uniqBy(newList, 'key');
  return result;
};

export const dealListData = obj => {
  const { originData: result, columns } = obj;

  if (result && result.code === 1 && result.object) {
    const { object, page } = result;
    const pageSize = page && page.pageSize ? page.pageSize : 10;

    const pagination =
      (page && {
        total: page.totalRecords,
        pageSize: page.pageSize,
        current: page.currentPage,
      }) ||
      false;

    const bool0 = Array.isArray(object) && object.length === 0;
    const bool1 = JSON.stringify(object) === '{}';

    const bool4 = columns.length === 0;
    // 如果没有数据
    if (bool0 || bool1 || bool4) {
      return {
        list: [],
        pagination: {
          current: 1,
          pageSize: 10,
          total: 0,
        },
      };
    }
    // 如果object是数组 且有数据
    if (Array.isArray(object) && object.length > 0) {
      const newList = dealReaptList(object);

      return { list: newList, pagination };
    }
    if (Array.isArray(object.data) && object.data.length === 0) {
      return {
        list: [],
        pagination: {
          current: 1,
          pageSize: 10,
          total: 0,
        },
      };
    }

    // 如果object.data是数组 且有数据
    if (Array.isArray(object.data) && object.data.length > 0) {
      // items
      const list = dealReaptList(object.data);

      // 是否有总计 数据
      if (object.dataSum && Object.keys(object.dataSum).length > 0) {
        const { dataSum } = object;

        const totalObj = {};

        const firstColumName = columns[0].dataIndex;

        totalObj[firstColumName] = '总计';

        Object.keys(dataSum).forEach(nextItem => {
          totalObj[nextItem] = dataSum[nextItem];
        });

        list.splice(pageSize - 1, 0, {
          ...totalObj,
          key: 'dataSum',
        });
        return { list, pagination };
      }
      // 是否有平均数据
      if (object.dataAvg && Object.keys(object.dataAvg).length > 0) {
        const { dataAvg } = object;

        const totalObj = {};

        const firstColumName = columns[0].fields;

        totalObj[firstColumName] = '平均数';

        Object.keys(dataAvg).forEach(nextItem => {
          totalObj[nextItem] = dataAvg[nextItem];
        });

        list.splice(pageSize - 1, 0, {
          ...totalObj,
          key: 'dataAvg',
        });
        return { list, pagination };
      }
      return { list, pagination };
    }
    // 如果object.dataList 且有数据
    if (Array.isArray(object.dataList) && object.dataList.length > 0) {
    }
  } else {
    const error1 = (result && result.message) || 'dealListData-error';
    return error1;
  }
};

// -----处理 生产 报表列表数据
export const deaProducelListData = obj => {
  const { originData: result } = obj;

  if (result && result.code === 1 && result.object) {
    const {
      object,
      object: { dataList, fields },
      page,
    } = result;

    const pagination =
      (page && {
        total: page.totalRecords,
        pageSize: page.pageSize,
        current: page.currentPage,
      }) ||
      false;

    const columns = fields.filter(item => item.title);

    const bool0 = !dataList || (Array.isArray(dataList) && dataList.length === 0);
    // 如果没有数据
    if (bool0) {
      return {
        data: {
          list: [],
          pagination,
        },
        columns,
      };
    }

    // 如果object.data是数组 且有数据
    if (Array.isArray(object.dataList) && object.dataList.length > 0) {
      // shiftPercent1
      const list = object.dataList.map((item, index) => {
        const newItem = { ...item };
        newItem.key === item.id || `${index}`;

        Object.keys(newItem).forEach(nextItem => {
          if (`${nextItem}`.includes('shiftPercent') || `${nextItem}`.includes('furnacePercent')) {
            newItem[nextItem] = `${newItem[nextItem]}%`;
          }
        });
        return newItem;
      });

      return {
        data: { list, pagination },
        columns,
      };
    }
  } else {
    const error1 = (result && result.message) || 'dealListData-error';
    return error1;
  }
};
