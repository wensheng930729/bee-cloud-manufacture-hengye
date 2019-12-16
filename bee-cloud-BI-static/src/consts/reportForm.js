// 常量 不能随意更改----------------------------

export const MENU_TYPE = {
  // 库存
  STOCK: 'stock',
  QUALITY_TEST: 'quality_test',
  // 质检
  PURCHASE: 'purchase',
  sale: 'sale',
  // 产量分析
  THROUGHPUT_ANALYSIS: 'throughput_analysis',
  // 合格率
  PASS_RATE: 'pass_rate',
  // 产量，消耗分析
  OUTPUT_AND_CONSUMPTION_ANALYSIS: 'output_and_consumption_analysis',
  // 物流
  LOGISTICS: 'logistics',
};

export const businessType = {
  1: { name: '采购', key: 1 },
  2: { name: '销售', key: 2 },
  3: { name: '生产', key: 3 },
  4: { name: '进出厂【采购+销售】', key: 4 },
};

export const businessTypeOptions = [{ value: '1', label: '采购' }, { value: '2', label: '销售' }];

export const reportType = {
  existingDeatil: {
    cName: '现存明细表',
    key: '1',
    businessType: '1',
  },
  buySendStorage: {
    cName: '采购入库表',
    key: '2',
    businessType: '1',
  },
  rawMaterial: {
    cName: '原料日报表',
    key: '3',
  },
  productWarehouse: {
    cName: '产成品入库',
    key: '4',
    businessType: '3',
  },

  productOutStorage: {
    cName: '成品出库',
    key: '5',
    businessType: '4',
  },
  productQualiInspect: {
    cName: '生产样质检报表',
    key: '6',
    businessType: 3,
    listType: 1,
    effectName: 'getQualityTestEffect',
  },
  entryExitQualiInspect: {
    cName: '进出厂质检报表',
    key: '7',
    businessType: 4,
    listType: 2,
    effectName: 'getQualityTestEffect',
  },
  purchase: {
    cName: '采购',
    key: '8',
    effectName: 'getListEffect',
    businessType: 1,
  },
  sales: {
    cName: '销售',
    key: '9',
    effectName: 'getListEffect',
    businessType: 2,
  },
  yieldAnalysis: { cName: '产量分析', key: '10', effectName: 'getListEffect' },
  passRate: {
    cName: '合格率',
    key: '11',
    effectName: 'getListEffect',
    businessType: 3,
  },
  productConsumptAnaly: {
    cName: '产量消耗分析',
    key: '12',
    effectName: 'getListEffect',
  },

  productionStatistics: {
    key: 'productionStatistics',
    cName: '产量统计报表',
    listType: '1',
    effectName: 'getListEffect',
  },
  consumptionAnalysis: {
    key: 'consumptionAnalysis',
    cName: '消耗分析报表',
    listType: '2',
    effectName: 'getListEffect',
  },

  logisticsBuy: {
    cName: '采购运输台账',
    key: '13',
    effectName: 'getListEffect',
    businessType: '1',
  },
  logisticsSale: {
    cName: '销售运输台账',
    key: '14',
    effectName: 'getListEffect',
    businessType: '2',
  },
};

export const tabPages = {
  // 库存报表
  300001: { ...reportType.existingDeatil },
  300002: { ...reportType.buySendStorage },
  300003: { ...reportType.rawMaterial },
  300004: { ...reportType.productWarehouse },
  300005: { ...reportType.productOutStorage },
  // 质检报表  ,
  300021: { ...reportType.productQualiInspect },
  300022: { ...reportType.entryExitQualiInspect },

  300061: { ...reportType.purchase },
  300041: { ...reportType.sales },

  // 生产报表
  300081: { ...reportType.yieldAnalysis },
  300101: { ...reportType.passRate },
  300121: { ...reportType.productionStatistics },
  300122: { ...reportType.consumptionAnalysis },
  // 物流
  300141: { ...reportType.logisticsSale },
  300142: { ...reportType.logisticsBuy },
};

export const productTypeOptions = [{ value: '1', label: '生产' }, { value: '2', label: '进出厂' }];
