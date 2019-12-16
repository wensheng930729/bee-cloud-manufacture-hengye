
import { api_factory_prefix } from '@/constants/prefix';
export default {
  getRecoveryRate: {//条件查询生产数据回收率
    api: () => `${api_factory_prefix}/proStatisticController/recoveryRate`,
    type: 'POST'
  },
  getPowerFactor: {//查询矿热炉实时功率因素
    api: () => `${api_factory_prefix}/proStatisticController/powerFactor`,
    type: 'GET'
  },
  getProductSpec: {//条件查询矿热炉产出质量
    api: () => `${api_factory_prefix}/proStatisticController/productSpec`,
    type: 'POST'
  },
  getProduction: {//条件查询矿热炉产量
    api: () => `${api_factory_prefix}/proStatisticController/production`,
    type: 'POST'
  },

  getPowerConsume: {//条件查询矿热炉电力消耗
    api: () => `${api_factory_prefix}/proStatisticController/powerConsume`,
    type: 'POST'
  },  
  getPowerTonConsume: {//条件查询矿热炉吨电耗
    api: () => `${api_factory_prefix}/proStatisticController/powerTonConsume`,
    type: 'POST'
  },
  
  getMaterialConsume: {//条件查询矿热炉原料消耗
    api: () => `${api_factory_prefix}/proStatisticController/materialConsume`,
    type: 'POST'
  },  
  getMaterialTonConsume: {//条件查询矿热炉主料（辅料）吨耗
    api: () => `${api_factory_prefix}/proStatisticController/materialTonConsume`,
    type: 'POST'
  },
  getDeviceInspection: {//统计设备检修状况
    api: () => `${api_factory_prefix}/proStatisticController/deviceInspection`,
    type: 'GET'
  }
}