import { api_factory_prefix } from '@/constants/prefix';

export default {
  //电表管理接口
  configAmmeter: {
    //条件查询电表列表
    searchAmmeterList: {
      api: params =>
        `${api_factory_prefix}/configAmmeter/searchAmmeterList?${params}`,
      type: 'GET'
    },
    //保存电表信息 （新增）
    saveAmmeter: {
      api: () => `${api_factory_prefix}/configAmmeter/saveAmmeter`,
      type: 'POST'
    },
    //修改电表信息 （新增）
    updateAmmeter: {
      api: () => `${api_factory_prefix}/configAmmeter/updateAmmeter`,
      type: 'POST'
    },

    //获取电表详情
    getAmmeterById: {
      api: id =>
        `${api_factory_prefix}/configAmmeter/getAmmeterById?id=${id}`,
      type: 'GET'
    },
    //删除电表
    deleteAmmeterById: {
      api: id =>
        `${api_factory_prefix}/configAmmeter/deleteAmmeterById?id=${id}`,
      type: 'DELETE'
    }
  },
  //电价管理接口
  configElectricityPrice: {
    //查询电价列表
    searchElectricityPriceList: {
      api: (a) =>
        `${api_factory_prefix}/configElectricityPrice/searchElectricityPriceList?${a}`,
      type: 'GET'
    },

    //新增电价信息
    saveElectricityPrice: {
      api: () =>
        `${api_factory_prefix}/configElectricityPrice/saveElectricityPrice`,
      type: 'POST'
    },

    //编辑电价信息
    updateElectricityPrice: {
      api: () =>
        `${api_factory_prefix}/configElectricityPrice/updateElectricityPrice`,
      type: 'POST'
    },

    //查询电价详情
    getElectricityPriceById: {
      api: id =>
        `${api_factory_prefix}/configElectricityPrice/getElectricityPriceById?id=${id}`,
      type: 'GET'
    },

    //删除电价信息
    deleteElectricityPriceById: {
      api: id =>
        `${api_factory_prefix}/configElectricityPrice/deleteElectricityPriceById?id=${id}`,
      type: 'DELETE'
    }
  },

  //设备巡检接口
  configDeviceInspection: {
    //获取设备列表
    searchDeviceInspectionList: {
      api: params =>
        `${api_factory_prefix}/configDeviceInspection/searchDeviceInspectionList?${params}`,
      type: 'GET'
    },
    //保存设备巡检信息
    saveDeviceInspection: {
      api: params =>
        `${api_factory_prefix}/configDeviceInspection/saveDeviceInspection`,
      type: 'POST'
    },
    //修改设备巡检信息
    updateDeviceInspection: {
      api: params =>
        `${api_factory_prefix}/configDeviceInspection/updateDeviceInspection`,
      type: 'POST'
    },

    //删除设备巡检信息
    deleteDeviceInspectionById: {
      api: id =>
        `${api_factory_prefix}/configDeviceInspection/deleteDeviceInspectionById?id=${id}`,
      type: 'DELETE'
    },

    //获取设备巡检信息
    getDeviceInspectionById: {
      api: id =>
        `${api_factory_prefix}/configDeviceInspection/getDeviceInspectionById?id=${id}`,
      type: 'GET'
    }
  },

  //设备管理接口
  configDevice: {
    //模糊搜索设备下拉列表
    getDeviceListByName: {
      api: name =>
        `${api_factory_prefix}/configDevice/getDeviceListByName?deviceName=${name}`,
      type: 'GET'
    },

    //查询设备列表
    searchDeviceList: {
      api: params =>
        `${api_factory_prefix}/configDevice/searchDeviceList?${params}`,
      type: 'GET'
    },

    //保存设备信息
    saveDevice: {
      api: params => `${api_factory_prefix}/configDevice/saveDevice`,
      type: 'POST'
    },
    //修改设备信息
    updateDevice: {
      api: params =>
        `${api_factory_prefix}/configDevice/updateDevice`,
      type: 'POST'
    },
    //查询设备信息
    getDeviceById: {
      api: id =>
        `${api_factory_prefix}/configDevice/getDeviceById?id=${id}`,
      type: 'GET'
    },
    //删除设备信息
    deleteDeviceById: {
      api: id =>
        `${api_factory_prefix}/configDevice/deleteDeviceById?id=${id}`,
      type: 'DELETE'
    }
  },

  //称重设备管理接口
  configWeighDevice: {
    //获取称重设备列表
    searchWeighDeviceList: {
      api: params =>
        `${api_factory_prefix}/configWeighDevice/searchWeighDeviceList?${params}`,
      type: 'GET'
    },

    //保存称重设备信息
    saveWeighDevice: {
      api: params =>
        `${api_factory_prefix}/configWeighDevice/saveWeighDevice`,
      type: 'POST'
    },

    //修改称重设备信息
    updateWeighDevice: {
      api: params =>
        `${api_factory_prefix}/configWeighDevice/updateWeighDevice`,
      type: 'POST'
    },

    //获取称重设备信息
    getWeighDeviceById: {
      api: id =>
        `${api_factory_prefix}/configWeighDevice/getWeighDeviceById?id=${id}`,
      type: 'GET'
    },

    //删除称重设备信息
    deleteWeighDeviceById: {
      api: id =>
        `${api_factory_prefix}/configWeighDevice/deleteWeighDeviceById?id=${id}`,
      type: 'DELETE'
    }
  },

  //PLC设备管理
  configPlcDevice: {
    //查询PLC；列表
    searchPlcDeviceList: {
      api: params =>
        `${api_factory_prefix}/configPlcDevice/searchPlcDeviceList?${params}`,
      type: 'GET'
    },
    //保存PLC信息
    savePlcDevice: {
      api: params =>
        `${api_factory_prefix}/configPlcDevice/savePlcDevice`,
      type: 'POST'
    },
    //修改PLC信息
    updatePlcDevice: {
      api: params =>
        `${api_factory_prefix}/configPlcDevice/updatePlcDevice`,
      type: 'POST'
    },
    //查询PLC信息
    getPlcDeviceById: {
      api: id =>
        `${api_factory_prefix}/configPlcDevice/getPlcDeviceById?id=${id}`,
      type: 'GET'
    },
    //删除PLC信息
    deletePlcDeviceById: {
      api: id =>
        `${api_factory_prefix}/configPlcDevice/deletePlcDeviceById?id=${id}`,
      type: 'DELETE'
    },
    getFieldTypes: {
      api: () => `${api_factory_prefix}/plc/business/fieldTypes`,
      type: 'GET'
    }
  },

  //网关管理接口
  gateways: {
    //获取网关列表
    getGateList: {
      api: params =>
        `${api_factory_prefix}/plc/business/gateways?${params}`,
      type: 'GET'
    },

    //保存网关配置
    saveGateway: {
      api: params => `${api_factory_prefix}/plc/business/gateway`,
      type: 'POST'
    },

    //删除网关配置
    deleteGateway: {
      api: id => `${api_factory_prefix}/plc/business/gateway/${id}`,
      type: 'DELETE'
    }
  }
}
