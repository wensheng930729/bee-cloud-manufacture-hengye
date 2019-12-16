import request from '@/utils/request'
import apis from './api'

export function searchAmmeterList(params) {
  return request(apis.configAmmeter.searchAmmeterList.api(params), {
    method: apis.configAmmeter.searchAmmeterList.type
  })
}
export function searchDeviceList(params) {
  return request(apis.configDevice.searchDeviceList.api(params), {
    method: apis.configDevice.searchDeviceList.type
  })
}
export function searchPlcDeviceList(params) {
  return request(apis.configPlcDevice.searchPlcDeviceList.api(params), {
    method: apis.configPlcDevice.searchPlcDeviceList.type
  })
}
export function getGateList(params) {
  return request(apis.gateways.getGateList.api(params), {
    method: apis.gateways.getGateList.type
  })
}
export function deleteGateway(params) {
  return request(apis.gateways.deleteGateway.api(params), {
    method: apis.gateways.deleteGateway.type
  })
}
export function saveGateway(params) {
  return request(apis.gateways.saveGateway.api(params), {
    method: apis.gateways.saveGateway.type,
    body: params
  })
}
export function savePlcDevice(params) {
  return request(apis.configPlcDevice.savePlcDevice.api(), {
    method: apis.configPlcDevice.savePlcDevice.type,
    body: params
  })
}
export function updatePlcDevice(params) {
  return request(apis.configPlcDevice.updatePlcDevice.api(), {
    method: apis.configPlcDevice.updatePlcDevice.type,
    body: params
  })
}
export function getPlcDeviceById(params) {
  return request(apis.configPlcDevice.getPlcDeviceById.api(params), {
    method: apis.configPlcDevice.getPlcDeviceById.type
  })
}
export function deletePlcDeviceById(params) {
  return request(apis.configPlcDevice.deletePlcDeviceById.api(params), {
    method: apis.configPlcDevice.deletePlcDeviceById.type
  })
}

export function getFieldTypes() {
  return request(apis.configPlcDevice.getFieldTypes.api(), {
    method: apis.configPlcDevice.getFieldTypes.type
  })
}

export function searchWeighDeviceList(params) {
  return request(apis.configWeighDevice.searchWeighDeviceList.api(params), {
    method: apis.configWeighDevice.searchWeighDeviceList.type
  })
}
export function getWeighDeviceById(params) {
  return request(apis.configWeighDevice.getWeighDeviceById.api(params), {
    method: apis.configWeighDevice.getWeighDeviceById.type
  })
}
export function deleteWeighDeviceById(params) {
  return request(apis.configWeighDevice.deleteWeighDeviceById.api(params), {
    method: apis.configWeighDevice.deleteWeighDeviceById.type
  })
}
export function saveWeighDevice(params) {
  return request(apis.configWeighDevice.saveWeighDevice.api(), {
    method: apis.configWeighDevice.saveWeighDevice.type,
    body: params
  })
}
export function updateWeighDevice(params) {
  return request(apis.configWeighDevice.updateWeighDevice.api(), {
    method: apis.configWeighDevice.updateWeighDevice.type,
    body: params
  })
}
export function saveDevice(params) {
  return request(apis.configDevice.saveDevice.api(), {
    method: apis.configDevice.saveDevice.type,
    body: params
  })
}
export function updateDevice(params) {
  return request(apis.configDevice.updateDevice.api(), {
    method: apis.configDevice.updateDevice.type,
    body: params
  })
}
export function getDeviceById(params) {
  return request(apis.configDevice.getDeviceById.api(params), {
    method: apis.configDevice.getDeviceById.type
  })
}
export function configDeleteDeviceById(params) {
  return request(apis.configDevice.deleteDeviceById.api(params), {
    method: apis.configDevice.deleteDeviceById.type
  })
}
export function searchDeviceInspectionList(params) {
  return request(
    apis.configDeviceInspection.searchDeviceInspectionList.api(params),
    {
      method: apis.configDeviceInspection.searchDeviceInspectionList.type
    }
  )
}
export function deleteDeviceInspectionById(params) {
  return request(
    apis.configDeviceInspection.deleteDeviceInspectionById.api(params),
    {
      method: apis.configDeviceInspection.deleteDeviceInspectionById.type
    }
  )
}
export function getDeviceInspectionById(params) {
  return request(
    apis.configDeviceInspection.getDeviceInspectionById.api(params),
    {
      method: apis.configDeviceInspection.getDeviceInspectionById.type
    }
  )
}
export function saveDeviceInspection(params) {
  return request(apis.configDeviceInspection.saveDeviceInspection.api(), {
    method: apis.configDeviceInspection.saveDeviceInspection.type,
    body: params
  })
}
export function updateDeviceInspection(params) {
  return request(apis.configDeviceInspection.updateDeviceInspection.api(), {
    method: apis.configDeviceInspection.updateDeviceInspection.type,
    body: params
  })
}

export function getDeviceListByName(params) {
  return request(apis.configDevice.getDeviceListByName.api(params), {
    method: apis.configDevice.getDeviceListByName.type
  })
}

export function searchElectricityPriceList(params) {
  return request(
    apis.configElectricityPrice.searchElectricityPriceList.api(params),
    {
      method: apis.configElectricityPrice.searchElectricityPriceList.type
    }
  )
}
export function getElectricityPriceById(params) {
  return request(
    apis.configElectricityPrice.getElectricityPriceById.api(params),
    {
      method: apis.configElectricityPrice.getElectricityPriceById.type
    }
  )
}
export function deleteElectricityPriceById(params) {
  return request(
    apis.configElectricityPrice.deleteElectricityPriceById.api(params),
    {
      method: apis.configElectricityPrice.deleteElectricityPriceById.type
    }
  )
}

export function saveElectricityPrice(params) {
  return request(apis.configElectricityPrice.saveElectricityPrice.api(), {
    method: apis.configElectricityPrice.saveElectricityPrice.type,
    body: params
  })
}
export function updateElectricityPrice(params) {
  return request(apis.configElectricityPrice.updateElectricityPrice.api(), {
    method: apis.configElectricityPrice.updateElectricityPrice.type,
    body: params
  })
}

export function getAmmeterById(params) {
  return request(apis.configAmmeter.getAmmeterById.api(params), {
    method: apis.configAmmeter.getAmmeterById.type
  })
}
export function deleteAmmeterById(params) {
  return request(apis.configAmmeter.deleteAmmeterById.api(params), {
    method: apis.configAmmeter.deleteAmmeterById.type
  })
}

export function saveAmmeter(params) {
  return request(apis.configAmmeter.saveAmmeter.api(), {
    method: apis.configAmmeter.saveAmmeter.type,
    body: params
  })
}
export function updateAmmeter(params) {
  return request(apis.configAmmeter.updateAmmeter.api(), {
    method: apis.configAmmeter.updateAmmeter.type,
    body: params
  })
}
