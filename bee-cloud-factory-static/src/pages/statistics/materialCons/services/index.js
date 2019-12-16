import request from '@/utils/request'
import apis from './api'

export function searchMaterialsConsumptionList(params) {
  return request(apis.searchMaterialsConsumptionList.api(params), {
    method: apis.searchMaterialsConsumptionList.type
  })
}

export function getProductListByCategory(params) {
  return request(apis.getProductListByCategory.api(params), {
    method: apis.getProductListByCategory.type
  })
}
export function deleteMaterialsConsumptionById(params) {
  return request(apis.deleteMaterialsConsumptionById.api(params), {
    method: apis.deleteMaterialsConsumptionById.type
  })
}
export function getMaterialsConsumptionById(params) {
  return request(apis.getMaterialsConsumptionById.api(params), {
    method: apis.getMaterialsConsumptionById.type
  })
}

export function saveMaterialsConsumption(params) {
  return request(apis.saveMaterialsConsumption.api(), {
    method: apis.saveMaterialsConsumption.type,
    body: params
  })
}
export function updateMaterialsConsumption(params) {
  return request(apis.updateMaterialsConsumption.api(), {
    method: apis.updateMaterialsConsumption.type,
    body: params
  })
}
