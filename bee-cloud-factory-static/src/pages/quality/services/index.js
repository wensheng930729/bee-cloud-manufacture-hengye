import request from '@/utils/request'
import apis from './api'

export function searchTestAttributeList(params) {
  return request(apis.searchTestAttributeList.api(params), {
    method: apis.searchTestAttributeList.type
  })
}
export function deleteTestAttributeById(params) {
  return request(apis.deleteTestAttributeById.api(params), {
    method: apis.deleteTestAttributeById.type
  })
}

export function saveTestAttribute(params) {
  return request(apis.saveTestAttribute.api(), {
    method: apis.saveTestAttribute.type,
    body: params
  })
}
export function updateTestAttribute(params) {
  return request(apis.updateTestAttribute.api(), {
    method: apis.updateTestAttribute.type,
    body: params
  })
}
