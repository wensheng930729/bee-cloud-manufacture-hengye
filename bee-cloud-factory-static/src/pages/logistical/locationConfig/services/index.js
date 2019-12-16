import request from '@/utils/request';
import apis from './api';

export function getList(params) {
  return request(apis.getList.api(params), {
    method: apis.getList.type
  })
}

export function saveLocation(params) {
  return request(apis.saveLocation.api(), {
    method: apis.saveLocation.type,
    body: params
  })
}

export function updateLocation(params) {
  return request(apis.updateLocation.api(), {
    method: apis.updateLocation.type,
    body: params
  })
}

export function deleteLocation(id) {
  return request(apis.deleteLocation.api(id), {
    method: apis.deleteLocation.type
  })
}