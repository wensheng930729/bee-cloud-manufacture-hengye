import request from '@/utils/request';
import apis from './api';

export function getList(params) {
  return request(apis.getList.api(params), {
    method: apis.getList.type
  })
}

export function enable(params) {
  return request(apis.enable.api(), {
    method: apis.enable.type,
    body: params
  })
}

export function disable(params) {
  return request(apis.disable.api(), {
    method: apis.disable.type,
    body: params
  })
}