import request from '@/utils/request';
import apis from './api';

export function getList(params) {
  return request(apis.getList.api(params), {
    method: apis.getList.type
  })
}

export function deleteRole(id) {
  return request(apis.deleteRole.api(id), {
    method: apis.deleteRole.type,
  })
}

export function getDetail(id) {
  return request(apis.getDetail.api(id), {
    method: apis.getDetail.type,
  })
}

export function save(params) {
  return request(apis.save.api(), {
    method: apis.save.type,
    body: params
  })
}

export function getResources() {
  return request(apis.getResources.api(), {
    method: apis.getResources.type,
  })
}