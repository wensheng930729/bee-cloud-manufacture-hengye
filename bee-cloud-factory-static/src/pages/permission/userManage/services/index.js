import request from '@/utils/request';
import apis from './api';

export function getRoles() {
  return request(apis.getRoles.api(), {
    method: apis.getRoles.type,
  })
}

export function getList(params) {
  return request(apis.getList.api(params), {
    method: apis.getList.type
  })
}

export function change(id) {
  return request(apis.change.api(id), {
    method: apis.change.type,
  })
}

export function save(params) {
  return request(apis.save.api(), {
    method: apis.save.type,
    body: params
  })
}

export function update(params) {
  return request(apis.update.api(), {
    method: apis.update.type,
    body: params
  })
}