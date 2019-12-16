import request from '@/utils/request';
import apis from './api';

export function getList({ currentPage, pageSize, params }) {
  return request(apis.getList.api({ currentPage, pageSize }), {
    method: apis.getList.type,
    body: params
  })
}

export function getDetail(id) {
  return request(apis.getDetail.api(id), {
    method: apis.getDetail.type,
  })
}

export function saveClient(params) {
  return request(apis.saveClient.api(), {
    method: apis.saveClient.type,
    body: params
  })
}

export function updateClient(params) {
  return request(apis.updateClient.api(), {
    method: apis.updateClient.type,
    body: params
  })
}