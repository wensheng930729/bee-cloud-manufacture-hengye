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

export function saveSupplier(params) {
  return request(apis.saveSupplier.api(), {
    method: apis.saveSupplier.type,
    body: params
  })
}

export function updateSupplier(params) {
  return request(apis.updateSupplier.api(), {
    method: apis.updateSupplier.type,
    body: params
  })
}