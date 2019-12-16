import request from '@/utils/request';
import apis from './api';

export function getList({ currentPage, pageSize, params }) {
  return request(apis.getList.api({ currentPage, pageSize }), {
    method: apis.getList.type,
    body: params
  })
}

export function getTypeList({ currentPage, pageSize, params }) {
  return request(apis.getTypeList.api({ currentPage, pageSize }), {
    method: apis.getTypeList.type,
    body: params
  })
}

export function getDetail(id) {
  return request(apis.getDetail.api(id), {
    method: apis.getDetail.type,
  })
}

export function saveAccount(params) {
  return request(apis.saveAccount.api(), {
    method: apis.saveAccount.type,
    body: params
  })
}

export function updateAccount(params) {
  return request(apis.updateAccount.api(), {
    method: apis.updateAccount.type,
    body: params
  })
}

export function changePassword(params) {
  return request(apis.changePassword.api(), {
    method: apis.changePassword.type,
    body: params
  })
}