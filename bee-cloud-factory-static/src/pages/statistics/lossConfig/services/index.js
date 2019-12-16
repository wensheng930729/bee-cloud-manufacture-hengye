import request from '@/utils/request';
import apis from './api';

export function getList(params) {
  return request(apis.getList.api(params), {
    method: apis.getList.type
  })
}

export function getDetail(id) {
  return request(apis.getDetail.api(id), {
    method: apis.getDetail.type
  })
}

export function getProType() {
  return request(apis.getProType.api(), {
    method: apis.getProType.type
  })
}

export function saveLoss(params) {
  return request(apis.saveLoss.api(), {
    method: apis.saveLoss.type,
    body: params
  })
}

export function updateLoss(params) {
  return request(apis.updateLoss.api(), {
    method: apis.updateLoss.type,
    body: params
  })
}

export function deleteLoss(id) {
  return request(apis.deleteLoss.api(id), {
    method: apis.deleteLoss.type
  })
}