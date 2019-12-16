import apis from './api'
import request from '@/utils/request'

export async function getPrice(params) {
  return request(apis.getPrice.api(), {
    method: apis.getPrice.type,
    body: params
  })
}

export async function getAmount(params) {
  return request(apis.getAmount.api(), {
    method: apis.getAmount.type,
    body: params
  })
}

export async function getPay(params) {
  return request(apis.getPay.api(), {
    method: apis.getPay.type,
    body: params
  })
}

export async function getRatio(params) {
  return request(apis.getRatio.api(), {
    method: apis.getRatio.type,
    body: params
  })
}

export async function getPosition(type) {
  return request(apis.getPosition.api(type), {
    method: apis.getPosition.type
  })
}

export async function getGoods(params) {
  return request(apis.getGoods.api(), {
    method: apis.getGoods.type,
    body: params
  })
}