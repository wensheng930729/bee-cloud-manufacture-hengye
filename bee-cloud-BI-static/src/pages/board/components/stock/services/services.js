import apis from './api'
import request from '@/utils/request'

export async function getStorage(params) {
  return request(apis.getStorage.api(params), {
    method: apis.getStorage.type
  });
}

export async function getOut(params) {
  return request(apis.getOut.api(params), {
    method: apis.getOut.type
  });
}

export async function getOnTheWay(goodsType) {
  return request(apis.getOnTheWay.api(goodsType), {
    method: apis.getOnTheWay.type
  });
}