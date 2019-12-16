import apis from './api'
import request from '@/utils/request'

export async function login(params) {
  return request(apis.login.api(), {
    method: apis.login.type,
    body: params
  })
}

export async function getSelfResource() {
  return request(apis.getSelfResource.api(), {
    method: apis.getSelfResource.type
  });
}