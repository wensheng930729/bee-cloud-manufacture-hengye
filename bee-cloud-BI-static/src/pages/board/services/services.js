import apis from './api'
import request from '@/utils/request'

export async function getBoard() {
  return request(apis.getBoard.api(), {
    method: apis.getBoard.type
  });
}

export async function getConfig(type) {
  return request(apis.getConfig.api(type), {
    method: apis.getConfig.type
  });
}